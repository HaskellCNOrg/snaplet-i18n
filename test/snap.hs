{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}


module Main where

------------------------------------------------------------------------------
import           Control.Category
import           Control.Monad
import           Control.Exception (SomeException, try)
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Network.HTTP.Conduit (responseBody)
import           Network.HTTP.Types (renderSimpleQuery)
import           Prelude hiding ((.))
import           Snap hiding (Config)
import qualified Data.Configurator as CF
import qualified Data.Configurator.Types as CF
import           Snap.Core
import           Snap.Http.Server
import           Snap.Snaplet.Heist
import           Snap.Snaplet.OAuth
import           Snap.Util.FileServe
import           System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.XmlHtml as X
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)

#ifdef DEVELOPMENT
import           Snap.Loader.Devel
#else
import           Snap.Loader.Prod
#endif

import Snap.Snaplet.I18N

------------------------------------------------------------------------------

data App = App
    { _heist   :: Snaplet (Heist App)
    , _i18n   :: Snaplet I18NSnaplet
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasI18N App where
   i18nLens = i18n

type AppHandler = Handler App App

------------------------------------------------------------------------------

decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------


testHandler :: Handler App App ()
testHandler = do
    (I18NMessage messages) <- getI18NMessages
    msg <- liftIO $ (CF.lookup messages "hello" :: IO (Maybe String) )
    liftIO $ print msg
    writeBS "test"

index :: AppHandler ()
index = do
     render "index"

-- | wrap to element span
--       

------------------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes  = [ ("/", index)
          , ("/test", testHandler)
          , ("", with heist heistServe)
          ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    i <- nestSnaplet "i18n" i18n $ initI18NSnaplet (Just "zh_CN") Nothing
    addRoutes routes
    return $ App h i


------------------------------------------------------------------------------

main :: IO ()
main = do
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["heist/templates"])

    _ <- try $ httpServe conf $ site :: IO (Either SomeException ())
    cleanup

getConf :: IO (Config Snap ())
getConf = commandLineConfig defaultConfig

getActions :: Config Snap () -> IO (Snap (), IO ())
getActions _ = do
    (msgs, site, cleanup) <- runSnaplet app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)

