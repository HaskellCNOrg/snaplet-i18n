{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where

------------------------------------------------------------------------------
import           Control.Exception      (SomeException, try)
import           Control.Lens           hiding (index)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.Maybe
import qualified Data.Text              as T
import qualified Heist.Interpreted      as I
import           Prelude                hiding ((.))
import           Snap
import           Snap.Snaplet.Config
import           Snap.Snaplet.Heist
import           System.IO

import           Snap.Snaplet.I18N

import           Snap.Loader.Static


------------------------------------------------------------------------------

data App = App
    { _heist :: Snaplet (Heist App)
    , _i18n  :: Snaplet I18N
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasI18N App where
   i18nLens = i18n

type AppHandler = Handler App App

------------------------------------------------------------------------------

decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------


testSplice :: I.Splice AppHandler
testSplice = do
    locale <- liftIO getDefaultLocale
    liftIO $ print locale
    I.textSplice $ T.pack "test"

index :: AppHandler ()
index = heistLocal (I.bindSplice "testSplice" testSplice) $ render "index"

-- | wrap to element span
--

------------------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes  = [ ("/", index)
          , ("", with heist heistServe)
          ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    locale <- liftIO getDefaultLocale
    i <- nestSnaplet "i18n" i18n $ initI18N locale
    addRoutes routes
    return $ App h i

getDefaultLocale :: IO (Maybe Locale)
--getDefaultLocale = return $ Just "zh_CN"
getDefaultLocale = fmap getLocale getConf

------------------------------------------------------------------------------

main :: IO ()
main = do
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["heist/templates"])

    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    cleanup

getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig defaultConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet
        (appEnvironment =<< getOther conf) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
