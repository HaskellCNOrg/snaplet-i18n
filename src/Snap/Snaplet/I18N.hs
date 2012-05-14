{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Snap.Snaplet.I18N where

import           Control.Monad
import           Data.Lens.Common
import           Data.Maybe
import           System.Directory
import           System.FilePath.Posix
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
import qualified Data.Configurator as Config
import qualified Data.Configurator.Types as Config
import qualified Data.Text as T
import qualified Text.XmlHtml as X

import Snap
import Snap.Snaplet.Heist
-------------------------------------------------------
-- 
--
-- 
-- 
-------------------------------------------------------

type Locale      = String
type MessageFile = String

data I18NConfig  = I18NConfig { _getLocale      :: Locale       -- ^ locale, default "en"
                              , _getMessageFile :: MessageFile  -- ^ message file name, default to "message"
                              } deriving (Show)

-- | A simple mapping to hold i18n messages
-- 
data I18NMessage = I18NMessage Config.Config

-- | data type
-- 
data I18NSnaplet = I18NSnaplet
                    { _getI18NConfig  :: I18NConfig
                    , _getI18NMessage :: I18NMessage
                    } 


-- | Compose App with a I18N Snaplet.
-- 
class HasI18N b where
  i18nLens :: Lens b (Snaplet I18NSnaplet)
  
-- | Util functions
-- 
i18nLens' :: HasI18N b => Lens (Snaplet b) (Snaplet I18NSnaplet)
i18nLens' = subSnaplet i18nLens

getI18NSnaplet :: HasI18N b => Handler b b I18NSnaplet
getI18NSnaplet = with i18nLens Snap.get

getI18NMessages :: HasI18N b => Handler b b I18NMessage
getI18NMessages = liftM _getI18NMessage getI18NSnaplet

-------------------------------------------------------

-- | Default I18N snaplet
-- 
defaultI18NSnaplet :: (HasHeist b, HasI18N b) => SnapletInit b I18NSnaplet
defaultI18NSnaplet = initI18NSnaplet Nothing Nothing

-- | Init this I18NSnaplet snaplet.
-- 
initI18NSnaplet :: (HasHeist b, HasI18N b) => Maybe Locale -> Maybe MessageFile -> SnapletInit b I18NSnaplet
initI18NSnaplet l m = makeSnaplet "I18NSnaplet" "" Nothing $ do
    --mainConfig <- getSnapletUserConfig
    --liftIO $ print $ show mainConfig
    i18nConfig <- return $ I18NConfig (fromMaybe "en" l) (fromMaybe "data/message" m)
    config <- liftIO $ readMessageFile i18nConfig
    defaultSplices
    return $ I18NSnaplet i18nConfig $ I18NMessage config
  where defaultSplices = addSplices [(i18nSpliceName, liftHeist i18nSplice)]

-------------------------------------------------------
-- 

-- | Load file
--   server will not be able to start up if dir doesnt exists.
--   Thus, no additional validation check so far.
-- 
readMessageFile :: I18NConfig -> IO Config.Config
readMessageFile config = do
    base     <- getCurrentDirectory
    fullname <- return $ base </> (file config)
    Config.load [Config.Required fullname]
  where file c = _getMessageFile c ++ "-" ++ _getLocale c ++ ".cfg"


-------------------------------------------------------

i18nSpliceName :: T.Text
i18nSpliceName = "i18n"

i18nSpliceElement :: T.Text
i18nSpliceElement = "span"

i18nSpliceAttr :: T.Text
i18nSpliceAttr = "name"

-- | Splices
-- 
i18nSplice :: HasI18N b => Splice (Handler b b)
i18nSplice = do
    input <- getParamNode
    (I18NMessage messages) <- lift getI18NMessages
    value <- liftIO $ getValue messages input
    -- FIXME: Turns out that it is not possible to fail at compilation if value is Nothing but runtime.
    return [X.Element i18nSpliceElement [] [X.TextNode $ T.pack value]]
    where getValue :: Config.Config -> Node -> IO String
          getValue m i = Config.lookupDefault "Error: no value found." m (getAttr' i)
          getAttr' i = case getAttribute i18nSpliceAttr i of
            Just x -> x
            _      -> ""
