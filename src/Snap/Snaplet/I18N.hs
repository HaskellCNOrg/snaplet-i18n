{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Snap.Snaplet.I18N 
  ( I18NSnaplet
  , HasI18N (..)
  , initI18NSnaplet
  , getI18NMessages
  , I18NMessage (..)
  , lookupI18NValue
  ) where

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

defaultLocale :: Locale
defaultLocale = "en_US"

-- | ?? could be multiple message files
-- 
defaultMessageFilePrefix :: MessageFile
defaultMessageFilePrefix = "data/message"

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
getI18NSnaplet :: HasI18N b => Handler b b I18NSnaplet
getI18NSnaplet = with i18nLens Snap.get

getI18NMessages :: HasI18N b => Handler b b I18NMessage
getI18NMessages = liftM _getI18NMessage getI18NSnaplet

-------------------------------------------------------

-- | Init this I18NSnaplet snaplet.
-- 
initI18NSnaplet :: (HasHeist b, HasI18N b)
                => Maybe Locale              -- ^ Locale, default to @defaultLocale@
                -> Maybe MessageFile         -- ^ Message file prefix, default to @defaultMessageFilePrefix@
                -> SnapletInit b I18NSnaplet
initI18NSnaplet l m = makeSnaplet "I18NSnaplet" "" Nothing $ do
    i18nConfig <- return $ I18NConfig (fromMaybe defaultLocale l) (fromMaybe defaultMessageFilePrefix m)
    config <- liftIO $ readMessageFile i18nConfig
    defaultSplices
    return $ I18NSnaplet i18nConfig $ I18NMessage config
  where defaultSplices = addSplices [ ("i18n", liftHeist i18nSplice)
                                    , ("i18nSpan", liftHeist i18nSpanSplice)]

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
  where 
    -- file fullname will be like message-en_US.cfg
    -- FIXME: Maybe replace "-" with "_" in locale for typo
    file c = _getMessageFile c ++ "-" ++ _getLocale c ++ ".cfg"


-------------------------------------------------------

-- | Splices just wrap value fonud at l10n message.
--
-- FIXME: Turns out that it is not possible to fail at compilation if value is Nothing but runtime. 
i18nSplice :: HasI18N b => Splice (Handler b b)
i18nSplice = do
    input <- getParamNode
    (I18NMessage messages) <- lift getI18NMessages
    value <- liftIO $ lookupI18NValue' messages input
    return [X.TextNode value]

-- | Splices. use 'span' html element wrap result.
-- 
i18nSpanSplice :: HasI18N b => Splice (Handler b b)
i18nSpanSplice = do
    input <- getParamNode
    (I18NMessage messages) <- lift getI18NMessages
    value <- liftIO $ lookupI18NValue' messages input
    return [X.Element "span" (elementAttrs input) [X.TextNode value]]

-- | element attribute used for looking up i18n value.
--   e.g. <i18n name="hello" />
-- 
i18nSpliceAttr :: T.Text
i18nSpliceAttr = "name"

-- | Look up messages and give a default value if not found.
-- 
lookupI18NValue' :: Config.Config -> Node -> IO T.Text
lookupI18NValue' m input = Config.lookupDefault "Error: no value found." m (getAttr' input)
                      where getAttr' i = case getAttribute i18nSpliceAttr i of
                                          Just x -> x
                                          _      -> "" 
lookupI18NValue :: Config.Config -> T.Text -> IO T.Text
lookupI18NValue = Config.lookupDefault "Error: no value found."
