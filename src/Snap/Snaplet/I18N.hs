{-# LANGUAGE OverloadedStrings #-}


module Snap.Snaplet.I18N
  ( I18N
  , HasI18N (..)
  , I18NMessage (..)
  , Locale
  , MessageFile

  , initI18N
  , getI18NMessages
  , lookupI18NValue
  ) where

import           Data.Map.Syntax((##))
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.Configurator       as Config
import qualified Data.Configurator.Types as Config
import           Data.Maybe
import qualified Data.Text               as T
import           Heist
import qualified Heist.Interpreted       as I
import           System.FilePath.Posix
import           Text.XmlHtml            hiding (render)
import qualified Text.XmlHtml            as X

import           Paths_snaplet_i18n
import           Snap
import           Snap.Snaplet.Heist


----------------------------------------------------------------------
--    Types
----------------------------------------------------------------------

type Locale      = String
type MessageFile = String

defaultLocale :: Locale
defaultLocale = "en_US"

-- | ?? could be multiple message files
--
defaultMessageFilePrefix :: MessageFile
defaultMessageFilePrefix = "message"

data I18NConfig  = I18NConfig { _getLocale      :: Locale
                                -- ^ locale, default "en"
                              , _getMessageFile :: MessageFile
                                -- ^ message file name, default to "message"
                              } deriving (Show)

-- | Message content.
--
newtype I18NMessage = I18NMessage Config.Config

-- | data type
--
data I18N = I18N
            { _getI18NConfig  :: I18NConfig
            , _getI18NMessage :: I18NMessage
            }

-- | Compose App with a I18N Snaplet.
--
class HasI18N b where
   i18nLens :: SnapletLens b I18N

-- | Util functions
--
getI18N :: HasI18N b => Handler b b I18N
getI18N = with i18nLens get

-- | Get the @I18NMessage@
--
getI18NMessages :: HasI18N b => Handler b b I18NMessage
getI18NMessages = fmap _getI18NMessage getI18N

-- | Look up a value in, usuallly Handler Monad
--
lookupI18NValue :: HasI18N b => T.Text -> Handler b b T.Text
lookupI18NValue key = do
    (I18NMessage msg) <- getI18NMessages
    liftIO $ Config.lookupDefault "Error: no value found." msg key

----------------------------------------------------------------------
--    Init Snaplet
----------------------------------------------------------------------

-- | Init this I18N snaplet.
--
initI18N :: (HasHeist b, HasI18N b)
                => Maybe Locale              -- ^ Locale, default to @defaultLocale@
                -> SnapletInit b I18N
initI18N l = makeSnaplet "i18n" description datadir $ do
    let i18nConfig = I18NConfig (fromMaybe defaultLocale l) defaultMessageFilePrefix
    fp <- getSnapletFilePath
    msg <- liftIO $ readMessageFile fp i18nConfig
    modifyHeistState addDefaultSplices
    return $ I18N i18nConfig msg
  where addDefaultSplices = I.bindSplices splices
        splices = do
                  "i18n" ## i18nSplice
                  "i18nSpan" ## i18nSpanSplice
        -- config dir for current snaplet
        datadir = Just $ fmap (++ "/resources") getDataDir
        description = "light weight i18n snaplet"


-- | Load file
--   server will not be able to start up if dir doesnt exists.
--   Thus, no additional validation check so far.
--
readMessageFile :: FilePath -> I18NConfig -> IO I18NMessage
readMessageFile base config = do
    let fullname = base </> file config
    fmap I18NMessage (Config.load [Config.Required fullname])
  where
    -- file fullname will be like message-en_US.cfg
    -- FIXME: Maybe replace "-" with "_" in locale in case typo
    file c = _getMessageFile c ++ "-" ++ _getLocale c ++ ".cfg"


----------------------------------------------------------------------
--    Splices
----------------------------------------------------------------------

-- | element attribute used for looking up i18n value.
--   e.g. <i18n name="hello" />
--
i18nSpliceAttr :: T.Text
i18nSpliceAttr = "name"


-- | Splices just wrap value fonud at l10n message.
--   When it is used for wrap around other elements, a.k.a children is not empty,
--   binding `i18nValue`.
--   e.g.
--       <i18n name="hello" />
--       <i18n name="hello"><p><i18nValue/></p></i18n>
--
-- FIXME: Turns out that it is not possible to fail at compilation if value is Nothing but runtime.
i18nSplice :: HasI18N b => I.Splice (Handler b b)
i18nSplice = do
    input <- getParamNode
    value <- lift . lookupI18NValue $ getNameAttr input
    case childElements input of
      [] -> return [X.TextNode value]
      _  -> I.runChildrenWithText ("i18nValue" ## value)

-- | Splices. use 'span' html element wrap result.
--
i18nSpanSplice :: HasI18N b => I.Splice (Handler b b)
i18nSpanSplice = do
    input <- getParamNode
    v <- lift . lookupI18NValue $ getNameAttr input
    return [X.Element "span" (elementAttrs input) [X.TextNode v]]

-- | Look up 'name' attribute value.
--
getNameAttr :: Node -> T.Text
getNameAttr n = case getAttribute i18nSpliceAttr n of
                  Just x -> x
                  _      -> ""

----------------------------------------------------------------------
