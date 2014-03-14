{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
module Web.Simple.Templates
  ( HasTemplates(..)
  , defaultGetTemplate, defaultRender, defaultFunctionMap
  , H.fromList
  , Function(..), ToFunction(..), FunctionMap
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector as V
import Network.Mime
import System.FilePath
import Web.Simple.Controller.Trans (ControllerT, respond)
import Web.Simple.Responses (ok)
import Web.Simple.Templates.Language

class Monad m => HasTemplates m hs where
  -- | The layout to use by default. Layouts are just templates that embed
  -- views. They are rendered with the a global object containing the rendered
  -- view in the \"yield\" field, and the object the view was rendered with in
  -- the \"page\" field. By default, no template is used.
  defaultLayout :: ControllerT hs m (Maybe Template)
  defaultLayout = return Nothing

  -- | The directory to look for views passed to 'render'. This defaults to
  -- \"views\", so
  --
  -- @
  -- render \"index.html.tmpl\" ...
  -- @
  --
  -- will look for a view template in \"views/index.html.tmpl\".
  viewDirectory :: ControllerT hs m FilePath
  viewDirectory = return "views"

  -- | A map of pure functions that can be called from within a template. See
  -- 'FunctionMap' and 'Function' for details.
  functionMap :: ControllerT hs m FunctionMap
  functionMap = return defaultFunctionMap

  -- | Function to use to get a template. By default, it looks in the
  -- 'viewDirectory' for the given file name and compiles the file into a
  -- template. This can be overriden to, for example, cache compiled templates
  -- in memory.
  getTemplate :: FilePath -> ControllerT hs m Template
  default getTemplate :: MonadIO m => FilePath -> ControllerT hs m Template
  getTemplate = defaultGetTemplate

  -- | Renders a view template with the default layout and a global used to
  -- evaluate variables in the template.
  render :: ToJSON a => FilePath -> a -> ControllerT hs m ()
  render = defaultRender

  -- | Same as 'render' but without a template.
  renderPlain :: ToJSON a => FilePath -> a -> ControllerT hs m ()
  renderPlain fp val = do
    fm <- functionMap
    dir <- viewDirectory
    tmpl <- getTemplate (dir </> fp)
    let pageContent =
          L.fromChunks . (:[]) . encodeUtf8 $
            renderTemplate tmpl fm $ toJSON val
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    respond $ ok mime pageContent

  -- | Render a view using the layout named by the first argument.
  renderLayout :: ToJSON a => FilePath -> FilePath -> a -> ControllerT hs m ()
  renderLayout lfp fp val = do
    layout <- getTemplate lfp
    renderLayout' layout fp val

  -- | Same as 'renderLayout' but uses an already compiled layout.
  renderLayout' :: ToJSON a => Template -> FilePath -> a -> ControllerT hs m ()
  renderLayout' layout fp val = do
    fm <- functionMap
    dir <- viewDirectory
    tmpl <- getTemplate (dir </> fp)
    let pageContent = renderTemplate tmpl fm $ toJSON val
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    respond $ ok mime $ L.fromChunks . (:[]) . encodeUtf8 $
      renderTemplate layout fm $ object ["yield" .= pageContent, "page" .= val]

defaultGetTemplate :: (HasTemplates m hs, MonadIO m)
                   => FilePath -> ControllerT hs m Template
defaultGetTemplate fp = do
  contents <- liftIO $ S.readFile fp
  case compileTemplate . decodeUtf8 $ contents of
    Left str -> fail str
    Right tmpl -> return tmpl

defaultRender :: (HasTemplates m hs , Monad m, ToJSON a)
              => FilePath -> a -> ControllerT hs m ()
defaultRender fp val = do
  mlayout <- defaultLayout
  case mlayout of
    Nothing -> renderPlain fp val
    Just layout -> renderLayout' layout fp val

defaultFunctionMap :: FunctionMap
defaultFunctionMap = H.fromList
  [ ("length", toFunction valueLength)
  , ("null", toFunction valueNull)]

valueLength :: Value -> Value
valueLength (Array arr) = toJSON $ V.length arr
valueLength (Object obj) = toJSON $ H.size obj
valueLength (String str) = toJSON $ T.length str
valueLength Null = toJSON (0 :: Int)
valueLength _ = error "length only valid for arrays, objects and strings"

valueNull :: Value -> Value
valueNull (Array arr) = toJSON $ V.null arr
valueNull (Object obj) = toJSON $ H.null obj
valueNull (String str) = toJSON $ T.null str
valueNull Null = toJSON True
valueNull _ = error "null only valid for arrays, objects and strings"

