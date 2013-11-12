{-# LANGUAGE OverloadedStrings #-}
module Web.Simple.Templates
  ( HasTemplates(..)
  , defaultGetTemplate, defaultRender
  , H.fromList
  , Function(..), ToFunction(..), FunctionMap
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Aeson
import Network.Mime
import System.FilePath
import Web.Simple (Controller, ok, respond)
import Web.Simple.Templates.Language
import Web.Simple.Templates.Types

class HasTemplates hs where
  -- | The layout to use by default. Layouts are just templates that embed
  -- views. They are rendered with the a global object containing the rendered
  -- view in the \"yield\" field, and the object the view was rendered with in
  -- the \"page\" field. By default, no template is used.
  defaultLayout :: Controller hs (Maybe Template)
  defaultLayout = return Nothing

  -- | The directory to look for views passed to 'render'. This defaults to
  -- \"views\", so
  --
  -- @
  -- render \"index.html.tmpl\" ...
  -- @
  --
  -- will look for a view template in \"views/index.html.tmpl\".
  viewDirectory :: Controller hs FilePath
  viewDirectory = return "views"

  -- | A map of pure functions that can be called from within a template. See
  -- 'FunctionMap' and 'Function' for details.
  functionMap :: Controller hs FunctionMap
  functionMap = return H.empty

  -- | Function to use to get a template. By default, it looks in the
  -- 'viewDirectory' for the given file name and compiles the file into a
  -- template. This can be overriden to, for example, cache compiled templates
  -- in memory.
  getTemplate :: FilePath -> Controller hs Template
  getTemplate = defaultGetTemplate

  -- | Renders a view template with the default layout and a global used to
  -- evaluate variables in the template.
  render :: ToJSON a => FilePath -> a -> Controller hs ()
  render = defaultRender

  -- | Same as 'render' but without a template.
  renderPlain :: ToJSON a => FilePath -> a -> Controller hs ()
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
  renderLayout :: ToJSON a => FilePath -> FilePath -> a -> Controller hs ()
  renderLayout lfp fp val = do
    layout <- getTemplate lfp
    renderLayout' layout fp val

  -- | Same as 'renderLayout' but uses an already compiled layout.
  renderLayout' :: ToJSON a => Template -> FilePath -> a -> Controller hs ()
  renderLayout' layout fp val = do
    fm <- functionMap
    dir <- viewDirectory
    tmpl <- getTemplate (dir </> fp)
    let pageContent =
          L.fromChunks . (:[]) . encodeUtf8 $
            renderTemplate tmpl fm $ toJSON val
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    respond $ ok mime $ L.fromChunks . (:[]) . encodeUtf8 $
      renderTemplate layout fm $ object ["yield" .= pageContent, "page" .= val]

defaultGetTemplate :: HasTemplates hs => FilePath -> Controller hs Template
defaultGetTemplate fp = do
  eres <- compileTemplate . decodeUtf8 <$>
    liftIO (S.readFile fp)
  case eres of
    Left str -> fail str
    Right tmpl -> return tmpl

defaultRender :: (HasTemplates hs , ToJSON a)
              => FilePath -> a -> Controller hs ()
defaultRender fp val = do
  mlayout <- defaultLayout
  case mlayout of
    Nothing -> renderPlain fp val
    Just layout -> renderLayout' layout fp val

