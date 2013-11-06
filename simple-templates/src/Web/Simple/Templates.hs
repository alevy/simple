{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, CPP #-}
module Web.Simple.Templates
  ( HasTemplates(..)
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

class HasTemplates hs where
  defaultLayout :: Controller hs (Maybe Template)
  defaultLayout = return Nothing

  functionMap :: Controller hs FunctionMap
  functionMap = return H.empty

  getTemplate :: FilePath -> Controller hs Template
{-  getTemplate fp = do
    eres <- compileTemplate . decodeUtf8 <$> liftIO (S.readFile fp)
    case eres of
      Left str -> fail str
      Right tmpl -> return tmpl-}

  render :: ToJSON a => FilePath -> a -> Controller hs ()
{-  render fp val = do
    mlayout <- defaultLayout
    case mlayout of
      Nothing -> renderPlain fp val
      Just layout -> renderLayout' layout fp val-}

  renderPlain :: ToJSON a => FilePath -> a -> Controller hs ()
{-  renderPlain fp val = do
    fm <- functionMap
    tmpl <- getTemplate fp
    let pageContent =
          L.fromChunks . (:[]) . encodeUtf8 $ unTemplate tmpl fm $ toJSON val
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    respond $ ok mime pageContent-}

  renderLayout :: ToJSON a => FilePath -> FilePath -> a -> Controller hs ()
{-  renderLayout lfp fp val = do
    layout <- getTemplate lfp
    renderLayout' layout fp val-}

  renderLayout' :: ToJSON a => Template -> FilePath -> a -> Controller hs ()
{-  renderLayout' layout fp val = do
    fm <- functionMap
    tmpl <- getTemplate fp
    let pageContent =
          L.fromChunks . (:[]) . encodeUtf8 $ unTemplate tmpl fm $ toJSON val
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    respond $ ok mime $ L.fromChunks . (:[]) . encodeUtf8 $
      unTemplate layout fm $ object ["yield" .= pageContent, "page" .= val]-}

