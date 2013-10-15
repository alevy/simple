{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Blog.Common where

import Blaze.ByteString.Builder
import Control.Concurrent.MVar
import Control.Exception.Peel
import Control.Monad.IO.Class
import Crypto.Hash
import Data.Byteable
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Search as L
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Network.Wai
import System.Environment
import System.INotify
import Web.Cookie
import Web.Simple
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Blog.Templates

type LBS = L.ByteString

type Session = Map S8.ByteString S8.ByteString

data AppSettings = AppSettings { appDB :: MVar Connection
                               , appTemplate :: IORef (LBS -> LBS)
                               , appSession :: Maybe Session
                               , appSessionKey :: S8.ByteString }

newAppSettings :: INotify -> IO AppSettings
newAppSettings inotify = do
  db <- newMVar =<< createConnection
  tmpl <- newIORef =<< generateTemplate
  let watcher = addWatch inotify [Modify] "templates/main.tmpl.html" $ const $
                  do
                    putStrLn "Reloading main template..."
                    writeIORef tmpl =<< generateTemplate
                    watcher >> return ()
  watcher
  return $ AppSettings { appDB = db
                       , appTemplate = tmpl
                       , appSession = Nothing
                       , appSessionKey = ""}

withSession :: Controller AppSettings a -> Controller AppSettings a
withSession (Controller act) = Controller $ \st0 -> do
  (eres, st@(r, _)) <- act st0
  case eres of
    Left resp0 -> do
      let resp = case appSession r of
                   Just sess ->
                      let sessionKey = appSessionKey r
                      in addCookie
                           ("session", dumpSession sessionKey sess)
                           resp0
                   Nothing -> resp0
      return (Left resp, st)
    Right _ -> return (eres, st)

addCookie :: (S8.ByteString, S8.ByteString) -> Response -> Response
addCookie (key, value) resp =
  let (stat, hdrs, src) = responseSource resp
  in ResponseSource stat (("Set-Cookie", cookie):hdrs) src
  where cookie = toByteString . renderSetCookie $
                  def { setCookieName = key
                      , setCookieValue = value
                      , setCookiePath = Just "/" }

getSessionKey :: IO S8.ByteString
getSessionKey = do
  env <- getEnvironment
  return $ maybe S8.empty S8.pack $ lookup "SESSION_KEY" env

session :: Controller AppSettings Session
session = do
  cs <- controllerState
  case appSession cs of
    Just sess -> return sess
    Nothing -> do
      cookies <- (maybe [] parseCookies) `fmap` requestHeader hCookie
      sess <- case lookup "session" cookies of
                Just session -> do
                  sessionKey <- appSessionKey `fmap` controllerState
                  return $ parseSession sessionKey session
                Nothing -> return M.empty
      putState $ cs {appSession = Just sess}
      return sess

parseSession :: S8.ByteString -> S8.ByteString -> Session
parseSession secret session =
  let (b64, serial) = S8.splitAt 44 session
      mdigest = digestFromByteString $ either (const S8.empty) id $ decode b64
  in case mdigest of
       Nothing -> M.empty
       Just digest ->
         if hmacGetDigest (hmacAlg SHA256 secret serial) == digest then
           M.fromList $ parseSimpleQuery serial
           else M.empty

dumpSession :: S8.ByteString -> Session -> S8.ByteString
dumpSession secret session =
  let serial = renderSimpleQuery False $ M.toList session
      digest = hmacGetDigest $ hmacAlg SHA256 secret serial
      b64 = encode $ toBytes digest
  in b64 `S8.append` serial

sessionLookup :: S8.ByteString -> Controller AppSettings (Maybe S8.ByteString)
sessionLookup key = M.lookup key `fmap` session

sessionInsert :: S8.ByteString -> S8.ByteString -> Controller AppSettings ()
sessionInsert key value = do
  cs <- controllerState
  sess <- session
  putState $ cs { appSession = Just $ M.insert key value sess }

sessionDelete :: S8.ByteString -> Controller AppSettings ()
sessionDelete key = do
  cs <- controllerState
  sess <- session
  putState $ cs { appSession = Just $ M.delete key sess }

sessionClear :: Controller AppSettings ()
sessionClear = do
  cs <- controllerState
  putState $ cs { appSession = Just M.empty }

generateTemplate :: IO (LBS -> LBS)
generateTemplate = do
  tmpl <- L.readFile "templates/main.tmpl.html"
  return $ \content -> L.replace (S8.pack "{% content %}") content tmpl

createConnection :: IO Connection
createConnection = do
  env <- getEnvironment
  let envConnect = maybe S8.empty S8.pack $ lookup "DATABASE_URL" env
  connectPostgreSQL envConnect

withConnection :: (Connection -> Controller AppSettings b) -> Controller AppSettings b
withConnection func = do
  dbvar <- appDB `fmap` controllerState
  bracket (liftIO $ takeMVar dbvar) (liftIO . (putMVar dbvar)) $ \conn -> do
    res <- func conn
    return res

respondTemplate :: Html -> Controller AppSettings a
respondTemplate content = do
  tmplFunc <- liftIO . readIORef =<< fmap appTemplate controllerState
  respond $ okHtml $ tmplFunc $ renderHtml content

respondAdminTemplate :: Html -> Controller AppSettings a
respondAdminTemplate = respondTemplate . adminTemplate

