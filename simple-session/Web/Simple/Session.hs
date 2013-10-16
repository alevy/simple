{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Web.Simple.Session where

import Control.Monad.IO.Class
import Blaze.ByteString.Builder
import Crypto.Hash
import Data.Byteable
import Data.ByteString.Base64
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Map (Map)
import qualified Data.Map as M
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Web.Cookie
import Web.Simple
import System.Environment

type Session = Map S.ByteString S.ByteString

class HasSession hs where
  sessionKey :: Controller hs S.ByteString
  sessionKey = liftIO $ S8.pack `fmap` getEnv "SESSION_KEY"

  getSession :: hs -> Maybe Session
  setSession :: Session -> Controller hs ()

instance HasSession (Maybe Session) where
  getSession = id
  setSession = putState . Just

withSession :: HasSession hs => Controller hs a -> Controller hs a
withSession (Controller act) = do
  sk <- sessionKey
  Controller $ \st0 -> do
    (eres, st@(r, _)) <- act st0
    case eres of
      Left resp0 -> do
        let resp = case getSession r of
                     Just sess -> addCookie
                                   ("session", dumpSession sk sess)
                                   resp0
                     Nothing -> resp0
        return (Left resp, st)
      Right _ -> return (eres, st)

addCookie :: (S.ByteString, S.ByteString) -> Response -> Response
addCookie (key, value) resp =
  let (stat, hdrs, src) = responseSource resp
  in ResponseSource stat (("Set-Cookie", cookie):hdrs) src
  where cookie = toByteString . renderSetCookie $
                  def { setCookieName = key
                      , setCookieValue = value
                      , setCookiePath = Just "/" }

session :: HasSession hs => Controller hs Session
session = do
  msession <- getSession `fmap` controllerState
  case msession of
    Just sess -> return sess
    Nothing -> do
      sess <- sessionFromCookie
      setSession =<< sessionFromCookie
      return sess

sessionFromCookie :: HasSession hs => Controller hs Session
sessionFromCookie = do
  cookies <- (maybe [] parseCookies) `fmap` requestHeader hCookie
  sess <- case lookup "session" cookies of
            Just sessionCookie -> do
              sk <- sessionKey
              return $ parseSession sk sessionCookie
            Nothing -> return M.empty
  return sess

parseSession :: S.ByteString -> S.ByteString -> Session
parseSession secret sessionCookie =
  let (b64, serial) = S.splitAt 44 sessionCookie
      mdigest = digestFromByteString $ either (const S.empty) id $ decode b64
  in case mdigest of
       Nothing -> M.empty
       Just digest ->
         if hmacGetDigest (hmacAlg SHA256 secret serial) == digest then
           M.fromList $ parseSimpleQuery serial
           else M.empty

dumpSession :: S.ByteString -> Session -> S.ByteString
dumpSession secret sess =
  let serial = renderSimpleQuery False $ M.toList sess
      digest = hmacGetDigest $ hmacAlg SHA256 secret serial
      b64 = encode $ toBytes digest
  in b64 `S.append` serial

sessionLookup :: HasSession hs
              => S.ByteString -> Controller hs (Maybe S.ByteString)
sessionLookup key = M.lookup key `fmap` session

sessionInsert :: HasSession hs
              => S.ByteString -> S.ByteString -> Controller hs ()
sessionInsert key value = do
  sess <- session
  setSession (M.insert key value sess)

sessionDelete :: HasSession hs => S.ByteString -> Controller hs ()
sessionDelete key = do
  sess <- session
  setSession $ M.delete key sess

sessionClear :: HasSession hs => Controller hs ()
sessionClear = setSession M.empty

