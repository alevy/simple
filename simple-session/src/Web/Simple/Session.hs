{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Adds cookie-based session management to simple 'Controller's. To add to an
  application, declare the Controller setting\'s type an instance of
  'HasSession', and wrap routes with 'withSession'. For example:

  > data AppSettings = ...
  >
  > instance HasSession AppSettings where
  >   ...

  > controllerApp settings $ withSessions $ do
  >   routeName \"posts\" $ ...

 -}
module Web.Simple.Session
  ( Session
  -- * Class and Middleware
  , HasSession(..), withSession
  -- * Accessors
  , sessionLookup, sessionInsert, sessionDelete, sessionClear

  -- * Utilities
  , session, parseSession, dumpSession, addCookie
  ) where

import Control.Monad
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
import Network.Wai.Internal (Response(..))
import Web.Cookie
import Web.Simple.Controller
import System.Environment

-- | Plaintext mapping of the session map. Both keys and values are
-- 'S.ByteString's.
type Session = Map S.ByteString S.ByteString

-- | Instances of this class can be used as states by a 'Controller' states
-- to manage cookie-based user sessions. Instances must minimally implement
-- 'getSession' and 'setSession'. By default, the secret session key is taken
-- from the environment variable \"SESSION_KEY\", or a default dummy key is
-- used if the environment variable \"ENV\" is set to \"development\". You can
-- override this behaviour by implementing the 'sessionKey' method.
-- If the controller state contains a dedicated field of type 'Maybe Session',
-- a reasonable implementation would be:
--
-- > data MyAppSettings = MyAppSettings { myAppSess :: Maybe Session, ...}
-- >
-- > instance HasSession MyAppSettings where
-- >    getSession = myAppSess <$> controllerState
-- >    setSession sess = do
-- >      cs <- controllerState
-- >      putState $ cs { myAppSess = sess }
--
class HasSession hs where
  -- | Returns the secret session key. The default implementation uses the
  -- \"SESSION_KEY\" environment variable. If it is not present, and the
  -- \"ENV\" environment variable is set to \"development\", a dummy, hardcoded
  -- key is used.
  sessionKey :: Controller hs S.ByteString
  sessionKey = liftIO $ do
    env <- getEnvironment
    case lookup "SESSION_KEY" env of
      Just key -> return $ S8.pack key
      Nothing ->
        case lookup "ENV" env of
          Just e | e == "development" -> return "test-session-key"
          _ -> (error "SESSION_KEY environment variable not set")

  -- | Returns the cached session for the current request, or nothing if the
  -- session has not been set yet for this request. 
  getSession :: hs -> Maybe Session

  -- | Stores a parsed or changed session for the remainder of the request.This
  -- is used both for cached a parsed session cookie as well as for serializing
  -- to the \"Set-Cookie\" header when responding.
  setSession :: Session -> Controller hs ()

-- | A trivial implementation if the 'Controller' settings is just a Session
-- store.
instance HasSession (Maybe Session) where
  getSession = id
  setSession = putState . Just

-- | A middleware wrapper around a 'Controller' that sets the \"Set-Cookie\"
-- header in the HTTP response if the Session is present, i.e. if it was
-- accessed/modified by the 'Controller'.
withSession :: HasSession hs
            => Controller hs a -> Controller hs a
withSession (ControllerT act) = do
  sk <- sessionKey
  ControllerT $ \st0 req -> do
    (eres, st) <- act st0 req
    case eres of
      Left resp0 -> do
        let resp = case getSession st of
                     Just sess -> addCookie
                                   ("session", dumpSession sk sess)
                                   resp0
                     Nothing -> resp0
        return (Left resp, st)
      Right _ -> return (eres, st)

-- | Adds a \"Set-Cookie\" with the given key-value tuple to the 'Response'.
-- The path set on the cookie is \"/\", meaning it applies to all routes on the
-- domain, and no expiration is set.
addCookie :: (S.ByteString, S.ByteString) -> Response -> Response
addCookie (key, value) (ResponseSource stat hdrs src) =
  ResponseSource stat (("Set-Cookie", cookie key value):hdrs) src
addCookie (key, value) (ResponseFile stat hdrs fl mfp) =
  ResponseFile stat (("Set-Cookie", cookie key value):hdrs) fl mfp
addCookie (key, value) (ResponseBuilder stat hdrs bldr) =
  ResponseBuilder stat (("Set-Cookie", cookie key value):hdrs) bldr
addCookie _ resp = resp -- Can't do anything for ResponseRaw

cookie :: S.ByteString -> S.ByteString -> S.ByteString
cookie key value = toByteString . renderSetCookie $
    def { setCookieName = key
        , setCookieValue = value
        , setCookiePath = Just "/" }

-- | Returns the current 'Session', either from the 'getSession' cache or by
-- parsing the cookie from the 'Request' using 'sessionFromCookie'.
session :: HasSession hs => Controller hs Session
session = do
  cs <- controllerState
  case getSession cs of
    Just sess -> return sess
    Nothing -> do
      sess <- sessionFromCookie
      setSession sess
      return sess

-- | Get and parse a 'Session' from the current 'Request'.
sessionFromCookie :: HasSession hs => Controller hs Session
sessionFromCookie = do
  cookies <- (maybe [] parseCookies) `liftM` requestHeader hCookie
  sess <- case lookup "session" cookies of
            Just sessionCookie -> do
              sk <- sessionKey
              return $ parseSession sk sessionCookie
            Nothing -> return M.empty
  return sess

-- | Parses and validates a serialized 'Session' given the secret. If the
-- 'Session' is invalid (i.e. the hmac does not match), an empty 'Session' is
-- returned.
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

-- | Serializes a 'Session' by applying a sha256 hmac with the given secret
-- key to the serialized 'Session' (using 'renderSimpleQuery'), base64 encoding
-- the result, and prepending it to the serialized 'Session'.
dumpSession :: S.ByteString -> Session -> S.ByteString
dumpSession secret sess =
  let serial = renderSimpleQuery False $ M.toList sess
      digest = hmacGetDigest $ hmacAlg SHA256 secret serial
      b64 = encode $ toBytes digest
  in b64 `S.append` serial

-- | Lookup a key from the current 'Request's session.
sessionLookup :: HasSession hs
              => S.ByteString -> Controller hs (Maybe S.ByteString)
sessionLookup key = M.lookup key `liftM` session

-- | Insert or replace a key in the current 'Request's session.
sessionInsert :: HasSession hs
              => S.ByteString -> S.ByteString -> Controller  hs ()
sessionInsert key value = do
  sess <- session
  setSession (M.insert key value sess)

-- | Remove a key from the current 'Request's session.
sessionDelete :: HasSession hs
              => S.ByteString -> Controller hs ()
sessionDelete key = do
  sess <- session
  setSession $ M.delete key sess

-- | Clear the entire 'Session'.
sessionClear :: HasSession hs => Controller hs ()
sessionClear = setSession M.empty

