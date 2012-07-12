{-# LANGUAGE FlexibleInstances #-}

module Network.Wai.Router where

import qualified Data.ByteString as S
import Data.Monoid
import Data.Conduit
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Responses


data Route a = Route (Request -> ResourceT IO (Maybe Response)) a

runRoute :: Route a -> Request -> ResourceT IO (Maybe Response)
runRoute (Route rtr _) req = rtr req

mroute :: (Request -> ResourceT IO (Maybe Response)) -> Route ()
mroute handler = Route handler ()

instance Monad Route where
  return a = Route (const $ return Nothing) a
  (Route rtA valA) >>= fn =
    let (Route rtB valB) = fn valA
    in Route (\req -> do
      resA <- rtA req
      case resA of
        Nothing -> rtB req
        Just _ -> return resA) valB

instance Monoid (Route ()) where
  mempty = mroute $ const $ return Nothing
  mappend (Route a _) (Route b _) = mroute $ \req -> do
    c <- a req
    case c of
      Nothing -> b req
      Just _ -> return c

mkRouter :: Route a -> Application
mkRouter route req = do
  mapp <- runRoute route req
  case mapp of
    Just resp -> return resp
    Nothing -> return $ resp404 req

routeApp :: Application -> Route ()
routeApp app = mroute $ \req -> fmap Just $ app req

routeConst :: Response -> Route ()
routeConst resp = routeApp . const . return $ resp

routePathPrefix :: S.ByteString -> Route a -> Route ()
routePathPrefix path route = mroute $ \req ->
  let patternParts = decodePathSegments path
      lenParts = length patternParts
  in if patternParts == (take lenParts $ pathInfo req) then
    runRoute route $ req { pathInfo = drop lenParts $ pathInfo req}
    else return Nothing

routeHost :: S.ByteString -> Route a -> Route ()
routeHost host route = mroute $ \req ->
  if host == serverName req then runRoute route req
  else return Nothing

routeTop :: Route a -> Route ()
routeTop route = mroute $ \req ->
  if null $ pathInfo req then runRoute route req
  else return Nothing

routeMethod :: Method -> Route a -> Route ()
routeMethod method route = mroute $ \req ->
  if method == requestMethod req then runRoute route req
  else return Nothing

routeName :: Text -> Route a -> Route ()
routeName name route = mroute $ \req ->
  let poppedHdrReq = req { pathInfo = (tail . pathInfo $ req) }
  in if name == (head . pathInfo) req then runRoute route poppedHdrReq
  else return Nothing

