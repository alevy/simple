module Network.Wai.Frank where

import Network.HTTP.Types
import Network.Wai.Router
import qualified Data.ByteString as S

frankMethod :: Routeable r => StdMethod -> S.ByteString -> r -> Route ()
frankMethod method path = routeMethod method . routePathPrefix path

get :: Routeable r => S.ByteString -> r -> Route ()
get = frankMethod GET

post :: Routeable r => S.ByteString -> r -> Route ()
post = frankMethod POST

head :: Routeable r => S.ByteString -> r -> Route ()
head = frankMethod HEAD

put :: Routeable r => S.ByteString -> r -> Route ()
put = frankMethod PUT

delete :: Routeable r => S.ByteString -> r -> Route ()
delete = frankMethod DELETE

options :: Routeable r => S.ByteString -> r -> Route ()
options = frankMethod OPTIONS

