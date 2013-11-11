{-# LANGUAGE OverloadedStrings #-}
module Blog.Helpers where

import Data.Aeson
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Time.LocalTime
import Data.Time.Format
import Network.Gravatar
import Text.Pandoc (writeHtmlString, readMarkdown)
import System.Locale
import Web.Simple.Templates

helperFunctions :: FunctionMap
helperFunctions = fromList
  [ ("formatTime", toFunction timeFormatter)
  , ("gravatar", toFunction gravatarUrl)
  , ("length", toFunction valueArrayLength)
  , ("markdown", toFunction markdown)]

valueArrayLength :: [Value] -> Value
valueArrayLength = toJSON . length

gravatarUrl :: Text -> Maybe Int -> Value
gravatarUrl email size = toJSON $
  gravatar (def {gSize = fmap Size size, gDefault = Just Wavatar}) email

timeFormatter :: ZonedTime -> Maybe String -> Value
timeFormatter t mfmt =
  let fmt = fromMaybe "%B %e, %C%y %l:%M%P" mfmt
  in toJSON $ formatTime defaultTimeLocale fmt t

markdown :: Text -> Value
markdown = toJSON . (writeHtmlString def) . (readMarkdown def)
               . (filter (/= '\r')) . unpack
