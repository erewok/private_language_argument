{-# LANGUAGE OverloadedStrings #-}

module PLA.Post where

import Control.Monad (fail)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Types as HT
import           Network.HTTP.Simple
import Prelude (String)
import Protolude
import Web.Authenticate.OAuth (signOAuth)

import PLA.Config
import PLA.Text
import PLA.Types


twitterHost :: ByteString
twitterHost = "api.twitter.com"

statusUpdateEndpoint :: ByteString
statusUpdateEndpoint = "/1.1/statuses/update.json"

piTextToStatus :: TwitToken -> [Text] -> Maybe StatusId -> IO ()
piTextToStatus _ [] _ = pure ()
piTextToStatus token [txt] statsId = postSectionToTwitter token (makeStatusUpdate txt statsId) *> pure ()
piTextToStatus token (txt:txts) statsId = do
  newStatus <- postSectionToTwitter token (makeStatusUpdate txt statsId)
  case newStatus of
    Nothing -> pure ()
    Just status -> piTextToStatus token txts $ statusId status

postSectionToTwitter :: TwitToken -> Status -> IO (Maybe Status)
postSectionToTwitter token status = do
  print status
  let request = setRequestQueryString (statusToQuery status)
                $ setRequestMethod "POST"
                $ setRequestPath statusUpdateEndpoint
                $ setRequestHost twitterHost
                $ setRequestPort 443
                $ setRequestSecure True
                $ defaultRequest
  print request
  signedReq <- signOAuth (twOAuth token) (twCredential token) request
  response <- httpBS signedReq
  let body = getResponseBody response
  print body
  pure (decodeStrict body :: Maybe Status)

makeStatusUpdate :: Text -> Maybe StatusId -> Status
makeStatusUpdate section Nothing = Status {
  statusId = Nothing
  , statusText = section
  , statusInReplyToScreenName = Nothing
  , statusInReplyToStatusId = Nothing }
makeStatusUpdate section (Just sid) = Status {
  statusId = Nothing
  , statusText = section
  , statusInReplyToScreenName = Just "priv_lang_arg"
  , statusInReplyToStatusId = Just sid }

statusToQuery :: Status -> HT.Query
statusToQuery status = [
  (encodeUtf8 "status", Just $ encodeUtf8 $ statusText status)
  , (encodeUtf8 "in_reply_to_status_id", show <$> statusInReplyToStatusId status)
  , (encodeUtf8 "in_reply_to_screen_name", encodeUtf8 <$> statusInReplyToScreenName status)
  ]
