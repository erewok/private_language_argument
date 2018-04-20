{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PLA.Post where

import Control.Lens
import Control.Monad (fail)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
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
piTextToStatus token [txt] Nothing = postSectionToTwitter token (makeStatusUpdate txt Nothing)
                               *> pure ()
piTextToStatus token [txt] (Just sid) = postSectionToTwitter token (makeStatusUpdate txt (Just sid))
                                  *> pure ()
piTextToStatus token (txt:txts) Nothing = do
  newStatus <- postSectionToTwitter token $ makeStatusUpdate txt Nothing
  case newStatus of
    Nothing -> pure ()
    Just (status) -> piTextToStatus token txts $ statusId status
piTextToStatus token (txt:txts) (Just sid) = do
  newStatus <- postSectionToTwitter token $ makeStatusUpdate txt (Just sid)
  case newStatus of
    Nothing -> pure ()
    Just (status) -> piTextToStatus token txts $ statusId status

postSectionToTwitter :: TwitToken -> Status -> IO (Maybe Status)
postSectionToTwitter token status = do
  let request = setRequestQueryString (statusToQuery status)
                $ setRequestMethod "POST"
                $ setRequestPath statusUpdateEndpoint
                $ setRequestHost twitterHost
                $ setRequestPort 443
                $ setRequestSecure True
                $ defaultRequest
  signedReq <- signOAuth (twOAuth token) (twCredential token) $ request
  response <- httpBS signedReq
  let body = getResponseBody response
  print $ body
  pure (decodeStrict body :: Maybe Status)


statusToQuery :: Status -> HT.Query
statusToQuery status = [
  (encodeUtf8 "status", Just $ encodeUtf8 $ statusText status)
  , (encodeUtf8 "in_reply_to_status_id", show <$> statusInReplyToStatusId status)
  , (encodeUtf8 "in_reply_to_screen_name", encodeUtf8 <$> statusInReplyToScreenName status)
  ]


makeStatusUpdate :: Text -> Maybe StatusId -> Status
makeStatusUpdate section Nothing = Status {
  statusId = Nothing
  , statusText = section
  , statusInReplyToScreenName = Nothing
  , statusInReplyToStatusId = Nothing }
makeStatusUpdate section (Just sid) = Status {
  statusId = Nothing
  , statusText = section <> " @priv_lang_arg"
  , statusInReplyToScreenName = Just "priv_lang_arg"
  , statusInReplyToStatusId = Just sid }
