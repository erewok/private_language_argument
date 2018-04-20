{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module PLA.Types where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Protolude


type StatusId = Integer

data Status = Status
    { statusId :: Maybe StatusId
    , statusInReplyToScreenName :: Maybe Text
    , statusInReplyToStatusId :: Maybe StatusId
    , statusText :: Text
    } deriving (Show, Eq)

checkError :: Object -> Parser ()
checkError o = do
    err <- o .:? "errors"
    case err of
        Just msg -> fail msg
        Nothing -> return ()

instance FromJSON Status where
    parseJSON (Object o) = checkError o >> Status
      <$> o .:?  "id" .!= Nothing
      <*> o .:? "in_reply_to_screen_name" .!= Nothing
      <*> o .:? "in_reply_to_status_id" .!= Nothing
      <*> o .:  "text"
    parseJSON v = fail $ "couldn't parse status from: " ++ show v

instance ToJSON Status where
    toJSON Status{..} = object ["in_reply_to_screen_name"  .= statusInReplyToScreenName
                               , "in_reply_to_status_id"    .= statusInReplyToStatusId
                               , "status"                     .= statusText
                               ]
