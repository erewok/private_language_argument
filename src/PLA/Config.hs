{-# LANGUAGE OverloadedStrings #-}

module PLA.Config where


import Control.Applicative
import Control.Lens
import qualified Data.ByteString.Char8 as C8
import Data.Typeable (Typeable)
import Network.HTTP.Conduit
import qualified Network.URI as URI
import Protolude
import System.Environment
import Web.Authenticate.OAuth


data TwitToken = TwitToken
    { twOAuth :: OAuth
    , twCredential :: Credential
    } deriving (Show, Read, Eq, Typeable)

getTextLocation :: IO FilePath
getTextLocation = getEnv "TEXT_LOCATION"

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    consumerKey <- getEnv' "TWITTER_API_KEY"
    consumerSecret <- getEnv' "TWITTER_CONSUMER_SECRET"
    accessToken <- getEnv' "TWITTER_ACCESS_TOKEN"
    accessSecret <- getEnv' "TWITTER_ACCESS_TOKEN_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
  where
    getEnv' = (C8.pack <$>) . getEnv


getTWInfoFromEnv :: IO TwitToken
getTWInfoFromEnv = do
    (oa, cred) <- getOAuthTokens
    return $ TwitToken oa cred

twitterOAuth :: OAuth
twitterOAuth =
    def { oauthServerName = "twitter"
        , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
        , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
        , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
        , oauthConsumerKey = ""
        , oauthConsumerSecret = ""
        , oauthSignatureMethod = HMACSHA1
        , oauthCallback = Nothing
        }
