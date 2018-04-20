module Main where

import Protolude
import PLA
import PLA.Config
import PLA.Post
import PLA.Text

main :: IO ()
main = do
  -- get credentials
  textLoc <- getTextLocation
  twitCreds <- getTWInfoFromEnv
  text <- getText textLoc
  let preppedText = textToList text
  piTextToStatus twitCreds preppedText Nothing
