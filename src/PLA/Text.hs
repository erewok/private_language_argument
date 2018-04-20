{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module PLA.Text (
  getText
  , textToList
  ) where


import qualified Data.Text as T
import Protolude
import System.Directory (doesFileExist)


_CURRENT_FILE_NAME :: FilePath
_CURRENT_FILE_NAME = "CURRENT"

type TextLoc = FilePath
type CurrentRecordFile = FilePath

textToList :: Text -> [Text]
textToList txt = filter (\t -> T.length t > 0 ) $ T.lines txt

-- Pass in the directory and this function will return the most recent
-- section. It will also increment for next access.
getText :: TextLoc -> IO Text
getText loc = do
  currentSection <- incrementCurrentSection loc
  let currentText = loc <> "/" <> T.unpack currentSection
  itExists <- doesFileExist currentText
  if itExists
    then readFile currentText
    else startCounting (currentFileLoc loc) *> readFile (loc <> "/1")

currentFileLoc :: TextLoc -> CurrentRecordFile
currentFileLoc loc = loc <> "/" <> _CURRENT_FILE_NAME

incrementCurrentSection :: TextLoc -> IO Text
incrementCurrentSection fp = do
  let currentFp = currentFileLoc fp
  itExists <- doesFileExist currentFp
  if not itExists
    then startCounting currentFp
    else do
      currentNum <- readFile currentFp
      let numText = readMaybe $ T.unpack currentNum :: Maybe Int
      case numText of
        (Just n) -> writeFile currentFp (T.pack . show $ n + 1) >> return (T.pack . show $ n)
        Nothing -> startCounting currentFp

startCounting :: CurrentRecordFile -> IO Text
startCounting fp = writeFile fp "2" >> return "1"
