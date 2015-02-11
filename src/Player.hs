{-# LANGUAGE OverloadedStrings #-}
module Player where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as B
import           System.Directory
import           Data.List (find)
import           Data.Maybe
import           Text.JSON
import qualified Filesystem.Path.CurrentOS as P
import           Data.Text (unpack)

import Control.Monad.IO.Class (MonadIO (..))

import           System.Process

-- api/setTrackList
-- {"trackList":["books/2hushit/gensokyo 1/CD1/2hushit - Gensokyo 1- Music For Shrine Maidens - 10 Lewd Butts.ogg"]}
-- api/unPause
-- api/activateTimeout

parseTrackList :: String -> Either String [FilePath]
parseTrackList jsonString = case decode jsonString of
                              Ok a      -> case (valFromObj "trackList" a) of
                                             Ok b -> Right $ map (fromJSString) b
                                             Error msgB -> Left msgB
                              Error msg -> Left msg

setTrackList :: MonadSnap m => m ()
setTrackList = do
  req <- getRequest
  trackListJson <- getPostParam "trackList"
  case trackListJson of
    Just tj -> case parseTrackList (B.unpack tj) of
                 Right tracks -> writeBS $ B.unlines $ map (B.pack . (++) "public/") tracks
                 Left msg -> writeBS $ B.append "fucked up trackList json" $ B.pack msg
    Nothing -> writeBS "no data in POST:trackList"
-- process <- liftIO $ createProcess $ proc "mplayer" [path]


play :: MonadSnap m => m ()
play = do
  writeBS "TODO"
