{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as B

import Control.Monad.IO.Class (MonadIO (..))

import qualified Listing
import qualified Player

main :: IO ()
main = quickHttpServe site

printEmpty = writeBS "TODO"

apiArray =
 [ ("api", printApi)
 , ("api/authors.json", Listing.authors)
 , ("api/artists.json", Listing.authors) -- just an alias for deprecated stuff
 , ("api/author.json", Listing.author)
 , ("api/book.json", Listing.book)
 , ("api/normalize", printEmpty)
 , ("api/play", Player.play)
 , ("api/setTrackList", Player.setTrackList)
 , ("api/togglePause", printEmpty)
 , ("api/pause", printEmpty)
 , ("api/unPause", printEmpty)
 , ("api/stop", printEmpty)
 , ("api/getProgress", printEmpty)
 , ("api/getPlayStatus", printEmpty)
 , ("api/activateTimeout", printEmpty)
 , ("api/removeTimeout", printEmpty)
 , ("api/setVolume", printEmpty)
 , ("api/getVolume", printEmpty)
 , ("api/createCoverThumbnails", printEmpty)
 , ("api/unifyTrackNamesForCd", printEmpty)
 , ("api/buildDate", printEmpty)
 , ("t",  (do
            req <- getRequest
            writeBS (rqQueryString req)))
 ]

appendSlash :: B.ByteString -> B.ByteString
appendSlash = (flip B.append) "/"

printApi = writeBS (B.unlines (map (appendSlash . fst) apiArray))

site :: Snap ()
site =
    id (serveDirectory "public") <|>
    route apiArray

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
