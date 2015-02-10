{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as B
import           AnyDir
import           System.Directory
import           Data.List (find)
import           Data.Maybe
import           Text.JSON

import Control.Monad.IO.Class (MonadIO (..))

main :: IO ()
main = quickHttpServe site

printEmpty = writeBS "TODO"

apiArray =
 [ ("api", printApi)
 , ("api/authors.json", authors)
 , ("api/artists.json", authors) -- just an alias for deprecated stuff
 , ("api/author.json", author)
 , ("api/book.json", printEmpty)
 , ("api/normalize", printEmpty)
 , ("api/play", printEmpty)
 , ("api/setTrackList", printEmpty)
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

booksRoot = "public/books/"
authorsPath = booksRoot

type Author = B.ByteString
type Cover = B.ByteString

subDirs :: FilePath -> IO [FilePath]
subDirs path = do
  dirNames  <- getDirectoryContents path
  existing  <- filterM (\x -> (do

                               exists <- doesDirectoryExist (path ++ x)
                               return (exists && not(x == ".") && not(x == "..")))) dirNames
  dirNamesB <- mapM (return) existing
  return dirNamesB

authorsArray :: IO [FilePath]
authorsArray = subDirs authorsPath

cover :: FilePath -> FilePath -> IO (Maybe FilePath)
cover prefix current =
    let extensions = [".png", ".jpg", ".jpeg"]
        p = (prefix ++ current) in
    foldM (\acc v -> if (mempty == acc)
                     then (do
                            exists <- doesFileExist (p ++ "/cover" ++ v)
                            print p
                            if exists
                            then return (Just (current ++ "/cover" ++ v))
                            else return acc)
                     else (return acc)
          ) mempty extensions
-- find (\e -> doesFileExist (p ++ "/cover" ++ e)) extensions

-- cover p | doesFileExist (p ++ "/cover.png")  = Just (p ++ "/cover.png")
--         | doesFileExist (p ++ "/cover.jpg")  = Just (p ++ "/cover.jpg")
--         | doesFileExist (p ++ "/cover.jpeg") = Just (p ++ "/cover.jpeg")
--         | otherwise = Nothing

filterCovers :: [(Maybe FilePath)] -> [FilePath]
filterCovers cs = map (fromJust) $ filter (isJust) cs


authorsJson :: [FilePath] -> [FilePath] -> JSObject JSValue
authorsJson as cs = toJSObject [ ("authors", JSArray $ map (JSString . toJSString) as)
                               , ("covers", JSArray $ map (JSString . toJSString) cs)
                               ]

authors :: MonadSnap m => m ()
authors = do
  a <- liftIO authorsArray
  c <- liftIO $ sequence $ map (cover authorsPath) a
  -- show $ showJSON $ JSArray
  writeBS $ B.pack $ (showJSObject $ authorsJson
                                       a
                                       (map (fromJust) $ filter (isJust) c)
                     ) ""

authorJson :: [FilePath] -> [FilePath] -> JSObject JSValue
authorJson bs cs = toJSObject [ ("books", JSArray $ map (JSString . toJSString) bs)
                              , ("covers", JSArray $ map (JSString . toJSString) cs)
                              ]

author :: MonadSnap m => m ()
author = do
  req <- getRequest
  let p = authorsPath ++ (B.unpack $ fromJust $ urlDecode $ rqQueryString req) ++ "/" in (do
    b <- liftIO $ subDirs p
    c <- liftIO $ sequence $ map (cover p) b
    writeBS $ B.pack $ (showJSObject $ authorJson b (filterCovers c)) "")

-- apiRoutes = map (\v -> (fst v, snd v)) apiArray

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
