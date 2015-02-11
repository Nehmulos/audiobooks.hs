{-# LANGUAGE OverloadedStrings #-}
module Listing where

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

type Author = B.ByteString
type Cover = B.ByteString

publicPath = "public/"
booksRoot = "public/books/"
authorsPath = booksRoot

subFS :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
subFS check path = do
  dirNames  <- getDirectoryContents path
  existing  <- filterM (\x -> (do
                               exists <- check (path ++ x)
                               return (exists && not(x == ".") && not(x == "..")))) dirNames
  dirNamesB <- mapM (return) existing
  return dirNamesB

subFiles :: FilePath -> IO [FilePath]
subFiles = subFS doesFileExist

subDirs :: FilePath -> IO [FilePath]
subDirs = subFS doesDirectoryExist


authorsArray :: IO [FilePath]
authorsArray = subDirs authorsPath

cover :: FilePath -> FilePath -> IO (Maybe FilePath)
cover prefix current =
    let extensions = [".png", ".jpg", ".jpeg"]
        p = (prefix ++ current) in
    foldM (\acc v -> if (mempty == acc)
                     then (do
                            exists <- doesFileExist (p ++ "/cover" ++ v)
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

isMediaExt :: String -> Bool
isMediaExt ext
    | ext == "ogg"  = True
    | ext == "wav"  = True
    | ext == "opus" = True
    | ext == "flac" = True
    | ext == "aac"  = True
    | ext == "mp3"  = True
    | otherwise = False

cd :: FilePath -> IO (String, [FilePath])
cd p = do
  files <- subFiles (p ++ "/")
  return (P.encodeString $ P.filename (P.decodeString p),
          map (P.encodeString) $ filter (\f -> case P.extension f of
                                                 Just ext -> (isMediaExt $ unpack ext)
                                                 Nothing  -> False)
         $ map (P.decodeString) files)



cdJson :: (String, [FilePath]) -> JSObject JSValue
cdJson (name, tracks) = toJSObject [ ("name", JSString (toJSString name))
                                   , ("tracks", JSArray $ map (JSString . toJSString) tracks)
                                   ]
bookJson :: [JSObject JSValue] -> JSObject JSValue
bookJson cds = toJSObject [ ("cds", JSArray $ map (JSObject) cds) ]

book' :: FilePath -> FilePath -> IO (JSObject JSValue)
book' a b =
  let p = (authorsPath ++ a ++ "/" ++ b ++ "/") in (do
    cdDirs <- subDirs p
    cds <- sequence $ map (cd . ((++) p)) cdDirs
    return $ bookJson $ map (cdJson) cds)

book :: MonadSnap m => m ()
book = do
  a <- getQueryParam "author"
  b <- getQueryParam "book"
  if (isJust a) && (isJust b)
  then do
    bjs <- liftIO $ book' (B.unpack $ fromJust a) (B.unpack $ fromJust b)
    writeBS $ B.pack $ (showJSObject bjs) ""
  else writeBS "error please supply ?author=foo&query=bar"
