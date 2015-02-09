{-# LANGUAGE OverloadedStrings #-}
module AnyDir
  (
   anyDir
  ) where

import           Control.Applicative
import           Control.Monad
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as B

anyDir :: MonadSnap m
    => B.ByteString  -- ^ path component to match
    -> m a         -- ^ handler to run
    -> m a
-- anyDir p action =  -- pathWith (\_ _ -> True) "" action
anyDir = pathWith f
  where
    f dr pinfo = dr == x || (dr == "" || dr == "/" || dr == ".")
      where
        (x,_) = B.breakEnd (=='/') pinfo

pathWith :: MonadSnap m
         => (B.ByteString -> B.ByteString -> Bool)
         -> B.ByteString
         -> m a
         -> m a
pathWith c p action = do
    req <- getRequest
    localRequest (updateContextPath $ B.length p) action

updateContextPath :: Int -> Request -> Request
updateContextPath n req | n > 0     = req { rqContextPath = ctx
                                          , rqPathInfo    = pinfo }
                        | otherwise = req
  where
    ctx'  = B.take n (rqPathInfo req)
    ctx   = B.concat [rqContextPath req, ctx', "/"]
    pinfo = B.drop (n+1) (rqPathInfo req)
