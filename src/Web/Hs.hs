----------------------------------------------------------------------------
-- |
-- Module      :  Web.Hs
-- Copyright   :  (c) Akihito KIRISAKI 2018
-- License     :  BSD3
--
-- Maintainer  :  Akihito KIRISAKI <kirisaki@klaraworks.net>
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Web.Hs
  ( run
  ) where

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.List                as L
import qualified Data.Text.Lazy           as T
import           Data.Text.Lazy.Encoding
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as WA
import           System.Directory

server :: FilePath -> Application
server base req respond = do
  let path = T.intercalate "/" $ T.pack base : L.map T.fromStrict (pathInfo req)
  exists <- doesFileExist $ T.unpack path
  if exists
    then
    respond $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    (encodeUtf8 $ path `T.append` " exists")
    else
    respond $ responseLBS
    status404
    [("Content-Type", "text/plain")]
    (encodeUtf8 $ path `T.append` " doesn't exist")

run :: IO ()
run = do
  path <- getCurrentDirectory
  WA.run 8080 (server path)
