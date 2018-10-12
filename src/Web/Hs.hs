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
import           Lucid
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as WA
import           System.Directory

server :: FilePath -> Application
server base req respond = do
  let path = L.intercalate "/" $
        base : L.map (T.unpack . T.fromStrict) (pathInfo req)
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  case (isFile, isDir) of
    (True, False) ->
      respond $ responseLBS
      status200
      [("Content-Type", "text/plain")]
      (encodeUtf8 $ T.pack path `T.append` " exists")
    (False, True) -> do
      html <- fileList path
      respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        html
    _ ->
      respond $ responseLBS
      status404
      [("Content-Type", "text/plain")]
      "404 Not found"

fileList :: FilePath -> IO LBS.ByteString
fileList path = do
  contents <- getDirectoryContents path
  return . renderBS $ do
    doctype_
    html_ $ do
      head_ $
        title_ [] "Hello Lucid!"
      body_ $ do
        h1_ [] "Lucid"
        mapM_ (p_ [] . toHtml) contents

run :: IO ()
run = do
  path <- getCurrentDirectory
  WA.run 8080 (server path)
