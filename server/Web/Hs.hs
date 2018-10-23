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

import           Control.Monad
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.List                as L
import           Data.Semigroup           ((<>))
import qualified Data.Text.Lazy           as T
import           Data.Text.Lazy.Encoding
import           Lucid
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as WA
import           Options.Applicative
import           System.Directory

server :: Options -> Application
server opts req respond = do
  let path = L.intercalate "/" $
        base opts : L.map (T.unpack . T.fromStrict) (pathInfo req)
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  case (isFile, isDir) of
    (True, False) ->
      respond $ responseFile
      status200
      []
      path
      Nothing
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
  conts <- L.sort <$> listDirectory path
  dirs <- L.sort <$> (filterM doesDirectoryExist =<< listDirectory path)
  let files = L.filter (not . flip elem dirs) conts
  return . renderBS $ do
    doctype_
    html_ $ do
      head_ $
        title_ [] "Hello Lucid!"
      body_ $ do
        h1_ [] "Lucid"
        mapM_ (p_ [] . toHtml) dirs
        mapM_ (p_ [] . toHtml) files

data Options = Options
  { base    :: String
  , port    :: Int
  , version :: Bool
  } deriving (Show, Eq)

options :: IO (Parser Options)
options = do
  path <- getCurrentDirectory
  return $ Options
    <$> argument str
    ( metavar "DIR" <>
      showDefaultWith (const "<current directory>") <>
      value path <>
      help "Base path"
    )
    <*> option auto
    ( long "port" <>
      metavar "PORT" <>
      short 'p' <>
      short 'P' <>
      showDefault <>
      value 8080 <>
      help "Port"
    )
    <*> switch
    ( long "version" <>
      short 'v' <>
      help "Show version."
    )

run :: IO ()
run = do
  optsP <- options
  opts <- execParser
    ( info (optsP <**> helper)
      ( fullDesc <>
        header "hs - Simple web sever." )
    )
  if version opts
    then
    print "hs - version 0.0.1"
    else
    WA.run (port opts) (server opts)
