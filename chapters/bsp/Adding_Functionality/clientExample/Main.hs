{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Value(String))
import           Data.Monoid ((<>))
import           Data.Default (def)
import qualified Data.Text.Lazy as TL
import           Hunt.Common.DocDesc
import           Hunt.Server.Client
import           Hunt.ClientInterface
import           Network.Wai.Handler.Warp
import           Web.Scotty
main :: IO ()
main = do
  let settings = setPort 8080 defaultSettings
      options = def {settings = settings}
  scottyOpts options $ do
    get "/" $ fileWithMime "index.html" "text/html"
    get "/search" $ do
        query <- param "query"
        results <- liftIO
            $ withHuntServer "http://localhost:3000"
            $ getQuery query 20 0
        html $ "<table>"
            <> (TL.unlines $ render `fmap` (lrResult results))
            <> "</table>"
fileWithMime :: FilePath -> TL.Text -> ActionM ()
fileWithMime path mime = do
  setHeader "Content-Type" mime
  file path
render :: ApiDocument -> TL.Text
render d = TL.unlines $ mkRow `fmap` (toList $ adDescr d)
  where
  mkRow (k, String v) = "<tr><td>" <> (TL.fromStrict k)
      <> "</td><td>" <> (TL.pack $ show v) <> "</td></tr>"
