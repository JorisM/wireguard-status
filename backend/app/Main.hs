{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Parse (parseConfig, parseStatus)
import System.Environment (lookupEnv)
import System.Process
import Web.Scotty qualified as Scotty

invert :: (Ord k, Ord v) => Map.Map k v -> Map.Map v (Set k)
invert = Map.foldlWithKey (\acc_ k v -> Map.insertWith Set.union v (Set.singleton k) acc_) Map.empty

main :: IO ()
main = do
  wgConfFile <- fromMaybe "vpn/config/wg0.conf" <$> lookupEnv "WG_CONF_FILE"
  nameOfWireguardContainer <- fromMaybe "wireguard" <$> lookupEnv "WG_CONTAINER_NAME"
  configContents <- readFile wgConfFile
  let peerNames = parseConfig configContents
  let invertedPeerNames = invert peerNames

  Scotty.scotty 8888 $ do
    Scotty.middleware $ staticPolicy (noDots >-> addBase "static")

    Scotty.get "/" $ do
      Scotty.file "/usr/local/bin/static/index.html"

    Scotty.get "/index.js" $ do
      Scotty.file "/usr/local/bin/static/index.js"

    Scotty.get "/peers" $ do
      status <- liftIO $ readProcess "docker" ["exec", nameOfWireguardContainer, "wg", "show"] ""
      let peers = parseStatus invertedPeerNames status
      Scotty.json peers
