{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Network.Wai.Middleware.Static
import System.Environment (lookupEnv)
import System.Process
import Web.Scotty qualified as Scotty

type Name = Text

type PublicKey = Text

data PeerNameMap = PeerNameMap {acc :: Map.Map Name PublicKey, currentPeerName :: Name}

data DataWithUnit = DataWithUnit
  { amount :: Text,
    unit :: Text
  }

data PeerData = PeerData
  { peerDataStatus :: Text,
    peerDataReceived :: DataWithUnit,
    peerDataSent :: DataWithUnit,
    peerDataRest :: [String]
  }

data Peer = Peer
  { peerName :: Text,
    peerPeerData :: PeerData
  }

defaultAcc :: PeerNameMap
defaultAcc = PeerNameMap {acc = Map.empty, currentPeerName = ""}

type InvertedPeerNameMap = Map.Map PublicKey (Set Name)

instance ToJSON DataWithUnit where
  toJSON (DataWithUnit amount unit) =
    object ["amount" .= amount, "unit" .= unit]

instance ToJSON PeerData where
  toJSON (PeerData status received sent _) =
    object ["status" .= status, "received" .= received, "sent" .= sent]

instance ToJSON Peer where
  toJSON (Peer name peerData) =
    object ["name" .= name, "data" .= peerData]

-- Parse WireGuard configuration file to extract names
parseConfig :: FilePath -> IO PeerNameMap
parseConfig confFile = do
  contents <- readFile confFile
  let linesOfFiles = lines contents
      peerMap = List.foldl' parseLine defaultAcc linesOfFiles
  return peerMap
  where
    parseLine accumulator line
      | "# Name: " `List.isPrefixOf` line =
          accumulator
            { acc = Map.insert (T.strip (T.pack (drop 8 line))) T.empty (acc accumulator),
              currentPeerName = T.strip (T.pack (drop 8 line))
            }
      | "PublicKey = " `List.isPrefixOf` line =
          accumulator
            { acc = Map.insert (currentPeerName accumulator) (T.strip (T.pack (drop 11 line))) (acc accumulator)
            }
      | otherwise = accumulator

tupleToPeer :: (Text, PeerData) -> Peer
tupleToPeer (name, peerData) = Peer name peerData

-- Get WireGuard status and append names
parseStatus :: InvertedPeerNameMap -> IO [Peer]
parseStatus peerNames = do
  nameOfWireguardContainer <- fromMaybe "wireguard" <$> lookupEnv "WG_CONTAINER_NAME"
  status <- readProcess "docker" ["exec", nameOfWireguardContainer, "wg", "show"] ""
  let statusLines = lines status
      peers = parseStatusLines peerNames statusLines
  return $ fmap tupleToPeer peers
  where
    parseStatusLines :: InvertedPeerNameMap -> [String] -> [(Text, PeerData)]
    parseStatusLines _ [] = []
    parseStatusLines peerNames_ (x : xs)
      | "peer: " `List.isPrefixOf` x =
          let publicKey = T.strip (T.pack (drop 6 x))
              peerName = fromMaybe (Set.singleton "Unknown") (Map.lookup publicKey peerNames_)
              peerData_ = parsePeerData xs (PeerData "" (DataWithUnit "" "") (DataWithUnit "" "") xs)
              (updatedPeerData, rest) = ensureOfflineIfNoHandshake peerData_
           in (Set.elemAt 0 peerName, updatedPeerData) : parseStatusLines peerNames_ rest
      | otherwise = parseStatusLines peerNames_ xs

    parsePeerData :: [String] -> PeerData -> (PeerData, [String])
    parsePeerData [] acc_ = (acc_, [])
    parsePeerData (x : xs) acc_
      | "  latest handshake: " `List.isPrefixOf` x =
          let handshake = T.strip (T.pack (drop 19 x))
              status_ =
                if handshake == "0 seconds ago" || " seconds ago" `T.isSuffixOf` handshake || " minute ago" `T.isSuffixOf` handshake || " minutes ago" `T.isSuffixOf` handshake
                  then "Online"
                  else "Offline"
           in parsePeerData xs $ acc_ {peerDataStatus = status_, peerDataRest = xs}
      | "  transfer: " `List.isPrefixOf` x =
          let transferText = T.strip (T.pack x)
              transferParts = T.splitOn ", " (T.drop 11 transferText)
              receivedParts = T.words $ transferParts !! 0
              sentParts = T.words $ transferParts !! 1
              received = DataWithUnit (receivedParts !! 0) (receivedParts !! 1)
              sent = DataWithUnit (sentParts !! 0) (sentParts !! 1)
           in parsePeerData xs $ acc_ {peerDataReceived = received, peerDataSent = sent, peerDataRest = xs}
      | otherwise = parsePeerData xs $ acc_ {peerDataRest = xs}

    ensureOfflineIfNoHandshake :: PeerData -> (PeerData, [String])
    ensureOfflineIfNoHandshake pdata =
      if peerDataStatus pdata == ""
        then (pdata {peerDataStatus = "Offline"}, peerDataRest pdata)
        else (pdata, peerDataRest pdata)

invert :: (Ord k, Ord v) => Map.Map k v -> Map.Map v (Set k)
invert = Map.foldlWithKey (\acc_ k v -> Map.insertWith Set.union v (Set.singleton k) acc_) Map.empty

main :: IO ()
main = do
  wgConfFile <- fromMaybe "vpn/config/wg0.conf" <$> lookupEnv "WG_CONF_FILE"
  peerNames <- parseConfig wgConfFile
  let invertedPeerNames = invert $ acc peerNames

  Scotty.scotty 8888 $ do
    Scotty.middleware $ staticPolicy (noDots >-> addBase "static")

    Scotty.get "/" $ do
      Scotty.file "/usr/local/bin/static/index.html"

    Scotty.get "/index.js" $ do
      Scotty.file "/usr/local/bin/static/index.js"

    Scotty.get "/peers" $ do
      peers <- liftIO $ parseStatus invertedPeerNames
      Scotty.json peers
