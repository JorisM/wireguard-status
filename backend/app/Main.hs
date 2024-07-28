{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Network.HTTP.Types.Status (status200)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import System.Environment (lookupEnv)
import System.IO
import System.Process
import Text.Printf (printf)
import Web.Scotty qualified as Scotty

type Name = Text

type PublicKey = Text

data PeerNameMap = PeerNameMap {acc :: Map.Map Name PublicKey, currentPeerName :: Name}

data PeerData = PeerData
  { status :: Text,
    transfer :: Text,
    rest :: [String]
  }

data Peer = Peer
  { name :: Text,
    peerData :: PeerData
  }

defaultAcc :: PeerNameMap
defaultAcc = PeerNameMap {acc = Map.empty, currentPeerName = ""}

type InvertedPeerNameMap = Map.Map PublicKey (Set Name)

instance ToJSON PeerData where
  toJSON (PeerData status transfer _) =
    object ["status" .= status, "transfer" .= transfer]

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
    parseStatusLines peerNames (x : xs)
      | "peer: " `List.isPrefixOf` x =
          let publicKey = T.strip (T.pack (drop 6 x))
              peerName = fromMaybe (Set.singleton "Unknown") (Map.lookup publicKey peerNames)
              peerData = parsePeerData xs (PeerData "" "" xs)
           in (Set.elemAt 0 peerName, peerData) : parseStatusLines peerNames (rest peerData)
      | otherwise = parseStatusLines peerNames xs

    parsePeerData :: [String] -> PeerData -> PeerData
    parsePeerData [] acc = acc
    parsePeerData (x : xs) acc
      | "  latest handshake: " `List.isPrefixOf` x =
          let handshake = T.strip (T.pack (drop 19 x))
              status =
                if handshake == "0 seconds ago" || " seconds ago" `T.isSuffixOf` handshake || " minute ago" `T.isSuffixOf` handshake || " minutes ago" `T.isSuffixOf` handshake
                  then "Online"
                  else "Offline"
           in parsePeerData xs $ acc {status = status, rest = xs}
      | "  transfer: " `List.isPrefixOf` x =
          let transfer = T.strip (T.pack x)
           in acc {transfer = transfer, rest = xs}
      | otherwise = parsePeerData xs $ acc {rest = xs}

invert :: (Ord k, Ord v) => Map.Map k v -> Map.Map v (Set k)
invert = Map.foldlWithKey (\acc k v -> Map.insertWith Set.union v (Set.singleton k) acc) Map.empty

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
