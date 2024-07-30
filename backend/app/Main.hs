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
import Network.Wai.Middleware.Static (staticPolicy, noDots, addBase, (>->))
import System.Environment (lookupEnv)
import System.Process
import Web.Scotty qualified as Scotty
import Data.Text (pack)
import Text.Parsec (char, digit, many1, parse, skipMany1, space, string, (<|>), ParsecT, try, newline, endOfLine, many, noneOf, option, manyTill, lookAhead, anyChar, eof)
import Text.Parsec.Text (Parser)
import Data.Functor.Identity (Identity)

type Name = Text
type PublicKey = Text

data PeerNameMap = PeerNameMap {acc :: Map.Map Name PublicKey, currentPeerName :: Name}

data DataWithUnit = DataWithUnit
  { _dataWithUnitAmount :: Text,
    _dataWithUnitUnit :: Text
  }

data PeerData = PeerData
  { peerDataStatus :: Text,
    peerDataReceived :: DataWithUnit,
    peerDataSent :: DataWithUnit,
    peerDataRest :: [String]
  }

data Peer = Peer
  { _peerName :: Text,
    _peerPeerData :: PeerData
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
    parseStatusLines peerNames_ statusLines = 
      case parse (many peerDataParser) "" (unlines $ fmap T.unpack statusLines) of
        Left _ -> []
        Right peers -> peers
      where
        peerDataParser :: Parser (Text, PeerData)
        peerDataParser = do
          _ <- string "peer: "
          publicKey <- T.pack <$> many1 (noneOf "\n")
          _ <- newline
          pName <- return $ fromMaybe (Set.singleton "Unknown") (Map.lookup publicKey peerNames_)
          pData <- peerDataFieldsParser
          return (Set.elemAt 0 pName, pData)

        peerDataFieldsParser :: Parser PeerData
        peerDataFieldsParser = do
          status <- option "Offline" (try latestHandshakeParser)
          (received, sent) <- option (DataWithUnit "" "", DataWithUnit "" "") (try transferLineParser)
          rest <- manyTill anyChar (try (lookAhead (string "peer: ")) <|> eof)
          return $ PeerData status received sent (lines rest)

        latestHandshakeParser :: Parser Text
        latestHandshakeParser = do
          _ <- string "  latest handshake: "
          handshake <- many1 (noneOf "\n")
          _ <- newline
          return $ if handshake == "0 seconds ago" || " seconds ago" `List.isSuffixOf` handshake || " minute ago" `List.isSuffixOf` handshake || " minutes ago" `List.isSuffixOf` handshake
            then "Online"
            else "Offline"

invert :: (Ord k, Ord v) => Map.Map k v -> Map.Map v (Set k)
invert = Map.foldlWithKey (\acc_ k v -> Map.insertWith Set.union v (Set.singleton k) acc_) Map.empty

-- Define a parser for amounts and units
amountAndUnitParser :: ParsecT Text () Identity DataWithUnit
amountAndUnitParser = do
  amount <- many1 (digit <|> char '.')
  skipMany1 space
  unit <- try (string "KiB")
      <|> try (string "MiB")
      <|> try (string "GiB")
      <|> string "B"
  return $ DataWithUnit (pack amount) (pack unit)

-- Define a parser for the entire transfer line
transferLineParser :: Parser (DataWithUnit, DataWithUnit)
transferLineParser = do
  _ <- string "  transfer: "
  received <- amountAndUnitParser
  _ <- string " received, "
  sent <- amountAndUnitParser
  _ <- string " sent"
  _ <- newline
  return (received, sent)

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
