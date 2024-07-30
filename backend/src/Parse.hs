{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse (parseConfig, parseStatus) where

import Data.Functor.Identity (Identity)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as T
import Text.Parsec (ParsecT, char, digit, many1, parse, skipMany1, space, string, try, (<|>))
import Text.Parsec.Text (Parser)
import Types

defaultAcc :: PeerNameMap
defaultAcc = PeerNameMap {acc = Map.empty, currentPeerName = ""}

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
parseStatus :: InvertedPeerNameMap -> String -> [Peer]
parseStatus peerNamesFromWgConfig dockerOutput = do
  let statusLines = lines dockerOutput
      peers = parseStatusLines peerNamesFromWgConfig statusLines
  fmap tupleToPeer peers
  where
    parseStatusLines :: InvertedPeerNameMap -> [String] -> [(Text, PeerData)]
    parseStatusLines _ [] = []
    parseStatusLines peerNames_ (x : xs)
      | "peer: " `List.isPrefixOf` x =
          let publicKey = T.strip (T.pack (drop 6 x))
              pName = fromMaybe (Set.singleton "Unknown") (Map.lookup publicKey peerNames_)
              (pData, _rest) = parsePeerData xs (PeerData "" (DataWithUnit "" "") (DataWithUnit "" "") [])
              (updatedPeerData, remaining) = ensureOfflineIfNoHandshake pData
           in (Set.elemAt 0 pName, updatedPeerData) : parseStatusLines peerNames_ remaining
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
          let transferText = T.pack (drop 2 x) -- Drop the leading "  " spaces
              parseResult = parse transferLineParser "" transferText
           in case parseResult of
                Left _ -> parsePeerData xs acc_ -- Handle parse error, skip this line
                Right (received, sent) ->
                  parsePeerData xs $ acc_ {peerDataReceived = received, peerDataSent = sent, peerDataRest = xs}
      | "peer: " `List.isPrefixOf` x = (acc_, x : xs) -- Stop parsing current peer data on encountering next peer
      | otherwise = parsePeerData xs $ acc_ {peerDataRest = xs}

    ensureOfflineIfNoHandshake :: PeerData -> (PeerData, [String])
    ensureOfflineIfNoHandshake pdata =
      if peerDataStatus pdata == ""
        then (pdata {peerDataStatus = "Offline"}, peerDataRest pdata)
        else (pdata, peerDataRest pdata)

-- Define a parser for amounts and units
amountAndUnitParser :: ParsecT Text () Identity DataWithUnit
amountAndUnitParser = do
  amount <- many1 (digit <|> char '.')
  skipMany1 space
  unit <-
    try (string "KiB")
      <|> try (string "MiB")
      <|> try (string "GiB")
      <|> string "B"
  return $ DataWithUnit (pack amount) (pack unit)

-- Define a parser for the entire transfer line
transferLineParser :: Parser (DataWithUnit, DataWithUnit)
transferLineParser = do
  _ <- string "transfer: "
  received <- amountAndUnitParser
  _ <- string " received, "
  sent <- amountAndUnitParser
  _ <- string " sent"
  return (received, sent)