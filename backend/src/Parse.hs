{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parse (parseConfig, parseStatus) where

import Data.Functor.Identity (Identity)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as T
import Text.Parsec (ParsecT, anyChar, char, digit, many, many1, manyTill, newline, parse, skipMany1, space, string, try, (<|>))
import Text.Parsec.Text (Parser)
import Types

data Accumulator = Accumulator
  { acc :: PeerNameMap,
    currentPeerName :: Text
  }

defaultAcc :: Accumulator
defaultAcc = Accumulator Map.empty T.empty

parseConfig :: String -> PeerNameMap
parseConfig contents = do
  let parseResult = parse configFileParser "unknown-location" (pack contents)
  case parseResult of
    Left err -> error (show err)
    Right peerMap -> peerMap
  where
    configFileParser :: Parser PeerNameMap
    configFileParser = do
      result <- many parseLine
      let result_ = catMaybes result
      return $ acc $ foldl updatePeerMap defaultAcc result_

    parseLine :: Parser (Maybe (Text, Text))
    parseLine = try parseName <|> try parsePublicKey <|> skipLine

    parseName :: Parser (Maybe (Text, Text))
    parseName = do
      _ <- string "# Name: "
      name <- manyTill anyChar newline
      return $ Just (T.strip (T.pack name), T.empty)

    parsePublicKey :: Parser (Maybe (Text, Text))
    parsePublicKey = do
      _ <- string "PublicKey = "
      publicKey <- manyTill anyChar newline
      return $ Just (T.empty, T.strip (T.pack publicKey))

    skipLine :: Parser (Maybe (Text, Text))
    skipLine = manyTill anyChar newline >> return Nothing

    updatePeerMap :: Accumulator -> (Text, Text) -> Accumulator
    updatePeerMap acc_ (name, key)
      | not (T.null name) = acc_ {currentPeerName = name}
      | not (T.null key) =
          let name = currentPeerName acc_
              newAcc = Map.insert name key (acc acc_)
           in acc_ {acc = newAcc, currentPeerName = T.empty}
      | otherwise = acc_

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

    tupleToPeer :: (Text, PeerData) -> Peer
    tupleToPeer (name, peerData) = Peer name peerData

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