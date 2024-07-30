{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (PeerData (..), DataWithUnit (..), Name, PublicKey, Peer (..), InvertedPeerNameMap, PeerNameMap (..)) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)

type Name = Text

type PublicKey = Text

type PeerNameMap = Map.Map PublicKey Name

data DataWithUnit = DataWithUnit
  { _dataWithUnitAmount :: Text,
    _dataWithUnitUnit :: Text
  }
  deriving (Show, Eq)

data PeerData = PeerData
  { peerDataStatus :: Text,
    peerDataReceived :: DataWithUnit,
    peerDataSent :: DataWithUnit,
    peerDataRest :: [String]
  }
  deriving (Show, Eq)

data Peer
  = Peer
  { _peerName :: Text,
    _peerPeerData :: PeerData
  }
  deriving (Show, Eq)

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