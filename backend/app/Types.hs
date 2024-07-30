{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (PeerData (..), DataWithUnit (..), Name, PublicKey, Peer (..), InvertedPeerNameMap, PeerNameMap (..)) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)

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