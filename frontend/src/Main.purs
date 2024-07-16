module Main
  ( Action(..)
  , Peer
  , PeerData
  , State(..)
  , component
  , fetchPeers
  , handleAction
  , main
  , peerHtml
  )
  where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat (json)
import Affjax.Web (driver)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftAff)
import Halogen as H
import Halogen.Aff (runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Halogen.Aff.Util (awaitBody)
import Halogen.HTML as HH

type PeerData = 
    { 
      status :: String
    , transfer :: String
    }

type Peer = 
  { name :: String
  , data :: PeerData
  }

fetchPeers :: Aff (Array Peer)
fetchPeers = do
  response <- AX.get driver json "/peers"
  case response of
    Left _err -> pure []
    Right r -> case decodeJson r.body of
      Left _decodeErr -> pure []
      Right decoded -> pure decoded

data Action = FetchAndUpdatePeers | SetPeers (Array Peer)

data State = State
  { peers :: Array Peer
  }

initialState :: forall t. t -> State
initialState _ = State ({ peers: [] })

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render (State state) =
  HH.div_
    [ HH.div_
        (map peerHtml state.peers)
    ]

peerHtml :: forall m. Peer -> HH.ComponentHTML Action () m
peerHtml peer =
  HH.div_
    [ HH.h2_ [ HH.text $ "Peer: " <> peer.name ]
    , HH.p_ [ HH.text $ "Status: " <> peer.data.status ]
    , HH.p_ [ HH.text $ "Transfer: " <> peer.data.transfer ]
    ]

-- handleAction :: Action -> H.HalogenM State Action () State Aff Unit
handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  FetchAndUpdatePeers -> do
    peers <- liftAff fetchPeers
    H.modify_ \(State state) -> State (state { peers = peers })
  SetPeers peers -> do
    H.modify_ \(State state) -> State (state { peers = peers })

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body