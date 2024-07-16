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
import Control.Monad.Rec.Class (forever)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.Aff (runHalogenAff)
import Halogen.Aff.Util (awaitBody)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

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

data Action = Initialize | FetchAndUpdatePeers | SetPeers (Array Peer)

data State = State
  { peers :: Array Peer
  }

initialState :: forall t. t -> State
initialState _ = State { peers: [] }

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render (State state) =
  HH.div_
    [ HH.div_
        (map peerHtml state.peers)
    , HH.button [ onClick \_ -> FetchAndUpdatePeers ] [ HH.text "Fetch and Update Peers" ]
    ]

peerHtml :: forall m. Peer -> HH.ComponentHTML Action () m
peerHtml peer =
  HH.div_
    [ HH.h2_ [ HH.text $ "Peer: " <> peer.name ]
    , HH.p_ [ HH.text $ "Status: " <> peer.data.status ]
    , HH.p_ [ HH.text $ "Transfer: " <> peer.data.transfer ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.subscribe =<< timer FetchAndUpdatePeers
    pure unit
  FetchAndUpdatePeers -> do
    peers <- liftAff fetchPeers
    H.modify_ \(State state) -> State (state { peers = peers })
  SetPeers peers -> do
    H.modify_ \(State state) -> State (state { peers = peers })


main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body


timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 2000.0
    H.liftEffect $ HS.notify listener val
  pure emitter