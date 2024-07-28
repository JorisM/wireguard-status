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
  , DataWithUnit
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
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff (runHalogenAff)
import Halogen.Aff.Util (awaitBody)
import Halogen.HTML as HH
-- import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

type DataWithUnit = 
  { amount :: String
  , unit :: String
  }

type PeerData = 
  { status :: String
  , received :: DataWithUnit
  , sent :: DataWithUnit
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
  HH.div [ class_ $ ClassName "min-h-screen bg-gray-100 flex flex-col items-center" ]
    [ HH.div [ class_ $ ClassName "w-full max-w-4xl mt-10 p-5 bg-white shadow-md rounded-lg" ]
        (map peerHtml state.peers)
    -- , HH.button [ class_ $ ClassName "mt-5 px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600", onClick \_ -> FetchAndUpdatePeers ] [ HH.text "" ]
    ]

peerHtml :: forall m. Peer -> HH.ComponentHTML Action () m
peerHtml peer =
  HH.div [ class_ $ ClassName "p-5 mb-4 border-b border-gray-200" ]
    [ HH.h2 [ class_ $ ClassName "text-2xl font-semibold text-gray-800" ] [ HH.text $ "Peer: " <> peer.name ]
    , HH.p [ class_ $ ClassName "text-gray-600" ] [ HH.text $ "Status: " <> peer.data.status ]
    , HH.p [ class_ $ ClassName "text-gray-600" ] [ HH.text $ "Received: " <> peer.data.received.amount <> " " <> peer.data.received.unit ]
    , HH.p [ class_ $ ClassName "text-gray-600" ] [ HH.text $ "Sent: " <> peer.data.sent.amount <> " " <> peer.data.sent.unit ]
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
    H.liftEffect $ HS.notify listener val
    Aff.delay $ Milliseconds 2000.0
  pure emitter
