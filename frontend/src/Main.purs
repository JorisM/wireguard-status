module Main
  ( main
  ) where

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
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Data.Array (filter, sortBy, reverse)
import Effect.Class.Console (log)
import Data.Number as Number

data SortCriteria = ByName | ByTransfer

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

data FetchError = NetworkError String | DecodeError String

fetchPeers :: Aff (Either FetchError (Array Peer))
fetchPeers = do
  response <- AX.get driver json "/peers"
  case response of
    Left err -> do
      log $ "Network error: " <> AX.printError err
      pure $ Left $ NetworkError "Failed to fetch peers from the server."
    Right r -> case decodeJson r.body of
      Left decodeErr -> do
        log $ "JSON decode error: " <> show decodeErr
        pure $ Left $ DecodeError "Failed to decode the peer data."
      Right decoded -> pure $ Right decoded

data Action = Initialize | FetchAndUpdatePeers | SetSortCriteria SortCriteria | ToggleSortOrder

type State =
  { onlinePeers :: Array Peer
  , offlinePeers :: Array Peer
  , sortCriteria :: SortCriteria
  , sortOrderAsc :: Boolean
  , errorMessage :: Maybe String
  }

initialState :: forall t. t -> State
initialState _ = { onlinePeers: [], offlinePeers: [], sortCriteria: ByName, sortOrderAsc: true, errorMessage: Nothing }

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ class_ $ ClassName "min-h-screen bg-gray-100 flex flex-col items-center" ]
    [ HH.div [ class_ $ ClassName "w-full max-w-4xl mt-10 p-5 bg-white shadow-md rounded-lg" ]
        [ HH.h2 [ class_ $ ClassName "text-2xl font-semibold text-gray-800 mb-5" ] [ HH.text "Online Peers" ]
        , renderError state.errorMessage
        , HH.div []
            (map peerHtml (sortPeers state.sortCriteria state.sortOrderAsc state.onlinePeers))
        , HH.h2 [ class_ $ ClassName "text-2xl font-semibold text-gray-800 mt-10 mb-5" ] [ HH.text "Offline Peers" ]
        , HH.div []
            (map peerHtml (sortPeers state.sortCriteria state.sortOrderAsc state.offlinePeers))
        , HH.div [ class_ $ ClassName "mt-5" ]
            [ HH.button [ class_ $ ClassName "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600", onClick \_ -> SetSortCriteria ByName ] [ HH.text "Sort by Name" ]
            , HH.button [ class_ $ ClassName "ml-2 px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600", onClick \_ -> SetSortCriteria ByTransfer ] [ HH.text "Sort by Transfer Size" ]
            , HH.button [ class_ $ ClassName "ml-2 px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600", onClick \_ -> ToggleSortOrder ] [ HH.text "Toggle Sort Order" ]
            ]
        ]
    ]

renderError :: forall m. Maybe String -> HH.ComponentHTML Action () m
renderError Nothing = HH.div [] []
renderError (Just msg) =
  HH.div [ class_ $ ClassName "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative mb-4" ]
    [ HH.span [ class_ $ ClassName "block sm:inline" ] [ HH.text msg ] ]

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
    fetchAndUpdatePeers
    pure unit
  FetchAndUpdatePeers ->
    fetchAndUpdatePeers
  ToggleSortOrder -> do
    H.modify_ \state -> state { sortOrderAsc = not state.sortOrderAsc }
  SetSortCriteria criteria -> do
    H.modify_ \state -> state { sortCriteria = criteria }

  where
  fetchAndUpdatePeers = do
    result <- liftAff fetchPeers
    case result of
      Left err -> do
        let
          errorMsg = case err of
            NetworkError msg -> msg
            DecodeError msg -> msg
        H.modify_ \state -> state { errorMessage = Just errorMsg }
      Right peers -> do
        let onlinePeers = filter (\p -> p.data.status == "Online") peers
        let offlinePeers = filter (\p -> p.data.status == "Offline") peers
        H.modify_ \state -> state { onlinePeers = onlinePeers, offlinePeers = offlinePeers, errorMessage = Nothing }

sortPeers :: SortCriteria -> Boolean -> Array Peer -> Array Peer
sortPeers criteria asc peers =
  let
    sortedPeers = case criteria of
      ByName -> sortBy (comparing _.name) peers
      ByTransfer -> sortBy (comparing (getTransferSize <<< _.data.sent)) peers
  in
    if asc then sortedPeers else reverse sortedPeers

getTransferSize :: DataWithUnit -> Number
getTransferSize { amount, unit } =
  case Number.fromString amount of
    Just num -> num * case unit of
      "B" -> 1.0
      "KiB" -> 1024.0
      "MiB" -> 1024.0 * 1024.0
      "GiB" -> 1024.0 * 1024.0 * 1024.0
      _ -> 0.0
    Nothing -> 0.0

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    H.liftEffect $ HS.notify listener val
    Aff.delay $ Milliseconds 4000.0
  pure emitter
