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
import Data.Array (filter, sortBy, reverse, length)
import Effect.Class.Console (log)
import Data.Number as Number

-- | Sort criteria for peers
data SortCriteria = ByName | ByTransfer

instance Eq SortCriteria where
  eq ByName ByName = true
  eq ByTransfer ByTransfer = true
  eq _ _ = false

-- | Data with unit (like "10 KiB")
type DataWithUnit =
  { amount :: String
  , unit :: String
  }

-- | Peer connection data
type PeerData =
  { status :: String
  , received :: DataWithUnit
  , sent :: DataWithUnit
  }

-- | Peer information
type Peer =
  { name :: String
  , data :: PeerData
  }

-- | Possible fetch errors
data FetchError = NetworkError String | DecodeError String

-- | Fetch peers from the API
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

-- | UI actions
data Action = Initialize | FetchAndUpdatePeers | SetSortCriteria SortCriteria | ToggleSortOrder | DismissError

-- | Application state
type State =
  { onlinePeers :: Array Peer
  , offlinePeers :: Array Peer
  , sortCriteria :: SortCriteria
  , sortOrderAsc :: Boolean
  , errorMessage :: Maybe String
  , isRefreshing :: Boolean
  , lastUpdated :: Maybe String
  }

-- | Initial state
initialState :: forall t. t -> State
initialState _ = 
  { onlinePeers: []
  , offlinePeers: []
  , sortCriteria: ByName
  , sortOrderAsc: true
  , errorMessage: Nothing
  , isRefreshing: false
  , lastUpdated: Nothing
  }

-- | Halogen component
component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

-- | Main render function
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ class_ $ ClassName "min-h-screen bg-slate-50 font-sans" ]
    [ HH.div [ class_ $ ClassName "max-w-6xl mx-auto px-4 py-8" ]
        [ renderHeader state
        , renderError state.errorMessage
        , HH.div [ class_ $ ClassName "grid grid-cols-1 md:grid-cols-2 gap-6 mb-8" ]
            [ renderPeerSection "Online Peers" (length state.onlinePeers) "bg-emerald-500" (sortPeers state.sortCriteria state.sortOrderAsc state.onlinePeers)
            , renderPeerSection "Offline Peers" (length state.offlinePeers) "bg-slate-500" (sortPeers state.sortCriteria state.sortOrderAsc state.offlinePeers)
            ]
        , renderControls state
        ]
    ]

-- | Render the header with app title and refresh indicator
renderHeader :: forall m. State -> H.ComponentHTML Action () m
renderHeader state =
  HH.div [ class_ $ ClassName "flex justify-between items-center mb-6" ]
    [ HH.h1 [ class_ $ ClassName "text-3xl font-bold text-slate-800" ] 
        [ HH.text "Peer Monitor" ]
    , HH.div [ class_ $ ClassName "flex items-center" ]
        [ HH.div [ class_ $ ClassName "text-sm text-slate-500 mr-3" ] 
            [ HH.text $ case state.lastUpdated of
                Nothing -> "Never updated"
                Just time -> "Last updated: " <> time
            ]
        , HH.div [ class_ $ ClassName if state.isRefreshing then "animate-spin text-blue-500" else "text-slate-400" ]
            [ HH.span [ class_ $ ClassName "inline-block w-5 h-5 border-2 border-current border-t-transparent rounded-full" ] []
            ]
        ]
    ]

-- | Render error message if there is one
renderError :: forall m. Maybe String -> H.ComponentHTML Action () m
renderError Nothing = HH.div [] []
renderError (Just msg) =
  HH.div [ class_ $ ClassName "bg-red-50 border-l-4 border-red-500 p-4 rounded-md mb-6 flex justify-between items-center" ]
    [ HH.span [ class_ $ ClassName "text-red-700" ] [ HH.text msg ]
    , HH.button 
        [ class_ $ ClassName "text-red-500 hover:text-red-700 focus:outline-none"
        , onClick \_ -> DismissError 
        ]
        [ HH.text "×" ]
    ]

-- | Render a section of peers (online or offline)
renderPeerSection :: forall m. String -> Int -> String -> Array Peer -> H.ComponentHTML Action () m
renderPeerSection title count accentColor peers =
  HH.div [ class_ $ ClassName "bg-white shadow-md rounded-xl overflow-hidden" ]
    [ HH.div 
        [ class_ $ ClassName $ accentColor <> " px-6 py-4 flex justify-between items-center" ]
        [ HH.h2 [ class_ $ ClassName "text-xl font-semibold text-white" ] 
            [ HH.text title ]
        , HH.span [ class_ $ ClassName "bg-white bg-opacity-20 text-white rounded-full px-3 py-1 text-sm" ]
            [ HH.text $ show count ]
        ]
    , if length peers == 0 
        then renderEmptyState
        else HH.div [ class_ $ ClassName "divide-y divide-slate-100" ] (map peerHtml peers)
    ]

-- | Render empty state when no peers are available
renderEmptyState :: forall m. H.ComponentHTML Action () m
renderEmptyState =
  HH.div [ class_ $ ClassName "flex flex-col items-center justify-center py-8 px-4 text-center" ]
    [ HH.div [ class_ $ ClassName "text-slate-300 text-5xl mb-3" ] [ HH.text "⦻" ]
    , HH.p [ class_ $ ClassName "text-slate-500" ] [ HH.text "No peers available" ]
    ]

-- | Render a single peer card
peerHtml :: forall m. Peer -> H.ComponentHTML Action () m
peerHtml peer =
  HH.div [ class_ $ ClassName "p-5 hover:bg-slate-50 transition-colors" ]
    [ HH.div [ class_ $ ClassName "flex justify-between items-start mb-3" ]
        [ HH.h3 [ class_ $ ClassName "text-lg font-medium text-slate-900" ] 
            [ HH.text peer.name ]
        , renderStatusBadge peer.data.status
        ]
    , HH.div [ class_ $ ClassName "grid grid-cols-2 gap-4 mt-4" ]
        [ renderDataCard "Received" "bg-blue-50 text-blue-700" peer.data.received
        , renderDataCard "Sent" "bg-purple-50 text-purple-700" peer.data.sent
        ]
    ]

-- | Render a status badge
renderStatusBadge :: forall m. String -> H.ComponentHTML Action () m
renderStatusBadge status =
  let
    bgColor = if status == "Online" 
      then "bg-emerald-100"
      else "bg-slate-100"
    textColor = if status == "Online" 
      then "text-emerald-800"
      else "text-slate-800"
  in
    HH.span [ class_ $ ClassName $ "px-2 py-1 rounded-full text-xs font-medium " <> bgColor <> " " <> textColor ] 
      [ HH.text status ]

-- | Render a data card (received or sent)
renderDataCard :: forall m. String -> String -> DataWithUnit -> H.ComponentHTML Action () m
renderDataCard label styles data_ =
  HH.div [ class_ $ ClassName $ "rounded-md p-3 " <> styles ]
    [ HH.p [ class_ $ ClassName "text-xs mb-1 opacity-80" ] [ HH.text label ]
    , HH.p [ class_ $ ClassName "text-lg font-medium" ] 
        [ HH.text $ data_.amount <> " " <> data_.unit ]
    ]

-- | Render the sorting controls
renderControls :: forall m. State -> H.ComponentHTML Action () m
renderControls state =
  HH.div [ class_ $ ClassName "flex flex-wrap justify-center gap-3 mt-6" ]
    [ renderSortButton "Sort by Name" (state.sortCriteria == ByName) (SetSortCriteria ByName)
    , renderSortButton "Sort by Transfer" (state.sortCriteria == ByTransfer) (SetSortCriteria ByTransfer)
    , HH.button 
        [ class_ $ ClassName "inline-flex items-center px-4 py-2 bg-white border border-slate-300 rounded-md shadow-sm text-sm font-medium text-slate-700 hover:bg-slate-50"
        , onClick \_ -> ToggleSortOrder 
        ]
        [ HH.span [ class_ $ ClassName "mr-2" ] [ HH.text "Order:" ]
        , HH.span [] [ HH.text if state.sortOrderAsc then "Ascending" else "Descending" ]
        , HH.span [ class_ $ ClassName "ml-2" ] [ HH.text if state.sortOrderAsc then "↑" else "↓" ]
        ]
    ]

-- | Render a sort button
renderSortButton :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action () m
renderSortButton label isActive action =
  HH.button 
    [ class_ $ ClassName $ "px-4 py-2 rounded-md text-sm font-medium " <> 
        if isActive 
          then "bg-blue-500 text-white shadow-sm" 
          else "bg-white text-slate-700 border border-slate-300 shadow-sm hover:bg-slate-50"
    , onClick \_ -> action 
    ]
    [ HH.text label ]

-- | Handle actions
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
  
  DismissError -> do
    H.modify_ \state -> state { errorMessage = Nothing }

  where
  fetchAndUpdatePeers = do
    -- Set refreshing state to show spinner
    H.modify_ \state -> state { isRefreshing = true }
    
    -- Format current time for last updated
    now <- liftAff $ H.liftEffect $ getCurrentTimeString
    
    -- Fetch peer data
    result <- liftAff fetchPeers
    
    case result of
      Left err -> do
        let
          errorMsg = case err of
            NetworkError msg -> msg
            DecodeError msg -> msg
        H.modify_ \state -> state 
          { errorMessage = Just errorMsg
          , isRefreshing = false
          , lastUpdated = Just now
          }
      Right peers -> do
        let onlinePeers = filter (\p -> p.data.status == "Online") peers
        let offlinePeers = filter (\p -> p.data.status == "Offline") peers
        H.modify_ \state -> state 
          { onlinePeers = onlinePeers
          , offlinePeers = offlinePeers
          , errorMessage = Nothing
          , isRefreshing = false
          , lastUpdated = Just now
          }

-- | Sort peers based on criteria and order
sortPeers :: SortCriteria -> Boolean -> Array Peer -> Array Peer
sortPeers criteria asc peers =
  let
    sortedPeers = case criteria of
      ByName -> sortBy (comparing _.name) peers
      ByTransfer -> sortBy (comparing (getTransferSize <<< _.data.sent)) peers
  in
    if asc then sortedPeers else reverse sortedPeers

-- | Calculate transfer size in bytes for comparison
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

-- | Get current time as a formatted string (this is a mock implementation)
getCurrentTimeString :: Effect String
getCurrentTimeString = pure "just now" -- In a real app, use Effect.Now and format properly

-- | Main entry point
main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

-- | Setup timer for polling
timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    H.liftEffect $ HS.notify listener val
    Aff.delay $ Milliseconds 4000.0
  pure emitter