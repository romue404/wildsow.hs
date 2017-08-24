{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveAnyClass , FlexibleContexts #-}
module GameSocket where

import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Exception (finally, catch, throw, handle, SomeException, Exception)
import Control.Monad (forever, forM_, when, unless)
import qualified Network.WebSockets as WS
import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Aeson
import Data.Maybe
import Control.Applicative
import qualified Model as GameModel
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import qualified ModelUpdates as GameUpdates
import System.Random
import qualified NetworkManagement
import Data.Either.Combinators
import Control.Monad.Except (MonadError, throwError)
import Data.Typeable
import Types


----------------------------------------------------- COMMUNICATION DATA -----------------------------------------------------
type GameId = String
type Games = NetworkManagement.GameChannels GameId
data Client = Client {name::String}  deriving (Eq, Show)
data ClientMessage =
  Join {userName::String, gameId::String} | Create {userName::String, gameId::String} |GameAction GameId GameModel.PlayerMove
  deriving (Show)

instance PlayerAction ClientMessage where
  whos (Join userName _) = userName
  whos (Create userName _) = userName
  whos (GameAction id ga) = whos ga

deriveJSON defaultOptions ''Client -- template haskell
instance FromJSON ClientMessage where
  parseJSON = withObject "client message" $ \o -> do
    kind <- o .: "kind"
    userName <- o.: "userName"
    gameId <- o.: "gameId"
    case kind of
      "join"  -> do return $ GameAction gameId $ GameModel.Join userName
      "create"-> do return $ Create userName gameId
      "tellNumberOfTricks" -> do
        tricks <- o.: "tricks"
        return $ GameAction gameId $ GameModel.TellNumberOfTricks userName tricks
      "playCard" -> do
        card <- o.: "card"
        return $ GameAction gameId $ GameModel.PlayCard userName card
      "tellColor" -> do
        color <-  o.: "color"
        return $ GameAction gameId $ GameModel.TellColor userName color
      _        -> fail ("unknown kind: " ++ kind)

----------------------------------------------------- GAME SOCKET -----------------------------------------------------

gameSocket :: IO()
gameSocket = do
  games <-  atomically $ newTVar (Map.empty:: Games)
  WS.runServer "127.0.0.1" 5000 $ app games

app :: TVar (NetworkManagement.GameChannels GameId) -> WS.PendingConnection ->  IO ()
app games pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30 -- ensure the connection stays alive
  forever $ handle (errorHandler conn) (do
    msg <- WS.receiveData conn
    action <- maybe (throw NetworkManagement.ParseError) pure (decode(msg)::Maybe ClientMessage)
    res <- case action of
      Join userName gameId -> do
        joined <- atomically $ loginSTM gameId games ((GameModel.HumanPlayer userName), conn)
        either (throw) (pure) joined
      Create userName gameId -> do
        gen <- newStdGen
        created <- (atomically $ createSTM gameId games (GameModel.HumanPlayer userName, conn) gen)
        channel <- either (throw) (pure) created
        -- hier gamestate verschicken bzw in die 2. phase wechseln
        broadcastState gameId games (NetworkManagement.gameState channel)
      (GameAction id move) -> do
        possibleAction <- atomically $ gameActionSTM id games (GameModel.HumanPlayer $ whos move) move
        state <- either (throw) (pure) possibleAction
        broadcastState id games state
    -- hier alles parsen und action behandlung machen
    --WS.sendTextData conn ("Parsed:" `mappend` (T.pack(show $ action))::Text)
    return res)


----------------------------------------------------- PERSIST ACTIONS VIA STM -----------------------------------------------------

loginSTM ::
  (Ord id) => id
  -> TVar(NetworkManagement.GameChannels id)
  -> (GameModel.Player, WS.Connection)
  -> STM(Either NetworkManagement.GameNetworkingException ())
loginSTM id channels player = do
  games <- readTVar channels
  let checks = (NetworkManagement.getChannel' id games) >>= NetworkManagement.checkChannelLimit GameModel.maxAmountOfPlayers
  case checks of
    Left error -> return $ Left error
    Right channel -> (writeTVar channels $ (NetworkManagement.joinChannel player id games)) >> (return $ Right ())

createSTM ::
  (Ord id) => id
  -> TVar(NetworkManagement.GameChannels id)
  -> (GameModel.Player, WS.Connection)
  -> StdGen
  -> STM(Either NetworkManagement.GameNetworkingException NetworkManagement.GameChannel)
createSTM id channels player gen = do
  games <- readTVar channels
  case swapEither(NetworkManagement.getChannel' id games) of
    Left _ -> return $ Left NetworkManagement.EntityAlreadyExists
    Right _ -> (
      let (updatedChannels, newChannel) = (NetworkManagement.createChannel player id gen games)
      in do
        writeTVar channels $ updatedChannels
        return $ Right newChannel
      )

gameActionSTM ::
  (Ord id) => id
  -> TVar(NetworkManagement.GameChannels id)
  -> GameModel.Player
  -> GameModel.PlayerMove
  -> STM(Either NetworkManagement.GameNetworkingException GameModel.GameState)
gameActionSTM gameId channels player move = do
  games <- readTVar channels
  let eihterNewState = do
            (NetworkManagement.GameChannel players state) <- NetworkManagement.getChannel' gameId games
            mapEitherR (\r -> NetworkManagement.GameError r) (GameUpdates.moveValidataion move state)
  case eihterNewState of
    Left err -> return $ Left err
    Right newState -> do
      writeTVar channels $ NetworkManagement.stepGameInChannel newState gameId games
      return $ Right (newState)

-- TODO fetch gamestate inside stm
broadcastState id channels state =
  do
    recieivers <- atomically $ do
        game <- readTVar channels
        let receiver = fromMaybe [] $ (fmap snd) <$> NetworkManagement.connectedPlayers <$> NetworkManagement.getChannel id game
        return receiver
    cast <- forM_ recieivers (\conn -> WS.sendTextData conn (Data.Aeson.encode(state)))
    return ()

----------------------------------------------------- COMMUNICATE MESSAGES TO CLIENTS -----------------------------------------------------

errorHandler :: WS.Connection -> NetworkManagement.GameNetworkingException -> IO ()
errorHandler conn e = WS.sendTextData conn (Data.Aeson.encode(e))


----------------------------------------------------- HELPER -----------------------------------------------------
mapEitherR f (Left error) = Left(f error)
mapEitherR f (Right r) = Right(r)

assertTrue :: MonadError e m => Bool -> e -> m ()
assertTrue x err = if x then pure () else throwError err

assertJust :: MonadError e m => Maybe a -> e -> m a
assertJust x err = maybe (throwError err) pure x

assertNothing :: MonadError e m => Maybe a -> (a -> e) -> m ()
assertNothing x toErr = maybe (pure ()) (throwError . toErr) x

assertLeft :: MonadError e m => Either a e -> m a
assertLeft = either pure throwError

assertRight :: MonadError e m => Either a b -> (a -> e) -> m b
assertRight x toErr = either (throwError . toErr) pure x