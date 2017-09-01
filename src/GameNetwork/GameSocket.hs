{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveAnyClass , FlexibleContexts #-}
module GameNetwork.GameSocket where

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
import qualified Model.Model as GameModel
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import qualified Model.Updates as GameUpdates
import System.Random
import qualified GameNetwork.NetworkManagement as NetworkManagement
import Data.Either.Combinators
import Control.Monad.Except (MonadError, throwError)
import Data.Typeable
import Model.Types
import Model.Step



----------------------------------------------------- COMMUNICATION DATA -----------------------------------------------------
type GameId = String
type Games = NetworkManagement.GameChannels GameId
data Client = Client {name::String}  deriving (Eq, Show)
data ClientMessage =
   Create {userName::String, gameId::String} |GameAction GameId GameModel.PlayerMove
  deriving (Show)

instance PlayerAction ClientMessage where
  whos (Create userName _) = userName
  whos (GameAction id ga) = whos ga

deriveJSON defaultOptions ''Client -- template haskell
instance FromJSON ClientMessage where
  parseJSON = withObject "client message" $ \o -> do
    kind <- o .: "kind"
    userName <- o.: "userName"
    gameId <- o.: "gameId"
    case kind of
      "join"  -> do
          botType <- o .: "botType"
          return $ if T.pack(botType) == "random" then GameAction gameId $ GameModel.Join (GameModel.RandomBot userName)
             else if T.pack(botType) == "smart" then  GameAction gameId $ GameModel.Join (GameModel.SmartBot userName)
             else GameAction gameId $ GameModel.Join (GameModel.HumanPlayer userName)
      "create"-> do return $ Create userName gameId
      "tellNumberOfTricks" -> do
        tricks <- o.: "tricks"
        return $ GameAction gameId $ GameModel.TellNumberOfTricks (GameModel.HumanPlayer userName) tricks
      "playCard" -> do
        card <- o.: "card"
        return $ GameAction gameId $ GameModel.PlayCard (GameModel.HumanPlayer userName) card
      "start" -> do return $ GameAction gameId $ GameModel.Begin
      _        -> fail ("unknown kind: " ++ kind)


----------------------------------------------------- GAME SOCKET -----------------------------------------------------

gameSocket :: IO()
gameSocket = do
  games <-  atomically $ newTVar (Map.empty:: Games)
  WS.runServer "127.0.0.1" 5000 $ app games

app :: TVar (NetworkManagement.GameChannels GameId) -> WS.PendingConnection ->  IO ()
app games pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 10 -- ensure the connection stays alive
  forever $ handle (errorHandler conn) (do
    msg <- WS.receiveData conn
    action <- maybe (throw NetworkManagement.ParseError) pure (decode(msg)::Maybe ClientMessage)
    res <- case action of
      (GameAction gameId move@(GameModel.Join player)) -> do
        joined <- atomically $ do
          chans <- readTVar games
          loggedIn <- gameActionSTM gameId games player move
          case loggedIn of
            Left e -> return $ Left e
            Right gs -> (writeTVar games $ (NetworkManagement.joinChannel (player, conn) gameId chans)) >> (return $ Right ())
        either (throw) (\_ -> gameLoop (conn, player) games gameId) joined -- joind change for gameloop
      Create userName gameId -> do
        gen <- newStdGen
        let player = GameModel.HumanPlayer userName
        created <- atomically $ createSTM gameId games (player, conn) gen
        either (throw) (\_ -> gameLoop (conn, player) games gameId) created
      _ -> throw NetworkManagement.MessageNotAllowed
    -- hier alles parsen und action behandlung machen
    --WS.sendTextData conn ("Parsed:" `mappend` (T.pack(show $ action))::Text)
    return res)


gameLoop :: ( WS.Connection, GameModel.Player) -> TVar (NetworkManagement.GameChannels GameId) -> GameId -> IO ()
gameLoop (conn, player) games gameId =
  do
    broadcastState gameId games
    flip finally(disconnectHandler gameId games player) $ forever $ do
      WS.forkPingThread conn 10
      print("listening for " `mappend` show player)
      msg <- WS.receiveData conn
      action <- pure (decode(msg)::Maybe ClientMessage)
      case action of
        Just (GameAction id  (GameModel.Join (GameModel.HumanPlayer p))) -> unicast conn $ GameModel.UnexpectedMove "You can only join one game" -- do nothing TODO check if player = player
        Just (GameAction id move) -> do
          possibleAction <- atomically $ gameActionSTM id games (GameModel.HumanPlayer $ whos move) move
          case possibleAction of
            Left e -> unicast conn e
            Right state ->  broadcastState id games
        Just e -> unicast conn $ GameModel.UnexpectedMove "Only game-actions are allowed"
        Nothing -> unicast conn $ NetworkManagement.ParseError

----------------------------------------------------- PERSIST ACTIONS VIA STM -----------------------------------------------------

loginSTM ::
  (Ord id) => id
  -> TVar(NetworkManagement.GameChannels id)
  -> (GameModel.Player, WS.Connection)
  -> STM(Either NetworkManagement.GameNetworkingException ())
loginSTM id channels player = do
  games <- readTVar channels
  case(NetworkManagement.getChannel' id games) of
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
            mapEitherR NetworkManagement.GameError (Model.Step.stepGame move state)
  case eihterNewState of
    Left err -> return $ Left err
    Right newState -> do
      writeTVar channels $ NetworkManagement.stepGameInChannel newState gameId games
      when ((GameModel.phase newState) == GameModel.GameOver) (modifyTVar channels $ Map.delete gameId)
      return $ Right (newState)


disconnectSTM id games player = do
  action <- gameActionSTM id games player (GameModel.Leave player)
  channel <- readTVar games
  writeTVar games $ NetworkManagement.leaveChannel player id channel


----------------------------------------------------- COMMUNICATE MESSAGES TO CLIENTS -----------------------------------------------------
disconnectHandler :: GameId -> TVar (NetworkManagement.GameChannels GameId) ->GameModel.Player -> IO ()
disconnectHandler id games player = do
  _ <- atomically $ disconnectSTM id games player
  print("DISCONNECTED: " `mappend` show player)
  broadcastState id games

discTest :: SomeException -> IO ()
discTest e = print(show e)

unicast conn msg =  WS.sendTextData conn (Data.Aeson.encode(msg))

errorHandler :: WS.Connection -> NetworkManagement.GameNetworkingException -> IO ()
errorHandler conn e = do
  print("handling error in main loop")
  WS.sendTextData conn (Data.Aeson.encode(e))

broadcastState :: (Ord id) => id -> TVar(NetworkManagement.GameChannels id) -> IO ()
broadcastState id channels =
  do
    stateReceivers <- atomically $ do
        game <- readTVar channels
        let sr = do
                    NetworkManagement.GameChannel{NetworkManagement.connectedPlayers=receivers,
                    NetworkManagement.gameState=state} <- NetworkManagement.getChannel id game
                    return (state, map snd receivers)
        return sr
    case stateReceivers of -- TODO or use when (...)
      Just (state, receivers) -> forM_ (receivers) (\conn -> WS.sendTextData conn (Data.Aeson.encode(state)))
      Nothing -> return ()

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