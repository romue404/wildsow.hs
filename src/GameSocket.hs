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

type GameId = String
type ServerState = [ClientWithConnection]
type ClientWithConnection = (Client, WS.Connection)
type Games = NetworkManagement.GameChannels GameId
data Client = Client {name::String}  deriving (Eq, Show)
data ClientMessage =
  Join {userName::String, gameId::String} | Create {userName::String, gameId::String} |GameAction GameId GameModel.PlayerMove
  deriving (Show)

deriveJSON defaultOptions ''Client -- template haskell
instance FromJSON ClientMessage where
  parseJSON = withObject "client message" $ \o -> do
    kind <- o .: "kind"
    userName <- o.: "userName"
    gameId <- o.: "gameId"
    case kind of
      "join"  -> do return $ Join userName gameId
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


gameSocket :: IO()
gameSocket = do
  socketState <- newMVar []
  games <-  atomically $ newTVar (Map.empty:: Games)
  WS.runServer "127.0.0.1" 8080 $ app socketState games

app :: MVar ServerState -> TVar (NetworkManagement.GameChannels GameId) -> WS.PendingConnection ->  IO ()
app state games pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30 -- ensure the connection stays alive
  forever $ handle (errorHandler conn) (do
    msg <- WS.receiveData conn
    clients <- readMVar state
    action <- maybe (throw NetworkManagement.ParseError) pure (decode(msg)::Maybe ClientMessage)
    -- hier alles parsen und action behandlung machen
    --let player = GameModel.Player name
    t <- atomically $ handleMessages games action
    x <- print(show t)
    gen <- newStdGen
    WS.sendTextData conn ("Parsed:" `mappend` (T.pack(show $ action))::Text))


--handleMessages :: TVar(Games) -> ClientMessage -> IO()
handleMessages gameChannels (Join userName gameId) =
  do
    games <- readTVar gameChannels
    if not (NetworkManagement.channelExists gameId games) then return $ Left NetworkManagement.EntityDoesNotExist
    else
     (modifyTVar gameChannels $ NetworkManagement.joinChannel (GameModel.Player userName) gameId)
     >> (return $ Right ())

handleMessages gameChannels (Create userName gameId) = undefined
handleMessages gameChannels (GameAction gameId action) =
  do
    games <- readTVar gameChannels
    let player = GameModel.Player (whos action)
    let checkNetwork = [(not(NetworkManagement.channelExists gameId games), NetworkManagement.EntityDoesNotExist),
                        ((NetworkManagement.isMemberOfChannel player gameId games), NetworkManagement.NotAMember)]
    let checkGameLogic =  fromMaybe [] $ do
                            channel <- NetworkManagement.getChannel gameId games
                            let validations = GameUpdates.moveValidataionPipeline action $ NetworkManagement.gameState channel
                            return $ map (\(a,b) -> (a, NetworkManagement.GameError b)) validations
    let combined = checkNetwork ++ checkGameLogic
    -- if isEmpty -> carry on and return gamechannel -> else return Left $ first error
    if length combined == 0 then modifyTVar gameChannels (\gc -> NetworkManagement.stepGameInChannel action gameId gc) >> (return $ Right ())
    else  return $ Left $ (snd . head) combined
    -- gameover detection
    -- closing connection behandeln


--check' predicates = fst map (== True)




errorHandler :: WS.Connection -> NetworkManagement.GameNetworkingException -> IO ()
errorHandler conn e = WS.sendTextData conn (T.pack(show e)::Text)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
   T.putStrLn message
   forM_ clients $ \(_, conn) -> WS.sendTextData conn message
























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