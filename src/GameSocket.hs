{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveAnyClass  #-}
module GameSocket where

import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import qualified Network.WebSockets as WS
import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Aeson as Aeson(encode, decode)
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe

type ServerState = [ClientWithConnection]
type ClientWithConnection = (Client, WS.Connection)

data Client = Client{name::Text, gameId::Text} deriving (Eq, Show)
deriveJSON defaultOptions ''Client -- template haskell


gameSocket :: IO()
gameSocket = do
  socketState <- newMVar []
  WS.runServer "127.0.0.1" 8080 $ application socketState


application :: MVar ServerState -> WS.PendingConnection -> IO()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30 -- ensure the connection stays alive
  forever $ do
    msg <- WS.receiveData conn
    clients <- readMVar state
    let maybeClient = Aeson.decode(msg)::Maybe Client
    maybe (WS.sendTextData conn ("INVALID MESSAGE" :: Text)) (\client -> case msg of
      _| clientExists client clients -> WS.sendTextData conn ("User already exists" :: Text)
       | otherwise -> flip finally disconnect $ do
        modifyMVar_ state $ \s -> do
          let s' = addClient (client, conn) s
          WS.sendTextData conn ("HELLO FROM THE OTHER SIDE" :: Text)
          broadcast (name client `mappend` " joined") s'
          return s'
        talk conn state (client, conn)
       where
        disconnect = do
          s <- modifyMVar state $ \s ->
            let s' = removeClient (client, conn) s in return (s', s')
          broadcast (name client `mappend` " disconnected") s) maybeClient




talk :: WS.Connection -> MVar ServerState -> ClientWithConnection -> IO ()
talk conn state (Client userName gameId, _) = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast
    (userName `mappend` ": " `mappend` msg)

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((==  client) . fst)

addClient :: ClientWithConnection -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: ClientWithConnection -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
   T.putStrLn message
   forM_ clients $ \(_, conn) -> WS.sendTextData conn message

numClients :: ServerState -> Int
numClients = length