module NetworkManagement where
import qualified Model as GameModel
import qualified ModelUpdates as GameModelUpdates
import qualified Data.Map.Strict as Map
import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Maybe
import Data.List
import Data.Typeable
import Control.Exception
import System.Random


data GameChannel = GameChannel {connectedPlayers:: [GameModel.Player], gameState :: GameModel.GameState}
type GameChannels id = Map.Map id GameChannel

data GameNetworkingException = EntityDoesNotExist | ChannelFull | NotAMember | ParseError | GameError GameModel.PlayerMoveError deriving (Show, Typeable)
instance Exception GameNetworkingException

joinChannel :: Ord id =>  GameModel.Player -> id -> GameChannels id -> GameChannels id
joinChannel player id channels = Map.adjust (\gc@GameChannel{connectedPlayers = players, gameState=gameState}->
  gc{connectedPlayers = player:players, gameState = GameModelUpdates.addPlayers [player] gameState}) id channels

leaveChannel ::  Ord id =>  GameModel.Player -> id -> GameChannels id -> GameChannels id
leaveChannel player id channels = Map.adjust (\gc@GameChannel{connectedPlayers = players} -> gc{connectedPlayers = delete player players}) id channels

getChannel :: Ord id => id -> GameChannels id -> Maybe GameChannel
getChannel id channels = Map.lookup id channels

isMemberOfChannel :: Ord id => GameModel.Player -> id -> GameChannels id -> Bool
isMemberOfChannel player id channels = fromMaybe False $ isIn <$> Just player <*> (connectedPlayers <$> Map.lookup id channels)

channelExists :: Ord id => id -> GameChannels id -> Bool
channelExists id channels = isJust $ Map.lookup id channels

playersInChannel :: Ord id => id -> GameChannels id -> Int
playersInChannel id channels = fromMaybe 0 ((length . connectedPlayers) <$> getChannel id channels)

createChannel ::  Ord id =>  GameModel.Player -> id -> StdGen-> GameChannels id ->  GameChannels id
createChannel  player id gen channels  = Map.insert id  (GameChannel [player] $ GameModelUpdates.initWildsowGameState gen) channels

stepGameInChannel ::  Ord id => GameModel.PlayerMove -> id -> GameChannels id ->  GameChannels id
stepGameInChannel move gameId channels = Map.adjust (\gc@GameChannel{gameState = state} ->
  gc{gameState = (GameModelUpdates.step . GameModelUpdates.processMove(move)) state}) gameId channels

isIn :: (Eq a) => a -> [a] -> Bool
isIn a as = any (== a) as