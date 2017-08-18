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

data GameNetworkingException = EntityDoesNotExist | EntityAlreadyExists | ChannelFull | NotAMember | ParseError | GameError GameModel.PlayerMoveError deriving (Typeable)
instance Exception GameNetworkingException
instance Show GameNetworkingException where
  show EntityDoesNotExist = "The requested entity does not exist"
  show EntityAlreadyExists = "The requested entity already exists"
  show ChannelFull = "The channel you are trying to access is full"
  show NotAMember = "You are not a member of this channel"
  show ParseError = "The message you sent could not be parsed"
  show (GameError ge) = "Game Error: " `mappend` show ge

joinChannel :: Ord id =>  GameModel.Player -> id -> GameChannels id -> GameChannels id
joinChannel player id channels = Map.adjust (\gc@GameChannel{connectedPlayers = players, gameState=gameState}->
  gc{connectedPlayers = player:players, gameState = GameModelUpdates.addPlayers [player] gameState}) id channels

leaveChannel ::  Ord id =>  GameModel.Player -> id -> GameChannels id -> GameChannels id
leaveChannel player id channels = Map.adjust (\gc@GameChannel{connectedPlayers = players} -> gc{connectedPlayers = delete player players}) id channels

getChannel :: Ord id => id -> GameChannels id -> Maybe GameChannel
getChannel id channels = Map.lookup id channels

getChannel' :: Ord id => id -> GameChannels id -> Either GameNetworkingException GameChannel
getChannel' id channels = case (Map.lookup id channels) of
                          (Just channel) -> Right channel
                          (Nothing) -> Left EntityDoesNotExist

isMemberOfChannel :: Ord id => GameModel.Player -> id -> GameChannels id -> Bool
isMemberOfChannel player id channels = fromMaybe False $ isIn <$> Just player <*> (connectedPlayers <$> Map.lookup id channels)

channelExists :: Ord id => id -> GameChannels id -> Bool
channelExists id channels = isJust $ Map.lookup id channels

playersInChannel :: GameChannel -> Int
playersInChannel channel = length $ GameModel.players $ gameState channel
--fromMaybe 0 ((length . connectedPlayers) <$> getChannel id channels)

checkChannelLimit :: Int -> GameChannel -> Either GameNetworkingException GameChannel
checkChannelLimit limit channel
  | playersInChannel channel >= limit = Left ChannelFull
  | otherwise = Right channel

createChannel ::  Ord id =>  GameModel.Player -> id -> StdGen-> GameChannels id ->  (GameChannels id, GameChannel)
createChannel  player id gen channels  =
  let newChannel = (GameChannel [player] $ GameModelUpdates.addPlayers [player] $ GameModelUpdates.initWildsowGameState gen)
  in (Map.insert id newChannel channels, newChannel)

stepGameInChannel ::  Ord id => GameModel.GameState -> id -> GameChannels id ->  GameChannels id
stepGameInChannel state' gameId channels = Map.adjust (\gc@GameChannel{gameState = state} -> gc{gameState = state'}) gameId channels

isIn :: (Eq a) => a -> [a] -> Bool
isIn a as = any (== a) as