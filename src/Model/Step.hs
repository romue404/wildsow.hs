module Model.Step (stepGame) where
import Model.Validation
import Model.Evaluation (evaluateSubRound, evaluateRound)
import Model.Updates(cardPlayedUpdate, tricksPlayerUpdate, updatePlayer,
  addPlayers, waitForColor,dealCards, waitForNextCard, waitForNextTricks,
  setNewTrump, clearPlayedCards, cardsPerRound,nextPlayer, reevaluatePlayersTurn,
  replaceBotWithPlayer, replaceHumanPlayerWithBot)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Model.Model as Model
import Data.Maybe (isNothing)

---------- SINGLE FUNCTION TO CALL ----------
stepGame :: PlayerMove -> GameState -> Either PlayerMoveError GameState
stepGame move gameState = step <$> (update move gameState)

-- TODO check if player is in game,
update :: PlayerMove -> GameState -> Either PlayerMoveError GameState
update move gs@GameState{phase=p,  players=players} =
  case move of
    (PlayCard name card) -> do
      (enoughPlayers players) `mustHoldOr` NotEnoughPlayers
      (isWaitingForCards p) `mustHoldOr` UnexpectedMove
      (isPlayersTurn name p) `mustHoldOr` NotPlayersTurn
      (card `elem` playeableCards name gs) `mustHoldOr` MoveAgainstRules "You are not allowed to play this card"
      Right gs{players = cardPlayedUpdate card (HumanPlayer name) $ Model.players gs}
    (TellNumberOfTricks name tricks) -> do
      (enoughPlayers players) `mustHoldOr` NotEnoughPlayers
      (isWaitingForTricks p) `mustHoldOr` UnexpectedMove
      (isPlayersTurn name p) `mustHoldOr` NotPlayersTurn
      (tricks >= 0) `mustHoldOr` (MoveAgainstRules "Tricks must be >= 0")
      Right $ gs{players = tricksPlayerUpdate tricks (HumanPlayer name) $ Model.players gs}
    (TellColor name color) -> do
      (enoughPlayers players) `mustHoldOr` NotEnoughPlayers
      (isWaitingForColor p) `mustHoldOr` UnexpectedMove
      (isPlayersTurn name p) `mustHoldOr` NotPlayersTurn
      Right $ gs{currentColor=Just color}
    (Join player) -> do
      (playerNameIsFree (playerName player) players) `mustHoldOr` NameTaken
      (loginLogic player gs)
    (Leave player) -> do
      Right $ reevaluatePlayersTurn $
        gs{players= (replaceHumanPlayerWithBot player (Model.Ai $ Model.playerName player) players)} -- Adds a bot with the same name


step :: GameState -> GameState
step gs@GameState {phase = Idle, players=players}
  |enoughPlayers players = (waitForColor . setNewTrump . dealCards) gs
  |otherwise = gs
step gs@GameState {phase = WaitingForTricks p}
  |allTricksSet gs = waitForNextCard gs
  |otherwise = waitForNextTricks gs
step gs@GameState {phase = WaitingForCard p}
  |everyPlayerPlayed gs = step gs{phase=Evaluation}
  |otherwise = waitForNextCard gs
step gs@GameState {phase=WaitingForColor p} =
  case currentColor gs of
    Nothing -> gs
    Just c -> gs{phase = WaitingForTricks p}
step gs@GameState {phase = Evaluation, currentRound=round}
  |not $ allHandsPlayed gs = (clearPlayedCards . setNewTrump . waitForNextCard . evaluateSubRound) gs
  |round+1 >= length  (cardsPerRound Model.deck $ length $ players gs) = (evaluateRound. evaluateSubRound) gs{phase=GameOver}
  |otherwise = (waitForColor . clearPlayedCards . setNewTrump . dealCards . evaluateRound. evaluateSubRound) gs{players = nextPlayer $ players gs}
  --  new round means we have to change the player twice
step gs@GameState {phase = GameOver} = gs



loginLogic :: Player -> GameState ->  Either Model.PlayerMoveError Model.GameState
loginLogic p gs@GameState{phase=Idle, players=ps}
  |length ps < maxAmountOfPlayers = Right (addPlayers [p] gs)
  |amountOfBots ps > 0 = Right (reevaluatePlayersTurn $ gs{players= (p `replaceBotWithPlayer` ps)})
  |otherwise = Left GameFull
loginLogic p gs@GameState{players=ps}
  |amountOfBots ps > 0 = Right (reevaluatePlayersTurn $ gs{players= (p `replaceBotWithPlayer` ps)})
  |otherwise = Left GameFull