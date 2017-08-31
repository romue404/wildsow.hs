module Model.Step (stepGame) where
import Model.Validation
import Model.Evaluation (evaluateSubRound, evaluateRound)
import Model.Updates(cardPlayedUpdate, tricksPlayerUpdate, updatePlayer,
  addPlayers, waitForColor,dealCards, waitForNextCard, waitForNextTricks,
  setNewTrump, clearPlayedCards, cardsPerRound,nextPlayer, reevaluatePlayersTurn,
  replaceBotWithPlayer, replaceHumanPlayerWithBot,clearCurrentColor, waitForWinnerToPlayCard)
import Model.Bots(botMove)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Model.Model as Model
import Data.Maybe (isNothing)
import Control.Applicative

---------- SINGLE FUNCTION TO CALL ----------
stepGame :: PlayerMove -> GameState -> Either PlayerMoveError GameState
stepGame move gameState = stepWhileBot $ step' move gameState -- bot hier ueberpruefen

-- TODO check if player is in game,
update :: PlayerMove -> GameState -> Either PlayerMoveError GameState
update move gs@GameState{phase=p,  playerStates=players} =
  case move of
    (PlayCard plr card) -> do
      (enoughPlayers players) `mustHoldOr` NotEnoughPlayers
      (isWaitingForCards p) `mustHoldOr` UnexpectedMove
      (isPlayersTurn plr p) `mustHoldOr` NotPlayersTurn
      let gs' = gs{currentColor=(Model.currentColor gs) <|> Just(Model.color card)}
      (card `elem` playeableCards plr gs') `mustHoldOr` MoveAgainstRules "You are not allowed to play this card"
      Right gs'{playerStates = cardPlayedUpdate card plr $ Model.playerStates gs}
    (TellNumberOfTricks plr tricks) -> do
      (enoughPlayers players) `mustHoldOr` NotEnoughPlayers
      (isWaitingForTricks p) `mustHoldOr` UnexpectedMove
      (isPlayersTurn plr p) `mustHoldOr` NotPlayersTurn
      (tricks >= 0) `mustHoldOr` (MoveAgainstRules "Tricks must be >= 0")
      Right $ gs{playerStates = tricksPlayerUpdate tricks plr $ Model.playerStates gs}
    (Join player) -> do
      (playerNameIsFree (playerName player) players) `mustHoldOr` NameTaken
      (loginLogic player gs)
    (Leave player) -> do
      Right $ reevaluatePlayersTurn $
        gs{playerStates= (replaceHumanPlayerWithBot player (Model.RandomBot $ Model.playerName player) players)} -- Adds a bot with the same name


step :: GameState -> GameState
step gs@GameState {phase = Idle, playerStates=players}
  |enoughPlayers players = (waitForNextTricks . setNewTrump . dealCards) gs
  |otherwise = gs
step gs@GameState {phase = WaitingForTricks p}
  |allTricksSet gs = waitForNextCard gs
  |otherwise = waitForNextTricks gs
step gs@GameState {phase = WaitingForCard p}
  |everyPlayerPlayed gs = step gs{phase=Evaluation}
  |otherwise = waitForNextCard gs
step gs@GameState {phase = Evaluation, currentRound=round}
  |not $ allHandsPlayed gs = (clearCurrentColor . clearPlayedCards . setNewTrump . waitForWinnerToPlayCard winner) subroundEvaluatedGame -- TODO winner should start
  |round >= length  (cardsPerRound Model.deck $ length $ playerStates gs) = evaluateRound subroundEvaluatedGame{phase=GameOver}
  |otherwise = (waitForNextTricks . clearCurrentColor . clearPlayedCards . setNewTrump . dealCards . evaluateRound) subroundEvaluatedGame
  where (subroundEvaluatedGame, winner) = evaluateSubRound gs
  --  new round means we have to change the player twice
step gs@GameState {phase = GameOver} = gs


stepWhileBot :: Either PlayerMoveError GameState -> Either PlayerMoveError GameState
stepWhileBot rgs@(Right gs) = case botMove gs of
                          Nothing -> rgs
                          Just m -> stepWhileBot (step' m gs)
stepWhileBot err@(Left e) = err


step' :: PlayerMove -> GameState -> Either PlayerMoveError GameState
step' (PlayCard player card) gs@GameState{phase = phase@(WaitingForCard player'),playerStates=players} = do
      (enoughPlayers players) `mustHoldOr` NotEnoughPlayers
      (isPlayersTurn player phase) `mustHoldOr` NotPlayersTurn
      let gs' = gs{currentColor=(Model.currentColor gs) <|> Just(Model.color card)}
      (card `elem` playeableCards player gs') `mustHoldOr` MoveAgainstRules "You are not allowed to play this card"
      let state = gs'{playerStates = cardPlayedUpdate card player $ Model.playerStates gs}
      if everyPlayerPlayed state then Right (eval state) else Right (waitForNextCard state)
step' (TellNumberOfTricks player tricks) gs@GameState{phase= phase@(WaitingForTricks player'), playerStates=players} = do
      (enoughPlayers players) `mustHoldOr` NotEnoughPlayers
      (isPlayersTurn player phase) `mustHoldOr` NotPlayersTurn
      (tricks >= 0) `mustHoldOr` (MoveAgainstRules "Tricks must be >= 0")
      let state =  gs{playerStates = tricksPlayerUpdate tricks player $ Model.playerStates gs}
      if allTricksSet state then Right $ waitForNextCard state else Right $ waitForNextTricks state
step' (Join player) gs = (loginLogic player gs)
step' (Leave player) gs = Right $ reevaluatePlayersTurn $ gs{playerStates= (replaceHumanPlayerWithBot player (Model.RandomBot $ Model.playerName player) (playerStates gs))}
step' Begin gs@GameState{phase = Idle, playerStates=players}
      |enoughPlayers players = Right $ (waitForNextTricks . setNewTrump . dealCards) gs
      |otherwise = Left NotEnoughPlayers
step' _ gs@GameState {phase = GameOver} = Right gs
step' move gs = Left UnexpectedMove



eval gs@GameState {currentRound=round}
  |not $ allHandsPlayed gs = (clearCurrentColor . clearPlayedCards . waitForWinnerToPlayCard winner) subroundEvaluatedGame -- TODO winner should start
  |round >= length  (cardsPerRound Model.deck $ length $ playerStates gs) = evaluateRound subroundEvaluatedGame{phase=GameOver}
  |otherwise = (waitForNextTricks . clearCurrentColor . clearPlayedCards . setNewTrump . dealCards . evaluateRound) subroundEvaluatedGame
  where (subroundEvaluatedGame, winner) = evaluateSubRound gs


loginLogic :: Player -> GameState ->  Either Model.PlayerMoveError Model.GameState
loginLogic p gs@GameState{phase=Idle, playerStates=ps}
  |length ps < maxAmountOfPlayers = Right (addPlayers [p] gs)
  |amountOfBots ps > 0 = Right (reevaluatePlayersTurn $ gs{playerStates= (p `replaceBotWithPlayer` ps)})
  |otherwise = Left GameFull
loginLogic p gs@GameState{playerStates=ps}
  |amountOfBots ps > 0 = Right (reevaluatePlayersTurn $ gs{playerStates= (p `replaceBotWithPlayer` ps)})
  |otherwise = Left GameFull