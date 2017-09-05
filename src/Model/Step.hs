module Model.Step (stepGame) where
import Model.Validation
import Model.Evaluation (evaluateSubRound, evaluateRound)
import Model.Updates(cardPlayedUpdate, tricksPlayerUpdate, updatePlayer,
  addPlayers, waitForColor,dealCards, waitForNextCard, waitForNextTricks,
  setNewTrump, clearPlayedCards, cardsPerRound,nextPlayer, reevaluatePlayersTurn,removePlayer,
  replaceBotWithPlayer, replaceHumanPlayerWithBot,clearCurrentColor, waitForWinnerToPlayCard,
  clearDiscardPile, updateDiscardPile)
import Model.Bots(botMove)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Model.Model as Model
import Data.Maybe (isNothing)
import Control.Applicative
import Model.Types


---------- SINGLE FUNCTION TO CALL ----------
stepGame :: PlayerMove -> GameState -> Either PlayerMoveError GameState
stepGame move gameState = stepWhileBot $ step' move gameState -- bot hier ueberpruefen

-- TODO separate gameStates for every move
stepWhileBot :: Either PlayerMoveError GameState -> Either PlayerMoveError GameState
stepWhileBot rgs@(Right gs@GameState{phase=GameOver}) = Right gs
stepWhileBot rgs@(Right gs) =  maybe rgs (stepWhileBot . (flip step' gs)) $ botMove gs
stepWhileBot err@(Left e) = err


step' :: PlayerMove -> GameState -> Either PlayerMoveError GameState
step' _ gs@GameState {phase = GameOver} = Left $ UnexpectedMove "The game has already finished, no more moves allowed"
step' (PlayCard player card) gs@GameState{phase = phase@(WaitingForCard player'),playerStates=players, discardPile=dp} = do
      (enoughPlayers players) `mustHoldOr` NotEnoughPlayers
      (isPlayersTurn player phase) `mustHoldOr` NotPlayersTurn
      let gs' = gs{currentColor=(Model.currentColor gs) <|> Just(Model.color card)}
      (card `elem` playeableCards player gs') `mustHoldOr` MoveAgainstRules "You are not allowed to play this card"
      let discardPile' =  take ((length players)*2) $ (playerName player, card):dp
      let state =  gs'{playerStates = cardPlayedUpdate card player $ Model.playerStates gs, discardPile = discardPile'}
      if everyPlayerPlayed state then Right (eval state) else Right (waitForNextCard state)
step' (TellNumberOfTricks player tricks) gs@GameState{phase= phase@(WaitingForTricks player'), playerStates=players} = do
      (enoughPlayers players) `mustHoldOr` NotEnoughPlayers
      (isPlayersTurn player phase) `mustHoldOr` NotPlayersTurn
      (tricks >= 0) `mustHoldOr` (MoveAgainstRules "Tricks must be >= 0")
      let state =  gs{playerStates = tricksPlayerUpdate tricks player $ Model.playerStates gs}
      if allTricksSet state then Right $ waitForNextCard state else Right $ waitForNextTricks state
step' (Join player) gs = (loginLogic player gs)
step' (Leave player) gs@GameState{phase = Idle} = Right $ reevaluatePlayersTurn  $ removePlayer player gs
step' (Leave player) gs = Right $ reevaluatePlayersTurn $ gs{playerStates=
        (replaceHumanPlayerWithBot player (Model.RandomBot $ Model.playerName player) (playerStates gs))}
step' Begin gs@GameState{phase = Idle, playerStates=players}
      |enoughPlayers players = Right $ (waitForNextTricks . setNewTrump . dealCards) gs
      |otherwise = Left NotEnoughPlayers
step' move gs = Left $ UnexpectedMove "The game does not expect you to make that move"


eval gs@GameState {currentRound=round}
  |not $ allHandsPlayed gs = (clearCurrentColor . clearPlayedCards . waitForWinnerToPlayCard winner) subroundEvaluatedGame
  |round >= length  (cardsPerRound Model.deck $ length $ playerStates gs) = evaluateRound subroundEvaluatedGame{phase=GameOver}
  |otherwise = (waitForNextTricks . clearCurrentColor . clearPlayedCards . setNewTrump . dealCards . evaluateRound) subroundEvaluatedGame
  where (subroundEvaluatedGame, winner) = evaluateSubRound gs


loginLogic :: Player -> GameState ->  Either Model.PlayerMoveError Model.GameState
loginLogic p gs@GameState{phase=Idle, playerStates=ps}
  |length ps < maxAmountOfPlayers = Right (addPlayers [p] gs)
  |otherwise = Left GameFull
loginLogic p gs@GameState{playerStates=ps}
  |amountOfBots ps > 0 = Right (reevaluatePlayersTurn $ gs{playerStates= (p `replaceBotWithPlayer` ps)})
  |otherwise = Left GameFull


