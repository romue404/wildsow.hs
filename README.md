# wildsow-hs

## API

    "phase" : Idle | GameOver | WaitingForTricks Player | WaitingForColor Player | WaitingForCard Player  | Evaluation,
    "round"  : Int,
    "color" : Eichel | Gras | Herz | Schellen,
    "trump" : Eichel | Gras | Herz | Schellen,
    "playerState" = [PlayerState]
    PlayerState:
    playerState":[{"score":[],"tricks":[],"tricksSubround":[],"player":{"tag":"HumanPlayer","name":"romue"},"hand":[],"playedCard":null}]

### PlayerHand
 