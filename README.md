# wildsow-hs
Wildsow the cardgame. A student project of LMU Munich.

## Description
Implementation of the cardgame Wildsow. It is round-based game where 3 up to 6 players have to make tricks in several rounds to gain points.
Before each round starts each player have to predict his tricks. The players with the right prediction gains extra points. The player with the most points wins the game. 

![Screen1](https://raw.githubusercontent.com/romue404/wildsow.hs/master/screens/screen4.PNG)

## Features
- Written in haskell.
- Play against your friends via websocket.
- Play against challenging bots.
- Game lobby
- Chat
- Unit tests
- Webclient with Angular

## Install and run
Navigate into this projects root directory and enter `stack install`.
You can now launch the app by entering `wildsow-hs-exe`.
This will open a websocket on port *5000*.

Start a webserver in folder "ui/". You can use a simple python webserver with "python -m SimpleHTTPServer 8000".
To play the game, now open "localhost:8000/index.html" in your browser. 

## Additional Informations
For copyright information see LICENSE and copyright.txt.
 