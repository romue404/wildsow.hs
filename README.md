# wildsow-hs
Wildsow the cardgame. A student project of LMU Munich.

## Description
Implementation of the cardgame Wildsow. It is round-based game where 3 up to 6 players have to make tricks in several rounds to gain points.
Before each round starts each player have to predict his tricks. The players with the right prediction gains extra points. The player with the most points wins the game. 

![Screen1](https://raw.githubusercontent.com/romue404/wildsow.hs/master/screens/screen4.PNG)

## Features
- Game Logic and Server written in haskell
- Unit tested
- Stateless Haskell Server -> Client extensions easily (e.g. Android, iOS, Desktop and also Web) 
- Includes web based client with AngularJS (mainly displaying the game state returned by the Haskell Server)
- Play against your friends via websocket
- Play against challenging bots of various difficulties
- Game lobby
- Chat

## Install and run
Navigate into this projects root directory and enter `stack install`.
You can now launch the app by entering `wildsow-hs-exe`.
This will open a websocket on port *5000*.

Run bower install in folder "ui/". 
To run the ui you need to start a webserver. You can use a simple python webserver with "python -m SimpleHTTPServer 8000".
To play the game, now open "localhost:8000/index.html" in your browser. Done :-). 

## Additional Informations
For copyright information see LICENSE and copyright.txt.
 
