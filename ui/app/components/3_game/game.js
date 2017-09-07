(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('GameCtrl', GameCtrl);

  GameCtrl.$inject = ['$scope', '$state', 'localStorageService', 'GameState'];

  function GameCtrl($scope, $state, localStorageService, GameState) {

    $scope.username = localStorageService.get("username");
    if(!$scope.username) $state.go('login');
    $scope.gameId = localStorageService.get("gameId");
    $scope.tricks = localStorageService.get("tricks") || 0;

    $scope.showTellTricks = false;

    // variables
    $scope.about = "Game Page";
    $scope.currentGameState = GameState.current.state || localStorageService.get("gameState");
    updateUi($scope.currentGameState);

    $scope.$on('gameStateUpdated', function(event, currentGameState) {
      updateUi(currentGameState);
      $scope.$apply();
    });

    function updateUi(currentGameState) {
      var debug = JSON.stringify(currentGameState, null, 2);
      //console.log(debug)

      $scope.currentGameState = currentGameState || localStorageService.get("gameState");

      if($scope.currentGameState && $scope.currentGameState.playerState){

        $scope.player = $scope.currentGameState.playerState.filter(
          ps => ps.player.playerName === $scope.username
        )[0];

        $scope.opponents = $scope.currentGameState.playerState.filter(
          ps => ps.player.playerName !== $scope.username
        ).sort(function(a,b) {return (a.player.playerName > B.player.playerName) ? 1 : ((b.player.playerName > a.player.playerName) ? -1 : 0);} );

        $scope.heap = $scope.currentGameState.playerState.map(function(ps) {
          return {heapCard: ps.playedCard, cardPlayer: ps.player.playerName};
        });

        $scope.currentCardsPlayed = $scope.currentGameState.playerState.map(
          ps => ps.playedCard
        ).filter(card => !!card);

        console.log(  $scope.currentCardsPlayed )

        $scope.showTellTricks = $scope.currentGameState.phase.includes($scope.player.player.playerName) &&
          $scope.currentGameState.phase.includes("tricks");

        $scope.allCardsOfPrevSubround = $scope.currentGameState.playedCards.filter(
          pc => !$scope.currentCardsPlayed.some(ccp => (ccp.color===pc[2].color) && (ccp.value===pc[2].value) ))
          .splice(0, 3);

        console.log( $scope.allCardsOfPrevSubround)

      }
    }

    $scope.getScore = function(arr){
      return arr.reduce((a, b) => a + b, 0);
    };

    $scope.getStiche = function(arr, round){
      var x = arr.filter(a => a[0]==round, 0);
      return x.length;
    };


    $scope.getPlayerIcon = function (tag) {
      if(tag){
        if (tag =="HumanPlayer"){
          return "person";
        }
        else if (tag == "RandomBot"){
          return "laptop";
        }
        else if (tag == "SmartBot"){
          return "android";
        }
      }
      return "HumanPlayer";
    };


    // apis
    $scope.tellTricks = tellTricks;
    $scope.playCard = playCard;
    $scope.getCardImgPath = getCardImgPath;
    $scope.getTrumpImg = getTrumpImg;


    function tellTricks() {
      localStorageService.set("tricks", $scope.tricks);
      var action = GameState.createActionRequest("tellNumberOfTricks", $scope.gameId, $scope.username, {tricks: $scope.tricks});
      GameState.sendActionRequest(action);
    }

    function playCard(card) {
      var c = {color: card.color, value: card.value};
      var action = GameState.createActionRequest("playCard", $scope.gameId, $scope.username, {card: c});
      GameState.sendActionRequest(action);
    }

    function getCardImgPath(card) {
      if(!card) return `images/cards/deck bavaria2.svg`;
      let cardColor = card.color.toLowerCase();
      if(cardColor === 'schellen') cardColor = 'schelln';
      let cardValueMapper = {
        Seven: '7er',
        Eight: '8er',
        Nine: '9er',
        Ten: '10er',
        Jack: 'Unter',
        Queen: 'Ober',
        King: 'König',
        Ace: 'Sau'
      };
      let cardValue = cardValueMapper[card.value];
      let cardImgName = cardColor + cardValue;
      return `images/cards/${cardImgName}.svg`;
    }

    function getTrumpImg(trump) {
      if(trump)
        return `images/trump/${trump}.png`;
      else "";
    }

  }

})();