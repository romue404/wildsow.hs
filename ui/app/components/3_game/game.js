(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('GameCtrl', GameCtrl);

  GameCtrl.$inject = ['$scope', '$state', 'localStorageService', 'GameState'];

  function GameCtrl($scope, $state, localStorageService, GameState) {

    // variables
    $scope.username = localStorageService.get("username");
    if(!$scope.username) $state.go('login');
    $scope.gameId = localStorageService.get("gameId");
    $scope.tricks = localStorageService.get("tricks") || 0;

    $scope.showTellTricks = false;
    $scope.showPrevRound = false;

    $scope.currentGameState = GameState.current.state || localStorageService.get("gameState");

    updateUi($scope.currentGameState);

    $scope.$on('gameStateUpdated', function(event, currentGameState) {
      updateUi(currentGameState);
      $scope.$apply();
    });

    $scope.$on('$locationChangeStart', function(event, next, current){
      event.preventDefault();
    });

    // apis

    $scope.getScore = arr => arr ? arr.reduce((a, b) => a + b, 0) : 0;

    $scope.getStiche = (arr, round) => arr ? arr.filter(a => a[0]==round, 0).length : 0;

    $scope.getTrumpImg = trump => trump ? `images/trump/${trump}.png` : '';

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

    $scope.next = function () {
      $scope.showPrevRound = false;
    }

    $scope.tellTricks = tellTricks;
    $scope.playCard = playCard;
    $scope.getCardImgPath = getCardImgPath;

    function tellTricks() {
      localStorageService.set("tricks", $scope.tricks);
      var action = GameState.createActionRequest("tellNumberOfTricks", $scope.gameId, $scope.username, {tricks: $scope.tricks});
      GameState.sendActionRequest(action);
    }

    function playCard(card) {
      if($scope.showPrevRound) return;
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

    function updateUi(currentGameState) {

      $scope.currentGameState = currentGameState || localStorageService.get("gameState");

      if($scope.currentGameState && $scope.currentGameState.playerState){

        $scope.player = $scope.currentGameState.playerState
          .filter(ps => ps.player.playerName === $scope.username)[0];

        $scope.opponents = $scope.currentGameState.playerState
          .filter(ps => ps.player.playerName !== $scope.username)
          .sort((a,b) => (a.player.playerName.toLowerCase() > b.player.playerName.toLowerCase()) ? 1
            : ((b.player.playerName.toLowerCase() > a.player.playerName.toLowerCase()) ? -1 : 0));

        $scope.heap = $scope.currentGameState.playerState
          .map(ps => ({ heapCard: ps.playedCard, cardPlayer: ps.player.playerName }) )
          .sort(cardSorter);

        $scope.currentCardsPlayed = $scope.currentGameState.playerState
          .map(ps => ps.playedCard)
          .filter(card => !!card);

        $scope.showTellTricks = $scope.currentGameState.phase
          .includes($scope.player.player.playerName) && $scope.currentGameState.phase
          .includes("tricks");

        $scope.allCardsOfPrevSubround = $scope.currentGameState.playedCards
          .filter(pc => !$scope.currentCardsPlayed
            .some(ccp => (ccp.color===pc[2].color) && (ccp.value===pc[2].value)))
          .splice(0, $scope.currentGameState.playerState.length)
          .map(card => ({ heapCard: card[2], cardPlayer: card[0] }) )
          .sort(cardSorter);

        if($scope.currentGameState.phase.includes($scope.username))
          $scope.showPrevRound = true;

      }
    }

    function cardSorter(card1, card2) {
      if(card1.cardPlayer === $scope.player.player.playerName)
        return -1;
      else if(card2.cardPlayer === $scope.player.player.playerName)
        return 1;
      else if(card1.cardPlayer > card2.cardPlayer)
        return 1;
      else if(card1.cardPlayer < card2.cardPlayer)
        return -1;
      return 0;
    }

  }

})();