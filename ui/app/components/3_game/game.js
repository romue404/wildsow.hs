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
        $scope.heap = $scope.currentGameState.playerState.map(ps => ps.playedCard);
        console.log($scope.player);
      }
    }

    // apis
    $scope.tellTricks = tellTricks;
    $scope.playCard = playCard;
    $scope.getCardImgPath = getCardImgPath;

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

  }

})();