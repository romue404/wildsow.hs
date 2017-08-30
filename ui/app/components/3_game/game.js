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

    // variables
    $scope.about = "Game Page";
    $scope.currentGameState = GameState.current.state;

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
        console.log($scope.player);
      }
    }

    // apis
    $scope.tellTricks = tellTricks;
    $scope.playCard = playCard;

    function tellTricks() {
      var action = GameState.createActionRequest("tellTricks", $scope.gameId, $scope.username);
      GameState.sendActionRequest(action);
    }

    // functions
    function playCard() {
      GameState.get();
    }
  }

})();