(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('LobbyCtrl', LobbyCtrl);

  LobbyCtrl.$inject = ['$scope', '$rootScope', '$state', 'localStorageService', 'GameState'];

  function LobbyCtrl($scope, $rootScope, $state, localStorageService, GameState) {

    $rootScope.username = localStorageService.get("username");
    if(!$rootScope.username) $state.go('login');

    $scope.about = "Lobby Page";
    $scope.currentGameState = GameState.current.state;

    $scope.$on('gameStateUpdated', function(event, currentGameState) {
      $scope.currentGameState = JSON.stringify(currentGameState, null, 2);
    });

    $scope.currentGameState = $scope.currentGameState || localStorageService.get("gameState");


    $scope.startGame = startGame;

    function startGame() {
      let action = GameState.createActionRequest('start', $rootScope.gameId, $rootScope.username);
      GameState.sendActionRequest(action);
      $state.go('game');
    }
  }

})();