(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('OverviewCtrl', OverviewCtrl);

  OverviewCtrl.$inject = ['$scope', '$state', 'localStorageService', 'GameState'];

  function OverviewCtrl($scope, $state, localStorageService, GameState) {

    $scope.username = localStorageService.get("username");
    if(!$scope.username) $state.go('login');

    $scope.state = GameState.current;
    $scope.about = "Overview Page";
    $scope.games = [
      'Party',
      'Hacken',
      'Feiern',
      'Saufen',
      'Essen',
      'Party2',
      'Hacken2',
      'Feiern2',
      'Saufen2',
      'Essen2',
      'Party3',
      'Hacken3',
      'Feiern3',
      'Saufen3',
      'Essen3'
    ];

    $scope.selectGame = selectGame;
    $scope.createGame = createGame;
    $scope.joinGame = joinGame;
    $scope.logout = logout;


    function selectGame(game) {
      $scope.selectedGame = game;
      $scope.gameId = game;
    }

    function createGame() {
      createOrJoinGame("create");
    }

    function joinGame() {
      createOrJoinGame("join");
    }

    function logout() {
      $scope.username = null;
      localStorageService.clearAll();
      $state.go('login');
    }

    function createOrJoinGame(type) {
      localStorageService.set("gameId", $scope.gameId);
      let action = GameState.createActionRequest(type, $scope.gameId, $scope.username, {botType: 'none'});
      GameState.sendActionRequest(action);
      $state.go('lobby');
    }

  }

})();