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

    ];


    $scope.$on('gamelistLoaded', function(event, currentGameState) {
      $scope.games = currentGameState.payload;
    });

    $scope.$on('inLobby', function(event, currentGameState) {
      console.log("Caught in lobby");
      $state.go('lobby');
    });

    $scope.$on('badAction', function(event, error) {
      console.log("Caught error");
      console.log(error);
      if(error.error.includes("already")) {
        $scope.gameAlreadyExists = 'Spiel kann nicht erstellt werden, da der Name bereits vorhanden ist';
      } else {
        $scope.gameNotExists = 'Spiel kann nicht beigetreten werden, da es noch nicht existiert';
      }
      $scope.$apply();
    });

    $scope.selectGame = selectGame;
    $scope.createGame = createGame;
    $scope.joinGame = joinGame;
    $scope.logout = logout;
    $scope.removeErrorMessage = removeErrorMessage;


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
      removeErrorMessage();
      localStorageService.set("gameId", $scope.gameId);
      let action = GameState.createActionRequest(type, $scope.gameId, $scope.username, {botType: 'none'});
      GameState.sendActionRequest(action);
    }

    function removeErrorMessage() {
      $scope.gameAlreadyExists = null;
      $scope.gameNotExists = null;
    }

    function getGames() {
      GameState.sendActionRequest({kind: "openGames", userName: $scope.username});
    }

    getGames();

  }

})();