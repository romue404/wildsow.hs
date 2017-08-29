(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('OverviewCtrl', OverviewCtrl);

  OverviewCtrl.$inject = ['$scope', '$rootScope', '$state', 'localStorageService', 'GameState'];

  function OverviewCtrl($scope, $rootScope, $state, localStorageService, GameState) {

    $rootScope.username = localStorageService.get("username");
    if(!$rootScope.username) $state.go('login');

    $scope.about = "Overview Page";

    $scope.createGame = createGame;
    $scope.joinGame = joinGame;
    $scope.logout = logout;

    function createGame() {
      createOrJoinGame("create");
    }

    function joinGame() {
      createOrJoinGame("join");
    }

    function logout() {
      $rootScope.username = null;
      localStorageService.set("username", null);
      $state.go('login');
    }

    function createOrJoinGame(type) {
      $rootScope.gameId = $scope.gameId;
      let action = GameState.createActionRequest(type, $scope.gameId, $rootScope.username);
      action.botType = 'none';
      GameState.sendActionRequest(action);
      $state.go('lobby');
    }

  }

})();