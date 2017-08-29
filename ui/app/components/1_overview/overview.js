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
      let action = GameState.createActionRequest('create', $scope.gameId, $rootScope.username);
      GameState.sendActionRequest(action);
    }

    function joinGame(gameId) {
      let action = GameState.createActionRequest('join', gameId, $rootScope.username);
      GameState.sendActionRequest(action);
    }

    function logout() {
      $rootScope.username = null;
      localStorageService.set("username", null);
      $state.go('login');
    }

  }

})();