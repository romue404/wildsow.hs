(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('OverviewCtrl', OverviewCtrl);

  OverviewCtrl.$inject = ['$scope', '$rootScope', '$state', 'GameState'];

  function OverviewCtrl($scope, $rootScope, $state, GameState) {

    if(!$rootScope.username) $state.go('login');

    $scope.about = "Overview Page";

    $scope.createGame = createGame;
    $scope.joinGame = joinGame;


    function createGame() {
        let action = GameState.createActionRequest('create', $scope.gameId, $rootScope.username);
        GameState.sendActionRequest(action);
    }

    function joinGame(gameId) {
      let action = GameState.createActionRequest('join', gameId, $rootScope.username);
      GameState.sendActionRequest(action);
    }

  }

})();