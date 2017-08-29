(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('LobbyCtrl', LobbyCtrl);

  LobbyCtrl.$inject = ['$scope', '$rootScope', '$state', 'GameState'];

  function LobbyCtrl($scope, $rootScope, $state, GameState) {

    if(!$rootScope.username) $state.go('login');

    $scope.about = "Lobby Page";

    $scope.startGame = startGame;

    function startGame() {

    }
  }

})();