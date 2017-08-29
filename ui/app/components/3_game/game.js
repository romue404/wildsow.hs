(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('GameCtrl', GameCtrl);

  GameCtrl.$inject = ['$scope', 'GameState'];

  function GameCtrl($scope, GameState) {

    // variables
    $scope.title = "About";
    $scope.about = "Game Page";
    $scope.gameState = GameState;

    // apis
    $scope.playCard = playCard;

    // functions
    function playCard() {
      GameState.get();
    }
  }

})();