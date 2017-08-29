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
    $scope.createGame = createGame;
    $scope.joinGame = joinGame;
    $scope.startGame = startGame;
    
    $scope.tellTricks = tellTricks;
    $scope.playCard = playCard;
    
    function tellTricks() {
      
    }

    // functions
    function playCard() {
      GameState.get();
    }
  }

})();