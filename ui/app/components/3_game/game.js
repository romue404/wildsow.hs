(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('GameCtrl', GameCtrl);

  GameCtrl.$inject = ['$scope', '$rootScope', '$state', 'localStorageService', 'GameState'];

  function GameCtrl($scope, $rootScope, $state, localStorageService, GameState) {

    $rootScope.username = localStorageService.get("username");
    if(!$rootScope.username) $state.go('login');

    // variables
    $scope.about = "Game Page";

    $scope.$on('gameStateUpdated', function(event, currentGameState) {
      $scope.currentGameState = JSON.stringify(currentGameState, null, 2);
    });

    // apis
    $scope.tellTricks = tellTricks;
    $scope.playCard = playCard;
    
    function tellTricks() {
      var action = GameState.createActionRequest("tellTricks", )
    }

    // functions
    function playCard() {
      GameState.get();
    }
  }

})();