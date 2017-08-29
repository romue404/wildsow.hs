(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('GameCtrl', GameCtrl);

  GameCtrl.$inject = ['$scope', '$rootScope', '$state', 'GameState'];

  function GameCtrl($scope, $rootScope, $state, GameState) {

    if(!$rootScope.username) $state.go('login');

    // variables
    $scope.about = "Game Page";

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