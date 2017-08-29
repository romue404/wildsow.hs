(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('MainCtrl', MainCtrl);

  MainCtrl.$inject = ['$scope', 'GameState'];

  function MainCtrl($scope, GameState) {

    // variables
    $scope.gameState = GameState;

  }

})();