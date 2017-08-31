(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('MainCtrl', MainCtrl);

  MainCtrl.$inject = ['$scope', '$state', 'GameState'];

  function MainCtrl($scope, $state, GameState) {

    // variables
    $scope.gameState = GameState;
    $scope.$on('socketError', function(event, currentGameState) {
      $state.go('error');
    });


  }

})();