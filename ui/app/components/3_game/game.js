(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('GameCtrl', GameCtrl);

  GameCtrl.$inject = ['$scope'];

  function GameCtrl($scope) {
    $scope.title = "About";
    $scope.about = "Game Page"
  }

})();