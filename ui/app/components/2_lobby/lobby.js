(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('LobbyCtrl', LobbyCtrl);

  LobbyCtrl.$inject = ['$scope'];

  function LobbyCtrl($scope) {
    $scope.title = "About";
    $scope.about = "Lobby Page"
  }

})();