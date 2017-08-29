(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('LoginCtrl', LoginCtrl);

  LoginCtrl.$inject = ['$scope', '$rootScope', '$state', 'localStorageService'];

  function LoginCtrl($scope, $rootScope, $state, localStorageService) {
    $scope.about = "Wildsow!";

    $scope.createPlayer = createPlayer;

    function createPlayer() {
      $rootScope.username = $scope.username;
      localStorageService.set("username", $scope.username);
      $state.go('overview');
    }
  }

})();