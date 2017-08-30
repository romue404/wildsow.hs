(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('LoginCtrl', LoginCtrl);

  LoginCtrl.$inject = ['$scope', '$state', 'localStorageService'];

  function LoginCtrl($scope, $state, localStorageService) {

    $scope.about = 'Wildsow!';
    $scope.createPlayer = createPlayer;

    function createPlayer() {
      localStorageService.set("username", $scope.username);
      $state.go('overview');
    }
  }

})();