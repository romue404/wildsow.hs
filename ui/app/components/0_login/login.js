(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('LoginCtrl', LoginCtrl);

  LoginCtrl.$inject = ['$scope', '$rootScope', '$state'];

  function LoginCtrl($scope, $rootScope, $state) {
    $scope.about = "Wildsow!";

    $scope.createPlayer = createPlayer;

    function createPlayer() {
      $rootScope.username = $scope.username;
      $state.go('overview');
    }
  }

})();