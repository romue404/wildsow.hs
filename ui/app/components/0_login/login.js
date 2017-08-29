(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('LoginCtrl', LoginCtrl);

  LoginCtrl.$inject = ['$scope'];

  function LoginCtrl($scope) {
    $scope.title = "About";
    $scope.about = "Login Page"
  }

})();