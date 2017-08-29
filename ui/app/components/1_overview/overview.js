(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('OverviewCtrl', OverviewCtrl);

  OverviewCtrl.$inject = ['$scope'];

  function OverviewCtrl($scope) {
    $scope.title = "About";
    $scope.about = "Overview Page"
  }

})();