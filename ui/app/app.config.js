(function () {
  'use strict';
  angular
    .module('wildsow')
    .config(['localStorageServiceProvider', function (localStorageServiceProvider) {
      localStorageServiceProvider
        .setPrefix('wildsow')
        .setStorageType('sessionStorage')
    }]);
})();