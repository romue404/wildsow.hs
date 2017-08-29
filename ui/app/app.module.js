(function () {
  'use strict';
  //modules/services that the app uses/depends on
  angular
    .module('wildsow', [
      'ui.materialize',
      'ui.router',
      'ngWebSocket',
      'LocalStorageModule'
    ]);

})();