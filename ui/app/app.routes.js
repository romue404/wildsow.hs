(function () {
  'use strict';
  angular
    .module('wildsow')
    .config(uiRouterConfig);

  uiRouterConfig.$inject = ['$stateProvider', '$urlRouterProvider'];

  function uiRouterConfig($stateProvider, $urlRouterProvider) {
    $urlRouterProvider.otherwise('/overview');
    $stateProvider
      .state('login', {
        url: '/login',
        templateUrl: 'app/components/0_login/login.html',
        controller: 'LoginCtrl'
      })
      .state('overview', {
        url: '/overview',
        templateUrl: 'app/components/1_overview/overview.html',
        controller: 'OverviewCtrl'
      })
      .state('lobby', {
        url: '/lobby',
        templateUrl: 'app/components/2_lobby/lobby.html',
        controller: 'LobbyCtrl'
      })
      .state('game', {
        url: '/game',
        templateUrl: 'app/components/3_game/game.html',
        controller: 'GameCtrl'
      })

    ;

  }

})();