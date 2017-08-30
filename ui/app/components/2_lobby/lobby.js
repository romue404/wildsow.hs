(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('LobbyCtrl', LobbyCtrl);

  LobbyCtrl.$inject = ['$scope', '$rootScope', '$state', 'localStorageService', 'GameState'];

  function LobbyCtrl($scope, $rootScope, $state, localStorageService, GameState) {

    $rootScope.username = localStorageService.get("username");
    if(!$rootScope.username) $state.go('login');

    $scope.about = "Lobby Page";
    $scope.currentGameState = GameState.current.state;

    $scope.$on('gameStateUpdated', function(event, currentGameState) {
      updateUi(currentGameState);
      $scope.$apply();
    });

    function updateUi(currentGameState) {
      var debug = JSON.stringify(currentGameState, null, 2);
      console.log(debug)

      $scope.currentGameState = currentGameState || localStorageService.get("gameState");

      if($scope.currentGameState && $scope.currentGameState.playerState){
        $scope.players = $scope.currentGameState.playerState.map(ps => ps.player);
        console.log($scope.players)
      }
    }




    /*

                    <select>
                  <option value="" disabled selected>Bot auswählen</option>
                  <option value="1">Überraschungs Bot - Man weiß es nie!</option>
                  <option value="2">Statistiker - I love Bayes!</option>
                  <option value="3">Reinforcement - Learning is Living</option>
                </select>

    */

    var botsDescriptions = [
      "Überraschungs Bot - Man weiß es nie!",
      "Statistiker - I love Bayes",
      "Reinforcement - Learning is Living"
    ];


    $scope.select = {
      value: 'Überraschungs Bot - Man weiß es nie!',
      choices: botsDescriptions
    };


    $scope.startGame = startGame;
    $scope.addBot = addBot;

    function startGame() {
      let action = GameState.createActionRequest('start', $rootScope.gameId, $rootScope.username);
      GameState.sendActionRequest(action);
      $state.go('game');
    }

    function addBot() {
      let action = GameState.createActionRequest('join', $rootScope.gameId, $rootScope.username);

      //TODO based on select
      action.botType = 'random';
      GameState.sendActionRequest(action);
    }
  }

})();