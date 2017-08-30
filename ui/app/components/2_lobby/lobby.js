(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('LobbyCtrl', LobbyCtrl);

  LobbyCtrl.$inject = ['$scope', '$state', 'localStorageService', 'GameState'];

  function LobbyCtrl($scope, $state, localStorageService, GameState) {

    $scope.username = localStorageService.get("username");
    if(!$scope.username) $state.go('login');

    $scope.about = "Lobby Page";
    $scope.currentGameState = GameState.current.state;

    $scope.$on('gameStateUpdated', function(event, currentGameState) {
      updateUi(currentGameState);
      $scope.$apply();
    });

    function updateUi(currentGameState) {
      var debug = JSON.stringify(currentGameState, null, 2);
    //  console.log(debug)

      $scope.currentGameState = currentGameState || localStorageService.get("gameState");

      if($scope.currentGameState && $scope.currentGameState.playerState){
        $scope.players = $scope.currentGameState.playerState.map(ps => ps.player);
      }
    }


    var botsDescriptions = [
      "Überraschungs Bot - Man weiß es nie!",
      "Statistiker - I love Bayes",
      "Reinforcement - Learning is Living"
    ];

    $scope.select = {
      value: 'Überraschungs Bot - Man weiß es nie!',
      choices: botsDescriptions
    };



    var botNames = localStorageService.get('botNames') || [
      'Green',
      'Blue',
      'Red',
      'White',
      'Purple',
      'Yellow'
    ];

    var gameId = localStorageService.get("gameId");


    $scope.startGame = startGame;
    $scope.addBot = addBot;

    function startGame() {
      let action = GameState.createActionRequest('start', gameId, $scope.username);
      GameState.sendActionRequest(action);
      $state.go('game');
    }

    function addBot() {
      var botName = botNames.pop();
      localStorageService.set('botNames', botNames);
      let action = GameState.createActionRequest('join', gameId, botName);
      action.botType = getBotTypeByName($scope.select.value);
      console.log(action)
      GameState.sendActionRequest(action);
    }

    function getBotTypeByName(name) {
      if(name === botsDescriptions[0]) return "random";
      //TODO: add future bot types when backend implemented
      if(name === botsDescriptions[1]) return "none";
      if(name === botsDescriptions[2]) return "none";
      return 'none';
    }
  }

})();