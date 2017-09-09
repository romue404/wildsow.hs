(function () {
  'use strict';

  angular
    .module('wildsow')
    .controller('LobbyCtrl', LobbyCtrl);

  LobbyCtrl.$inject = ['$scope', '$state', 'localStorageService', 'GameState'];

  function LobbyCtrl($scope, $state, localStorageService, GameState) {


    var gameId = localStorageService.get("gameId");
    $scope.gameId = localStorageService.get("gameId");

    $scope.username = localStorageService.get("username");
    if(!$scope.username) $state.go('login');

    $scope.isHuman = function (tag) {
      if(tag) {
        if(tag == "RandomBot"){
          return "laptop"
        }
        else if (tag == "SmartBot") {
          return "android"
        }
      }
      return "person";
    };

    $scope.about = "Lobby Page";
    $scope.currentGameState = GameState.current.state || localStorageService.get("gameState");
    updateUi($scope.currentGameState);

    $scope.$on('gameStateUpdated', function(event, currentGameState) {
      updateUi(currentGameState);
      $scope.$apply();
    });

    $scope.chatInput = '';
    $scope.chats = [];

    $scope.$on('chatReceived', function(event, chat) {
      $scope.chats.push(chat);
      $scope.$apply();
    });

    $scope.$on('gameStarted', function(event, currentGameState) {
      $state.go('game');
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



    var botNames = JSON.parse(localStorageService.get('botNames')) || [
      'Arni',
      'Merkel',
      'Ronaldo',
      'Trump',
      'Yoda'
    ];



    $scope.startGame = startGame;
    $scope.addBot = addBot;
    $scope.sendChat = sendChat;

    function startGame() {
      let action = GameState.createActionRequest('start', gameId, $scope.username);
      GameState.sendActionRequest(action);
      $state.go('game');
    }

    function addBot() {
      var botName = botNames.pop();
      localStorageService.set('botNames', JSON.stringify(botNames));
      var botType = {botType: $scope.select.value};
      let action = GameState.createActionRequest('join', gameId, botName, botType);
      GameState.sendActionRequest(action);
    }

    function sendChat(msg) {
      if(!msg) return;
      GameState.sendActionRequest({kind: 'chat', userName: $scope.username, message: msg});
      $scope.chatInput = '';
    }

  }

})();