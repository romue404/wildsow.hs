(function () {
  'use strict';

  var baseUrl = 'ws://localhost:5000';

  angular
    .module('wildsow')
    .factory('GameState', GameState);

  GameState.$inject = ['$websocket', '$rootScope', 'localStorageService'];

  function GameState($websocket, $rootScope, localStorageService) {

    var dataStream = $websocket(baseUrl);

    dataStream.onError(function(error) {
      console.log(error)
    });

    dataStream.onOpen(function() {
      console.log('Socket opened');
      $rootScope.$broadcast('gameStateUpdated', JSON.parse(localStorageService.get("gameState")));
    });

    dataStream.onClose(function() {
      console.log('Socket closed');
    });

    dataStream.onError(function() {
      console.log('Socket Error');
      $rootScope.$broadcast('socketError');
    });

    var states = [];
    var current = {};

    dataStream.onMessage(function(message) {

      current.state = JSON.parse(message.data);

      let gameState = current.state && current.state.phase;
      let error = current.state && current.state.error;
      let kind = current.state && current.state.kind;

      states.push(JSON.parse(message.data));
      console.log("Received following response...");
      console.log(current.state);
      localStorageService.set("gameState", JSON.stringify(JSON.parse(message.data)));

      if(gameState){
        let phase = current.state.phase;
        $rootScope.$broadcast('gameStateUpdated', JSON.parse(message.data));
        if(phase.startsWith("Waiting for player")){
          $rootScope.$broadcast('gameStarted', JSON.parse(message.data));
        } else if(phase.includes("Idle")) {
          $rootScope.$broadcast('inLobby', JSON.parse(message.data));
        }
      } else if(error) {
        $rootScope.$broadcast('badAction', JSON.parse(message.data));
      } else if(kind && (kind == 'games')) {
        $rootScope.$broadcast('gamelistLoaded', JSON.parse(message.data));
      }
    });

    var methods = {
      current: current,
      states: states,
      createActionRequest: function (action, gameId, username, etc) {
        return Object.assign({
          kind: action,
          gameId: gameId,
          userName: username,
        }, etc)
      },
      sendActionRequest: function(action) {
        console.log("Sending follow action...");
        console.log(action);
        dataStream.send(JSON.stringify(action));
      }
    };

    return methods;
  }

})();