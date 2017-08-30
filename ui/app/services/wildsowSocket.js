(function () {
  'use strict';

  var baseUrl = 'ws://localhost:5000';

  angular
    .module('wildsow')
    .factory('GameState', GameState);

  GameState.$inject = ['$websocket', '$rootScope', 'localStorageService'];

  function GameState($websocket, $rootScope, localStorageService) {
// Open a WebSocket connection
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

    var states = [];
    var current = {};

    dataStream.onMessage(function(message) {
      current.state = JSON.parse(message.data);
      states.push(JSON.parse(message.data));
      console.log(current.state);
      localStorageService.set("gameState", JSON.stringify(JSON.parse(message.data)));
      $rootScope.$broadcast('gameStateUpdated', JSON.parse(message.data));
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
        console.log(action)
        dataStream.send(JSON.stringify(action));
      }
    };

    return methods;
  }

})();