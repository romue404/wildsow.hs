(function () {
  'use strict';

  var baseUrl = 'ws://localhost:5000';

  angular
    .module('wildsow')
    .factory('GameState', GameState);

  GameState.$inject = ['$websocket', '$rootScope'];

  function GameState($websocket, $rootScope) {
// Open a WebSocket connection
    var dataStream = $websocket(baseUrl);

    dataStream.onError(function(error) {
      console.log(error)
    });

    dataStream.onOpen(function() {
      console.log('Socket opened');
    });

    dataStream.onClose(function() {
      console.log('Socket closed');
    });

    var states = [];
    var current = {};

    dataStream.onMessage(function(message) {
      current.state = JSON.parse(message.data);
      states.push(JSON.parse(message.data));
      console.log('Current State: ' + JSON.stringify(current, null, 2));
      $rootScope.$broadcast('gameStateUpdated', JSON.parse(message.data));
    });

    var methods = {
      current: current,
      states: states,
      createActionRequest: function (action, gameId, username) {
        return {
          kind: action,
          gameId: gameId,
          userName: username,
        }
      },
      sendActionRequest: function(action) {
        dataStream.send(JSON.stringify(action));
      }
    };

    return methods;
  }

})();