(function () {
  'use strict';

  var baseUrl = 'ws://localhost:5000';

  angular
    .module('wildsow')
    .factory('GameState', GameState);

  GameState.$inject = ['$websocket'];

  function GameState($websocket) {
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
    var current;

    dataStream.onMessage(function(message) {
      states.push(JSON.parse(message.data));
    });

    var methods = {
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