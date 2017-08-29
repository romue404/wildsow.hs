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

    var states = [];

    dataStream.onMessage(function(message) {
      states.push(JSON.parse(message.data));
    });

    var methods = {
      states: states,
      get: function() {
        dataStream.send(JSON.stringify({ action: 'get' }));
      }
    };

    return methods;
  }

})();