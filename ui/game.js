var connection = new WebSocket("ws://localhost:8080");

let firstStep = {
  kind: "create",
  gameId: "hackers",
  userName: "zhen"
}

// When the connection is open, send some data to the server
connection.onopen = function () {
  connection.send( JSON.stringify(firstStep) ); // Send the message 'Ping' to the server
};

// Log errors
connection.onerror = function (error) {
  console.log('WebSocket Error ' + error);
};

// Log messages from the server
connection.onmessage = function (e) {
  console.log('Server: ' + e.data);
};