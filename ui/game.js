$(document).ready(function(){
  // the "href" attribute of the modal trigger must specify the modal ID that wants to be triggered
  $('.modal').modal();

  var tbial = tbial || {};
  tbial.x1 = 0;
  tbial.x2 = 0;
  tbial.x3 = 0;
  tbial.x4 = 0;
  tbial.x5 = 0;
  $('.player1 .gamecards').children().each(function () {
    $(this).css("left", 220 + 90*tbial.x1 + "px");
    tbial.x1++;
  });
  $('.player2 .gamecards').children().each(function () {
    $(this).css("top", 200 + 90*tbial.x2 + "px");
    tbial.x2++;
  });
  $('.player3 .gamecards').children().each(function () {
    $(this).css("top", 200 + 90*tbial.x3 + "px");
    tbial.x3++;
  });
  $('.player4 .gamecards').children().each(function () {
    $(this).css("left", 220 + 90*tbial.x4 + "px");
    tbial.x4++;
  });
  $('.heap .gamecards').children().each(function () {
    $(this).css("transform", "rotate(" + 30*tbial.x5 + "deg)");
    $(this).css("left", 50 + 30*tbial.x5 + "px");
    $(this).css("top", 50 + 15*tbial.x5 + "px");
    tbial.x5++;
  });

  tbial.y1 = 0;
  tbial.y2 = 0;
  tbial.y3 = 0;
  tbial.y4 = 0;
  $('#played1').click(function () {
    var playerCard =  $('.player1 .gamecards .gamecard:eq(' + tbial.y1 + ')');
    var pxToRight = (tbial.y1 <= 1) ? "60px" : "-60px";
    playerCard.css("transform", "translate(" + pxToRight +", 300px)");
    tbial.y1++;
  })
  $('#played2').click(function () {
    var playerCard =  $('.player2 .gamecards .gamecard:eq(' + tbial.y2 + ')');
    var pxToRight = (tbial.y2 <= 1) ? "60px" : "-60px";
    playerCard.css("transform", "translate(300px, 0)");
    tbial.y2++;
  })
  $('#played3').click(function () {
    var playerCard =  $('.player3 .gamecards .gamecard:eq(' + tbial.y3 + ')');
    var pxToRight = (tbial.y3 <= 1) ? "60px" : "-60px";
    playerCard.css("transform", "translate(-300px, 0)");
    tbial.y3++;
  })
  $('#played4').click(function () {
    var playerCard =  $('.player4 .gamecards .gamecard:eq(' + tbial.y4 + ')');
    var pxToRight = (tbial.y4 <= 1) ? "60px" : "-60px";
    playerCard.css("transform", "translate(0, -300px)");
    tbial.y4++;
  })



});

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