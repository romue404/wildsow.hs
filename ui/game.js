$(document).ready(function(){
  // the "href" attribute of the modal trigger must specify the modal ID that wants to be triggered
  $('.modal').modal();

  var wildsow = wildsow || {};
  wildsow.x1 = 0;
  wildsow.x2 = 0;
  wildsow.x3 = 0;
  wildsow.x4 = 0;
  wildsow.x5 = 0;
  $('.player1 .gamecards').children().each(function () {
    $(this).css("left", 220 + 90*wildsow.x1 + "px");
    wildsow.x1++;
  });
  $('.player2 .gamecards').children().each(function () {
    $(this).css("top", 200 + 90*wildsow.x2 + "px");
    wildsow.x2++;
  });
  $('.player3 .gamecards').children().each(function () {
    $(this).css("top", 200 + 90*wildsow.x3 + "px");
    wildsow.x3++;
  });
  $('.player4 .gamecards').children().each(function () {
    $(this).css("left", 220 + 90*wildsow.x4 + "px");
    wildsow.x4++;
  });
  $('.heap .gamecards').children().each(function () {
    $(this).css("transform", "rotate(" + 30*wildsow.x5 + "deg)");
    $(this).css("left", 50 + 30*wildsow.x5 + "px");
    $(this).css("top", 50 + 15*wildsow.x5 + "px");
    wildsow.x5++;
  });

  wildsow.y1 = 0;
  wildsow.y2 = 0;
  wildsow.y3 = 0;
  wildsow.y4 = 0;
  $('#played1').click(function () {
    var playerCard =  $('.player1 .gamecards .gamecard:eq(' + wildsow.y1 + ')');
    var pxToRight = (wildsow.y1 <= 1) ? "60px" : "-60px";
    playerCard.css("transform", "translate(" + pxToRight +", 300px)");
    wildsow.y1++;
  })
  $('#played2').click(function () {
    var playerCard =  $('.player2 .gamecards .gamecard:eq(' + wildsow.y2 + ')');
    var pxToRight = (wildsow.y2 <= 1) ? "60px" : "-60px";
    playerCard.css("transform", "translate(300px, 0)");
    wildsow.y2++;
  })
  $('#played3').click(function () {
    var playerCard =  $('.player3 .gamecards .gamecard:eq(' + wildsow.y3 + ')');
    var pxToRight = (wildsow.y3 <= 1) ? "60px" : "-60px";
    playerCard.css("transform", "translate(-300px, 0)");
    wildsow.y3++;
  })
  $('#played4').click(function () {
    var playerCard =  $('.player4 .gamecards .gamecard:eq(' + wildsow.y4 + ')');
    var pxToRight = (wildsow.y4 <= 1) ? "60px" : "-60px";
    playerCard.css("transform", "translate(0, -300px)");
    wildsow.y4++;
  })

  /**
   * player1Name
   * createPlayer1Name
   * spielName1
   * createGame1
   * ticks1
   * tellTicks1
   * played1
   * joinGame2
   */
  
  $('#createPlayer1Name').click(function () {
    wildsow.player1Name = $('#player1Name').val();
  })
  $('#createPlayer2Name').click(function () {
    wildsow.player2Name = $('#player2Name').val();
  })
  $('#createPlayer3Name').click(function () {
    wildsow.player3Name = $('#player3Name').val();
  })
  $('#createPlayer4Name').click(function () {
    wildsow.player4Name = $('#player4Name').val();
  })

  $('#createGame1').click(function () {

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