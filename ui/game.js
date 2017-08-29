var wildsow = wildsow || {};

$(document).ready(function(){
  // the "href" attribute of the modal trigger must specify the modal ID that wants to be triggered
  $('.modal').modal();

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

  /**
   * set player names locally, these will be sended every time when a game action is executed
   */
  $('#createPlayer1Name').click(function () {
    wildsow.player1Name = $('#player1Name').val();
    $(this).prop("disabled", true);
    $(this).html("Spieler erstellt");
  })
  $('#createPlayer2Name').click(function () {
    wildsow.player2Name = $('#player2Name').val();
    $(this).prop("disabled", true);
    $(this).html("Spieler erstellt");
  })
  $('#createPlayer3Name').click(function () {
    wildsow.player3Name = $('#player3Name').val();
    $(this).prop("disabled", true);
    $(this).html("Spieler erstellt");
  })
  $('#createPlayer4Name').click(function () {
    wildsow.player4Name = $('#player4Name').val();
    $(this).prop("disabled", true);
    $(this).html("Spieler erstellt");
  })

  /**
   * first player creates a game giving a game name,
   * remaining players joins the game by entering the game name
   */
  $('#createGame1').click(function () {
    wildsow.gameName1 = $('#spielName1').val();
    let action = getAction("create", wildsow.gameName1,  wildsow.player1Name);
    sendDataToServerViaSocket(action);
  });

  $('#joinGame2').click(function () {
    wildsow.gameName2 = $('#spielName2').val();
    let action = getAction("join", wildsow.gameName2,  wildsow.player2Name);
   action.botType = 'none';
    sendDataToServerViaSocket(action);
  });

  $('#joinGame3').click(function () {
    wildsow.gameName3 = $('#spielName3').val();
    let action = getAction("join", wildsow.gameName3, wildsow.player3Name);
    action.botType = 'none';
    sendDataToServerViaSocket(action);
  })

  $('#joinGame4').click(function () {
    wildsow.gameName4 = $('#spielName4').val();
    let action = getAction("join", wildsow.gameName4, wildsow.player4Name);
    sendDataToServerViaSocket(action);
  })

  /**
   * tell tricks
   */
  $('#tellTicks1').click(function () {
    let action = getAction("tellNumberOfTricks", wildsow.gameName1,  wildsow.player1Name);
    action.tricks = Number($('#ticks1').val());
    sendDataToServerViaSocket(action);
  })

  $('#tellTicks2').click(function () {
    let action = getAction("tellNumberOfTricks", wildsow.gameName2,  wildsow.player2Name);
    action.tricks = Number($('#ticks2').val());
    sendDataToServerViaSocket(action);
  })

  $('#tellTicks3').click(function () {
    let action = getAction("tellNumberOfTricks", wildsow.gameName3,  wildsow.player3Name);
    action.tricks = Number($('#ticks3').val());
    sendDataToServerViaSocket(action);
  })

  $('#tellTicks4').click(function () {
    let action = getAction("tellNumberOfTricks", wildsow.gameName4,  wildsow.player4Name);
    action.tricks = Number($('#ticks4').val());
    sendDataToServerViaSocket(action);
  })

  $('#start').click(function () {
    let action = getAction("start", wildsow.gameName1,  wildsow.player1Name);
    sendDataToServerViaSocket(action);
  })



  $('#played1').click(function () {

  })

  $('#played2').click(function () {

  })

  $('#played3').click(function () {

  })

  $('#played4').click(function () {

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


  $('#generateTestData').click(function () {
    let gameId = Math.random();
    $('#player1Name').val("Zhen");
    $('#player2Name').val("Rob");
    $('#player3Name').val("Chris");
    $('#player4Name').val("Dr Jost");

    $('#spielName1').val('Party' + gameId);
    $('#spielName2').val('Party' + gameId);
    $('#spielName3').val('Party' + gameId);
    $('#spielName4').val('Party' + gameId);

    $('#ticks1').val('3');
    $('#ticks2').val('5');
    $('#ticks3').val('3');
    $('#ticks4').val('3');

    /**
     *  Eichel | Gras | Herz | Schellen
     */

    $('#color1').val('Eichel');
    $('#color2').val('Gras');
    $('#color3').val('Herz');
    $('#color4').val('Schellen');
  })

});

wildsow.connection = new WebSocket("ws://localhost:5000");

function sendDataToServerViaSocket(dataToSend) {

  let connection = wildsow.connection;

// When the connection is open, send some data to the server
  connection.send( JSON.stringify(dataToSend) ); // Send the message 'Ping' to the server

// Log errors
  connection.onerror = function (error) {
    console.log('WebSocket Error ' + error);
  };

// Log messages from the server
  connection.onmessage = function (e) {
    //console.log('Server: ' + e.data);
    let gameState = JSON.parse(e.data);

    let debug = JSON.stringify(gameState, null, 2); // spacing level = 2
    $('#gameState').text(debug);

    if(gameState.playerState) {
      $('#round').text(gameState.round);
      $('#phase').text(gameState.phase);
      $('#color').text(gameState.color);
      $('#trump').text(gameState.trump);

      let players = gameState.playerState;

      $('#heap-cards').empty();
      players.forEach(function (player, playerId) {
        playerId = playerId + 1; //due to html naming
        $('#score-player' + playerId).text(player.score);
        $('#tricks-player' + playerId).text(player.tricks);
        $('#tricksSubround-player' + playerId).text(player.tricksSubround);
        $('#name-player' + playerId).text(player.player.playerName);
        $('#type-player' + playerId).text(player.player.tag);

        let heapCard = player.playedCard;
        if(heapCard) {
          $('#heap-cards').append(`
        <div class="game-card heap-card">
         <p class="center">
         <span>${heapCard.color}</span><br>
         <span>${heapCard.value}</span>
         </p> 
       </div>
      `)
          let cardImgName = mapCardToImgName(heapCard);

          let cardImgPath = `images/cards/${cardImgName}.svg`;
          $('.heap-card').last().css("background-image", `url(${cardImgPath})`);
        }

        $('#hand-player' + playerId).empty();
        player.hand.forEach(function (card) {
          $('#hand-player' + playerId).append(`
       <div class="game-card player${playerId}-card">
         <p class="center">
         <span>${card.color}</span><br>
         <span>${card.value}</span>
         </p> 
       </div>
      `)
          let cardImgName = mapCardToImgName(card);

          let cardImgPath = `images/cards/${cardImgName}.svg`;
          $('.player1-card').last().css("background-image", `url(${cardImgPath})`);

        });
      })

      $('.player1-card').click(function () {
        let action = getAction("playCard", wildsow.gameName1,  players[0].player.playerName);
        action.card = {
          color: $(this).children().first().children().first().text(),
          value: $(this).children().first().children().eq(2).text()
        }

        console.log(action.card)

        sendDataToServerViaSocket(action);
      })

    }

  };
}

function getAction(action, gameId, username) {
  return {
    kind: action,
    gameId: gameId,
    userName: username,
  }
}

function mapCardToImgName(card) {
  let cardColor = card.color.toLowerCase();
  if(cardColor === 'schellen') cardColor = 'schelln';
  let cardValueMapper = {
    Seven: '7er',
    Eight: '8er',
    Nine: '9er',
    Ten: '10er',
    Jack: 'Unter',
    Queen: 'Ober',
    King: 'König',
    Ace: 'Sau'
  }
  let cardValue = cardValueMapper[card.value];
  return cardColor + cardValue;
}
