// We need to import the CSS so that webpack will load it.
// The MiniCssExtractPlugin is used to separate it out into
// its own CSS file.
import css from "../css/app.css"

// webpack automatically bundles all modules in your
// entry points. Those entry points can be configured
// in "webpack.config.js".
//
// Import dependencies
//
import "phoenix_html"

// Import local files
//
// Local files can be imported directly using relative paths, for example:
// import socket from "./socket"

var phoenix = require("phoenix");

import { Elm } from "../src/Main.elm";


var socket = new phoenix.Socket("/socket", {});
socket.connect()

function new_channel(subtopic, screen_name) {
  return socket.channel("game:" + subtopic, {screen_name: screen_name});
}


window.addEventListener("DOMContentLoaded", function() {
  var app = Elm.Main.init({
    flags: {
      hash: window.location.hash,
      protocol: window.location.protocol,
      host: window.location.host,
    },
  });


  app.ports.createGame.subscribe(name => {
    var channel = new_channel(name, name);

    join(app, channel, "player1", () =>
      channel.push("new_game")
        .receive("error", response => console.error("Unable to start a new game", response))
        .receive("ok", response => console.log("New Game!", response))
    );
  });


  app.ports.joinGame.subscribe((params) => {
    var channel = new_channel(params.hostName, params.name);

    join(app, channel, "player2", () =>
      channel.push("add_player", params.name)
        .receive("error", response =>
          console.error("Unable to add new player: " + player, response)
        )
    );
  });
});


function join(app, channel, player, onJoin) {
  channel.on("player_added", () => {
    app.ports.playerAdded.send(null);
  });

  app.ports.positionIsland.subscribe(params => {
    params.player = player;

    channel.push("position_island", params)
      .receive("ok", () => app.ports.positionedIsland.send(params))
      .receive("error", () => app.ports.failedPositioningIsland.send(params));
  });

  app.ports.setIslands.subscribe(() =>
    channel.push("set_islands", player)
      .receive("ok", () => app.ports.receivedBoard.send(null))
      .receive("error", () => app.ports.failedSettingIslands.send(null))
  );

  channel.on("player_set_islands", response => {
    if (response.player !== player) {
      app.ports.opponentSetIslands.send(null);
    }
  });

  app.ports.guessCoordinate.subscribe(params =>
    channel.push("guess_coordinate", params)
      .receive("error", () => app.ports.failedGuessingCoordinate.send(null))
  );

  channel.on("player_guessed_coordinate", response =>
    app.ports.playerGuessedCoordinate.send(response)
  )

  /* app.ports.leave.subscribe(() =>
    channel.leave()
      .receive("error", response => console.error("Unable to leave", response))
      .receive("ok", response => console.log("Left successfully", response))
  ); */

  channel.join()
    .receive("error", response => console.error("Unable to join", response))
    .receive("ok", response => {
      console.log("Joined successfully!", response);
      onJoin();
    });
}
