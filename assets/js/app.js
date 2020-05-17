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


function setup(socket, app) {
  var channels = {};
  var ons = {};

  app.ports.join.subscribe(args => {
    channels[args.name] = socket.channel(args.name, args.params);
    channels[args.name].join()
      .receive("error", payload =>
        app.ports.joinError.send({
          name: args.name,
          payload: payload,
        }),
      )
      .receive("ok", payload =>
        app.ports.joinOk.send({
          name: args.name,
          payload: payload,
        }),
      );
  });

  app.ports.push.subscribe(args => {
    if (channels[args.channel]) {
      var handleReceive = push =>
        push
          .receive("error", payload =>
            app.ports.pushError.send({
              id: args.id,
              payload: payload,
            })
          )
          .receive("ok", payload =>
            app.ports.pushOk.send({
              id: args.id,
              payload: payload,
            })
          );

      if (args.payload) {
        handleReceive(channels[args.channel].push(args.event, args.payload))
      } else {
        handleReceive(channels[args.channel].push(args.event))
      }
    } else {
      // TODO: report this error
    }
  });

  app.ports.on.subscribe(args => {
    if (channels[args.channel]) {
      ons[args.event] = channels[args.channel].on(args.event, payload =>
        app.ports.onReceive.send({
          channel: args.channel,
          event: args.event,
          payload: payload,
        })
      );
    } else {
      // TODO: report this error
    }
  });

  app.ports.off.subscribe(args => {
    if (channels[args.channel]) {
      if (ons[args.event]) {
        channels[args.channel].off(args.event, ons[args.event]);
      } else {
        // TODO: report this error
      }
    } else {
      // TODO: report this error
    }
  });
}


window.addEventListener("DOMContentLoaded", function() {
  var app = Elm.Main.init({
    flags: {
      hash: window.location.hash,
      protocol: window.location.protocol,
      host: window.location.host,
      innerWidth: window.innerWidth,
      innerHeight: window.innerHeight,
    },
  });

  var socket = new phoenix.Socket("/socket", {});
  socket.connect()

  setup(socket, app);
});
