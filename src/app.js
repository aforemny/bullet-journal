import "./material-components-elm/src/material-components-elm";
import {Elm} from "./elm";

var getToday = function() {
  var now = new Date();
  return '' + now.getFullYear() + '-' + (1 + now.getMonth()) + "-" + now.getDate();
};

var app = Elm.Main.init({
  node: document.querySelector("#root"),
  flags: {
    today: getToday(),
    now: ''+new Date(),
  }
});

var updateToday = function() {
  app.ports.todayUnsafe.send(getToday());
  window.setTimeout(updateToday, 10*1000);
};
window.setTimeout(updateToday, 10*1000);

var updateNow = function() {
  app.ports.nowUnsafe.send('' + new Date());
  window.setTimeout(updateNow, 500);
};
updateNow();
