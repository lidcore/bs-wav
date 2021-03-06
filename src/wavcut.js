// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var BsCallback = require("bs-async-monad/src/bsCallback.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Wav$LidcoreBsWav = require("./wav.js");

var path;

try {
  path = Caml_array.caml_array_get(process.argv, 2);
}
catch (exn){
  console.log("Usage: wavinfo /path/to/file.wav");
  path = process.exit(1);
}

BsCallback.finish(/* None */0, BsCallback.$great$great(Wav$LidcoreBsWav.read(path), (function (info) {
            var info$1 = JSON.stringify(info, null, 2);
            console.log("Wav file info:\n" + (String(info$1) + ""));
            return (function (param) {
                return BsCallback.$$return(/* () */0, param);
              });
          })));

/* path Not a pure module */
