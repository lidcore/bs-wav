open BsCallback
open LidcoreBsNode

external argv : string array = "" [@@bs.val] [@@bs.scope "process"]

external exit : int -> 'a = "" [@@bs.val] [@@bs.scope "process"]

let usage = "Usage: wavinfo /path/to/input.wav /path/to/output.wav"

let () =
 let input, output =
  try
    argv.(2), argv.(3);
  with _ ->
    Js.log usage;
    exit 1
 in
 finish (Wav.read input >> fun header ->
   let stats = Fs.statSync input in
   let position =
     float header##data_offset
   in
   let length =
     stats##size -. position
   in
   let content =
     Buffer.alloc stats##size
   in
   (Fs.openFile input "r" >> fun fd ->
     (Fs.read ~position fd content >> fun (read,_) ->
       assert (read = length);
       return (Buffer.toString ~encoding:"binary" content)) &> fun () ->
         Fs.close fd) >> fun data ->
         Wav.write ~header ~data output)
