open BsAsyncMonad.Callback
open LidcoreBsNode

external argv : string array = "" [@@bs.val] [@@bs.scope "process"]

external exit : int -> 'a = "" [@@bs.val] [@@bs.scope "process"]

let usage = "Usage: wavinfo /path/to/input.wav /path/to/output.wav"

let readFile ~position path =
  let stats = Fs.statSync path in
  let length =
    stats##size -. position
  in
  let content =
    Buffer.alloc stats##size
  in
  Fs.openFile path "r" >> fun fd ->
    (Fs.read ~position fd content >> fun (read,_) ->
      assert (read = length);
      return content) &> fun () ->
        Fs.close fd

let () =
 let input, output =
  try
    argv.(2), argv.(3);
  with _ ->
    Js.log usage;
    exit 1
 in
 finish (Wav.read input >> fun wav ->
   let header = wav|.Wav.header in
   let position =
     float (wav|.Wav.data_offset)
   in
   readFile ~position input >> fun data ->
     Wav.write ~header ~data output)
