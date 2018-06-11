open BsAsyncMonad.Callback

external argv : string array = "" [@@bs.val] [@@bs.scope "process"]

external exit : int -> 'a = "" [@@bs.val] [@@bs.scope "process"]

external stringify : 'a -> unit Js.Nullable.t -> int -> string = "" [@@bs.val] [@@bs.scope "JSON"]

let () =
 let path =
  try
   argv.(2)
  with _ ->
    Js.log "Usage: wavinfo /path/to/file.wav";
    exit 1
 in
 finish (Wav.read path >> fun info ->
   let info = stringify info Js.Nullable.null 2 in
   Js.log {j|Wav file info:\n$(info)|j};
   return ())
