open BsAsyncMonad
open LidcoreBsNode

type header = <
  channels         : int; (* 1 = mono ; 2 = stereo *)
  sample_rate      : int; (* in Hz *)
  bytes_per_second : int;
  bytes_per_sample : int; (* 1=8 bit Mono, 2=8 bit Stereo *)
                          (* or 16 bit Mono, 4=16 bit Stereo *)
  bits_per_sample  : int;
> Js.t

type t = <
  header      : header;
  data_offset : int;
  duration    : float
> Js.t

exception Not_a_wav_file of string

val read : string -> t Callback.t
val write : header:header -> data:Buffer.t -> string -> unit Callback.t
