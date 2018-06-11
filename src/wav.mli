open BsAsyncMonad
open LidcoreBsNode

type format_code =
  | PCM         [@bs.as 0x0001]
  | IEEE_FLOAT  [@bs.as 0x0003]
  | ALAW        [@bs.as 0x0006]
  | MULAW       [@bs.as 0x0007]
  | EXTENSIBLE  [@bs.as 0xFFFE]
[@@bs.deriving jsConverter]

type wav_header = {
  channels         : int; (* 1 = mono ; 2 = stereo *)
  format_code      : int; (* See above *)
  sample_rate      : int; (* in Hz *)
  bytes_per_second : int;
  bytes_per_sample : int; (* 1=8 bit Mono, 2=8 bit Stereo *)
                          (* or 16 bit Mono, 4=16 bit Stereo *)
  bits_per_sample  : int;
} [@@bs.deriving abstract]

type t = {
  header      : wav_header;
  data_offset : int;
  duration    : float
} [@@bs.deriving abstract]

exception Not_a_wav_file of string
exception Not_supported

val read : string -> t Callback.t

(* fails with [Not_supported] for any format_code that isn't PCM. *)
val write : header:wav_header -> data:Buffer.t -> string -> unit Callback.t
