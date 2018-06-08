# @lidcore/bs-wav

Wav parser and writter written for Bucklescript/node.js using Callback monad.

This provides a simple API to read and WAV files. It also examplifies how to use Bucklescript, [@lidcore/bs-node](https://github.com/lidcore/bs-node) and [bs-async-monad](https://github.com/lidcore/bs-callback)
to write sane asynchronous code.

Current API:

```
open BsAsyncMonad

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
```
