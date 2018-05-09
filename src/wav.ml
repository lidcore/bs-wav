open BsCallback
open LidcoreBsNode

type t = <
  channels         : int; (* 1 = mono ; 2 = stereo *)
  sample_rate      : int; (* in Hz *)
  bytes_per_second : int;
  bytes_per_sample : int; (* 1=8 bit Mono, 2=8 bit Stereo *)
                          (* or 16 bit Mono, 4=16 bit Stereo *)
  bits_per_sample  : int;
  data_offset      : int;
  duration         : float
> Js.t

type input = {
  fd: int;
  mutable offset: int
} [@@bs.deriving abstract]

exception Not_a_wav_file of string

let buf = Buffer.from " "

let input_byte ic =
  Fs.read (fd ic) buf 0. 1. >> fun (ret,buf) ->
    assert (ret = 1.);
    offsetSet ic (offset ic + 1); 
    return (Buffer.get buf 0.)

let read_float_num_bytes ic len =
  let l =
    Array.to_list
      (Array.make len 0.)
  in
  fold_lefti (fun cur idx _ ->
    input_byte ic >> fun b ->
      return
        ((float b) *. 256.**(float idx) +. cur))
             (return 0.) l

let read_int ic =
  read_float_num_bytes ic 4 >> fun ret ->
    return (int_of_float ret)

let read_short ic =
  read_float_num_bytes ic 2 >> fun ret ->
    return (int_of_float ret)

let read_string ic n =
  let n = float n in
  let buf = Buffer.alloc n in
  Fs.read (fd ic) buf 0. n >> fun (ret,buf) ->
    assert (ret = n);
    offsetSet ic (offset ic + (int_of_float ret));
    return (Buffer.toString buf)

let check_head ic =
  seqa [|
    read_string ic 4 >> (fun ret ->
      if ret <> "RIFF" then
        fail (Not_a_wav_file "Bad header: \"RIFF\" expected")
      else
        return ());

    discard(read_int ic);

    read_string ic 4 >> (fun ret ->
      if ret <> "WAVE" then
        fail (Not_a_wav_file "Bad header: \"WAVE\" expected")
      else
        return ());

    read_string ic 4 >> (fun ret ->
      if ret <> "fmt " then
        fail (Not_a_wav_file "Bad header: \"fmt \" expected")
      else
        return ())
  |] >> fun () ->
    read_int ic >> fun fmt_len ->
      if fmt_len < 0x10 then
        fail (Not_a_wav_file "Bad header: invalid \"fmt \" length")
      else
        return () >> fun () ->
          read_short ic >> fun ret ->
            if ret <> 1 then
             fail (Not_a_wav_file "Bad header: unhandled codec")
           else
             return fmt_len

let read path =
  Fs.openFile path "r" >> fun fd ->
    let ic = input ~fd ~offset:0 in
    check_head ic >> fun fmt_len ->
      mapa (fun fn -> fn ic) [|
        read_short;
        read_int;
        read_int;
        read_short;
        read_short
      |] >> fun a ->
        let chan_num     = a.(0) in
        let samp_hz      = a.(1) in
        let byt_per_sec  = a.(2) in
        let byt_per_samp = a.(3) in
        let bit_per_samp = a.(4) in

        (* The fmt header can be padded *)
        (if fmt_len > 0x10 then 
          discard(read_float_num_bytes ic (fmt_len - 0x10))
        else
          return ()) >> fun () ->
            read_string ic 4 >> fun ret ->
              let header = ref ret in
              (* Skip unhandled chunks. *)
              repeat (return (!header <> "data"))
                     (read_int ic >> fun len ->
                       discard (read_string ic len) >> fun () ->
                         read_string ic 4 >> fun ret ->
                           header := ret;
                           return ()) >> fun () ->
                read_int ic >> fun length ->
                  Fs.close fd >> fun () ->
                    return [%bs.obj{
                      channels         = chan_num;
                      sample_rate      = samp_hz;
                      bytes_per_second = byt_per_sec;
                      bytes_per_sample = byt_per_samp;
                      bits_per_sample  = bit_per_samp;
                      data_offset      = offset ic;
                      duration         = (float length) /. (float byt_per_sec)
                    }]
