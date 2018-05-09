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

let read path =
  let check condition reason =
    if condition then
      return ()
    else
      fail (Not_a_wav_file reason)
  in
  let fmt_len      = ref (-1) in
  let chan_num     = ref (-1) in
  let samp_hz      = ref (-1) in
  let byt_per_sec  = ref (-1) in
  let byt_per_samp = ref (-1) in
  let bit_per_samp = ref (-1) in
  Fs.openFile path "r" >> fun fd ->
    let ic = input ~fd ~offset:0 in
    seqa [|
      read_string ic 4 >> (fun ret ->
        check (ret = "RIFF") "Bad header: \"RIFF\" expected");

      discard(read_int ic);

      read_string ic 4 >> (fun ret ->
        check (ret = "WAVE") "Bad header: \"WAVE\" expected");

      read_string ic 4 >> (fun ret ->
        check (ret = "fmt ") "Bad header: \"fmt \" expected");

      read_int ic >> (fun ret ->
        fmt_len := ret;
        check (ret >= 0x10) "Bad header: invalid \"fmt \" length");

      read_short ic >> (fun ret ->
        check (ret = 1) "Bad header: unhandled codec");

      read_short ic >> (fun ret ->
        chan_num := ret; return ());

      read_int ic >> (fun ret ->
        samp_hz := ret; return ());

      read_int ic >> (fun ret ->
        byt_per_sec := ret; return ());

      read_short ic >> (fun ret ->
        byt_per_samp := ret; return ());

      read_short ic >> (fun ret ->
        bit_per_samp := ret; return ());

      (* The fmt header can be padded *)
      if !fmt_len > 0x10 then 
        discard(read_float_num_bytes ic (!fmt_len - 0x10))
      else
        return ();

      read_string ic 4 >> fun ret ->
        let header = ref ret in
        (* Skip unhandled chunks. *)
        repeat (fun cb -> return (!header <> "data") cb)
               (read_int ic >> fun len ->
                 discard (read_string ic len) >> fun () ->
                   read_string ic 4 >> fun ret ->
                   header := ret;
                   return ());
                   
    |] >> fun () ->
      read_int ic >> fun length ->
        Fs.close fd >> fun () ->
          return [%bs.obj{
            channels         = !chan_num;
            sample_rate      = !samp_hz;
            bytes_per_second = !byt_per_sec;
            bytes_per_sample = !byt_per_samp;
            bits_per_sample  = !bit_per_samp;
            data_offset      = offset ic;
            duration         = (float length) /. (float !byt_per_sec)
          }]
