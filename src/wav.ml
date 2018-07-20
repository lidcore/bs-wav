open BsAsyncMonad.Callback
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
  format_code      : int;
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

type input = {
  fd: int;
  mutable offset: int
} [@@bs.deriving abstract]

exception Not_a_wav_file of string
exception Not_supported

let buf = Buffer.from " "

let input_byte ic =
  Fs.read (fdGet ic) buf >> fun (ret,buf) ->
    assert (ret = 1.);
    offsetSet ic (offsetGet ic + 1); 
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
  Fs.read (fdGet ic) buf >> fun (ret,buf) ->
    assert (ret = n);
    offsetSet ic (offsetGet ic + (int_of_float ret));
    return (Buffer.toString buf)

let find_chunk ic id =
  read_string ic 4 >> fun ret ->
    let chunk_header = ref ret in
    repeat (fun () -> return (!chunk_header <> id))
           (fun () ->
             read_int ic >> fun len ->
               (* Official RIFF specifications: if chunk size is odd
                * it is padded with an extra byte. *)
               let len =
                 len + (len mod 2)
               in
               discard (read_string ic len) >> fun () ->
                 read_string ic 4 >> fun ret ->
                 chunk_header := ret;
                 return ())


let read fd =
  let check condition reason =
    if not condition then
      return ()
    else
      fail (Not_a_wav_file reason)
  in
  let remaining_fmt_len = ref (-1) in
  let format_code       = ref (-1) in
  let chan_num          = ref (-1) in
  let samp_hz           = ref (-1) in
  let byt_per_sec       = ref (-1) in
  let byt_per_samp      = ref (-1) in
  let bit_per_samp      = ref (-1) in
  let ic = input ~fd ~offset:0 in
  seqa [|
    read_string ic 4 >> (fun ret ->
      check (ret <> "RIFF") "Bad header: \"RIFF\" expected");

    discard(read_int ic);

    read_string ic 4 >> (fun ret ->
      check (ret <> "WAVE") "Bad header: \"WAVE\" expected");

    find_chunk ic "fmt ";

    read_int ic >> (fun ret ->
      (* This is what will be remaining after reading what we need. *)
      remaining_fmt_len := ret + (ret mod 2) - 0x10; 
      check (not (List.mem ret [16;18;40])) "Bad header: invalid \"fmt \" length");

    read_short ic >> (fun code ->
      format_code := code;
      check (not (List.mem code [0x1;0x3;0x6;0x7;0xFFFE])) "Bad header: unhandled codec");

    read_short ic >| (fun ret ->
      chan_num := ret);

    read_int ic >| (fun ret ->
      samp_hz := ret);

    read_int ic >| (fun ret ->
      byt_per_sec := ret);

    read_short ic >| (fun ret ->
      byt_per_samp := ret);

    read_short ic >| (fun ret ->
      bit_per_samp := ret);

    (* Skip remaining data. *)
    if !remaining_fmt_len > 0 then
      discard(read_float_num_bytes ic !remaining_fmt_len)
    else
      return ();

    find_chunk ic "data";

  |] >> fun () ->
    read_int ic >| fun length ->
      let header = wav_header
        ~channels:!chan_num
        ~format_code:!format_code
        ~sample_rate:!samp_hz
        ~bytes_per_second:!byt_per_sec
        ~bytes_per_sample:!byt_per_samp
        ~bits_per_sample:!bit_per_samp
      in
      t ~header:header
        ~data_offset:(offsetGet ic)
        ~duration:((float length) /. (float !byt_per_sec))

 let read path = 
   Fs.openFile path "r" >> fun fd ->
     read fd &> fun () ->
      Fs.close fd

let short_string i =
  let up = i/256 in
  let down = i-256*up in
  let pre =
    String.make 1 (char_of_int down)
  in
  let post =
    String.make 1 (char_of_int up)
  in
  `String {j|$(pre)$(post)|j}

let int_string n =
  let b = Bytes.create 4 in
  Bytes.set b 0 (char_of_int (n land 0xff));
  Bytes.set b 1 (char_of_int ((n land 0xff00) lsr 8));
  Bytes.set b 2 (char_of_int ((n land 0xff0000) lsr 16));
  Bytes.set b 3 (char_of_int ((n land 0x7f000000) lsr 24));
  `String (Bytes.to_string b)

let write fd data cb =
  let written = ref 0 in
  let len, buf =
    match data with
      | `String data ->
          String.length data, Buffer.from ~encoding:"binary" data
      | `Buffer data ->
          int_of_float (Buffer.length data), data
  in
  repeat (fun () -> return (!written < len))
         (fun () ->
           let offset = float !written in
           Fs.write fd buf ~offset >> fun (ret,_) ->
             written := !written + int_of_float ret;
             return ()) cb

let write ~header ~data path =
  Fs.openFile path "w" >> fun fd ->
    let write = write fd in
    let dlen =
      int_of_float (Buffer.length data)
    in
    seqa [|
      write (`String "RIFF");
      write (int_string (36+dlen));
      write (`String "WAVE");
      write (`String "fmt ");
      write (int_string 16);
      write (short_string (format_codeGet header));
      write (short_string (channelsGet header));
      write (int_string (sample_rateGet header));
      write (int_string (bytes_per_secondGet header));
      write (short_string (bytes_per_sampleGet header));
      write (short_string (bits_per_sampleGet header));
      write (`String "data");
      write (int_string dlen);
      write (`Buffer data)
    |] &> fun () -> Fs.close fd

let write ~header ~data path =
  match format_codeFromJs (format_codeGet header) with
    | Some PCM -> write ~header ~data path
    | _ -> fail Not_supported
