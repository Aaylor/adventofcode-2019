(*****************************************************************************)
(* The MIT License (MIT)                                                     *)
(*                                                                           *)
(* Copyright (c)                                                             *)
(*   Loïc Runarvot                                                           *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   *)
(* OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                *)
(* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    *)
(* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      *)
(* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT *)
(* OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  *)
(* THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                *)
(*****************************************************************************)

type layer =
  { zero: int;
    one: int;
    two: int; }

let parse_layer ~stream ~max () =
  let rec aux size layer =
    if size = max then
      begin
        Logs.debug (fun m -> m "  -> SIZE(%d); 0 = %d; 1 = %d; 2 = %d" size layer.zero layer.one layer.two);
        Lwt.return_some layer
      end
    else
      match%lwt Lwt_stream.get stream with
      | None -> Lwt.return_none
      | Some '0' -> aux (succ size) { layer with zero = succ layer.zero }
      | Some '1' -> aux (succ size) { layer with one = succ layer.one }
      | Some '2' -> aux (succ size) { layer with two = succ layer.two }
      | Some _ -> assert false
  in
  aux 0 { zero = 0; one = 0; two = 0 }

let parse_layers ~stream ~width ~height () =
  let max = width * height in
  let rec aux layer_with_few_zero =
    match%lwt parse_layer ~stream ~max () with
    | None ->
      begin
        match layer_with_few_zero with
        | None -> assert false
        | Some layer -> Lwt.return (layer.one * layer.two)
      end
    | Some layer ->
      let layer =
        match layer_with_few_zero with
        | None ->
          layer
        | Some layer_with_few_zero ->
          if layer_with_few_zero.zero < layer.zero
          then layer_with_few_zero
          else layer
      in
      aux (Some layer)
  in
  aux None


let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt input_line =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_line
  in
  let stream = Lwt_stream.of_string input_line in
  parse_layers ~stream ~width:25 ~height:6 ()

let () =
  let result = Lwt_main.run main in
  Format.printf "RESULT: %d@." result
