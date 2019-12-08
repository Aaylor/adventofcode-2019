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

let parse_layers ~stream ~width ~height () =
  let size = width * height in
  let rec aux acc =
    match%lwt Lwt_stream.nget size stream with
    | [] ->
      Lwt.return (List.rev acc)
    | list ->
      let layer = Array.of_list list in
      aux (layer :: acc)
  in
  aux []

let find_first_non_transparent layers idx =
  let rec iter = function
    | [] ->
      assert false
    | layer :: layers ->
      let pixel = layer.(idx) in
      if pixel = '2' then iter layers else pixel
  in
  iter layers

let generate_image ~width ~height ~layers () =
  let size = width * height in
  let image = Array.make (width * height) '0' in
  let rec aux_generate_image idx =
    if idx = size then
      image
    else
      begin
        image.(idx) <- find_first_non_transparent layers idx;
        aux_generate_image (succ idx)
      end
  in
  aux_generate_image 0

let color c =
  if c = '0'
  then ANSITerminal.[ Background White ]
  else ANSITerminal.[ Background Yellow; Foreground Yellow ]

let print_image ~width image =
  Array.iteri
    (fun idx c ->
       if idx mod width = 0 && idx <> 0 then Format.printf "@.";
       ANSITerminal.(printf (color c) " %c " c))
    image;
  Format.printf "@."

let main =
  let width = 25 in
  let height = 6 in
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt input_line =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_line
  in
  let stream = Lwt_stream.of_string input_line in
  let%lwt layers = parse_layers ~stream ~width ~height () in
  let image = generate_image ~width ~height ~layers () in
  print_image ~width image;
  Lwt.return_unit

let () =
  Lwt_main.run main
