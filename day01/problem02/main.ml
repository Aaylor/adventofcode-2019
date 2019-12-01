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

let eval_fuel mass = (mass / 3) - 2

let eval_complete_fuel fuel =
  let rec aux_eval_complete_fuel acc n =
    let fuel = eval_fuel n in
    if fuel <= 0 then acc
    else aux_eval_complete_fuel (acc + fuel) fuel
  in
  fuel + aux_eval_complete_fuel 0 fuel

let to_fuel input =
  (* For each module mass, we evaluate the necessary fuel; and then we evaluate
     the necessary fuel for the fuel. *)
  eval_fuel (int_of_string input)
  |> eval_complete_fuel

let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let mass_stream =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_lines
    |> Lwt_stream.map to_fuel
  in
  Lwt_stream.fold ( + ) mass_stream 0

let () =
  let result = Lwt_main.run main in
  Logs.app (fun m -> m "RESULT: %d" result)
