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

(** {1 DATATYPES} *)

module Mode = struct
  type t =
    | Position
    | Immediate

  let of_int = function
    | 0 -> Position
    | 1 -> Immediate
    | _ -> assert false
end

module Opcode = struct
  type t =
    | Addition of Mode.t * Mode.t * Mode.t
    | Multiply of Mode.t * Mode.t * Mode.t
    | Input of Mode.t
    | Output of Mode.t
    | Exit

  let opcode_size = function
    | Addition _ | Multiply _ -> 4
    | Input _ | Output _ -> 2
    | Exit -> 1

  let get_mode n =
    (* Returns the mode according to the digit. *)
    Mode.of_int (n mod 10),
    (* And the next number to check. *)
    n / 10

  let of_int n =
    (* First step, we extract the opcode. *)
    let opcode = n mod 100 in
    let n' = n / 100 in
    match opcode with
    | 1 ->
      let m1, n1 = get_mode n' in
      let m2, n2 = get_mode n1 in
      let m3, _ = get_mode n2 in
      Addition (m1, m2, m3)
    | 2 ->
      let m1, n1 = get_mode n' in
      let m2, n2 = get_mode n1 in
      let m3, _ = get_mode n2 in
      Multiply (m1, m2, m3)
    | 3 ->
      let m1, _ = get_mode n' in
      Input m1
    | 4 ->
      let m1, _ = get_mode n' in
      Output m1
    | 99 ->
      Exit
    | n ->
      failwith (Format.sprintf "Invalid opcode: %d@." n)
end


(** {1 INTERPRETER} *)

let get ~stack ~idx ~mode () =
  match mode with
  | Mode.Immediate ->
    stack.(idx)
  | Mode.Position ->
    stack.(stack.(idx))

let store ~stack ~idx ~mode ~value () =
  match mode with
  | Mode.Immediate ->
    (* NOTE: According to the documentation, the mode for [storing] parameters
       can never be immediate. *)
    assert false
  | Mode.Position ->
    stack.(stack.(idx)) <- value

let rec eval idx stack =
  assert (idx >= 0);
  let opcode = Opcode.of_int stack.(idx) in
  let continue =
    match opcode with
    | Opcode.Addition (m1, m2, m3) ->
      let v1 = get ~stack ~idx:(idx + 1) ~mode:m1 () in
      let v2 = get ~stack ~idx:(idx + 2) ~mode:m2 () in
      store ~stack ~idx:(idx + 3) ~mode:m3 ~value:(v1 + v2) ();
      true
    | Opcode.Multiply (m1, m2, m3) ->
      let v1 = get ~stack ~idx:(idx + 1) ~mode:m1 () in
      let v2 = get ~stack ~idx:(idx + 2) ~mode:m2 () in
      store ~stack ~idx:(idx + 3) ~mode:m3 ~value:(v1 * v2) ();
      true
    | Opcode.Input mode ->
      let value =
        Format.printf
          "Please, enter an integer (note that, for AOC2019 day05, you should \
           enter '1'): @?";
        read_int ()
      in
      store ~stack ~idx:(idx + 1) ~mode ~value ();
      true
    | Opcode.Output mode ->
      let value = get ~stack ~idx:(idx + 1) ~mode () in
      Format.printf "%d@." value;
      true
    | Opcode.Exit ->
      false
  in
  if continue
  then eval (Opcode.opcode_size opcode + idx) stack


(** {1 SOLVER} *)

let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt input_line =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_line
  in
  let stack =
    Array.of_list
    @@ List.map int_of_string
    @@ String.split_on_char ',' input_line
  in
  eval 0 stack;
  Lwt.return_unit

let () =
  Lwt_main.run main
