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

let rec eval idx stack =
  assert (idx >= 0);
  match stack.(idx) with
  | 1 ->
    let a = stack.(idx + 1) in
    let b = stack.(idx + 2) in
    let c = stack.(idx + 3) in
    stack.(c) <- stack.(a) + stack.(b);
    eval (idx + 4) stack
  | 2 ->
    let a = stack.(idx + 1) in
    let b = stack.(idx + 2) in
    let c = stack.(idx + 3) in
    stack.(c) <- stack.(a) * stack.(b);
    eval (idx + 4) stack
  | 99 ->
    ()
  | n ->
    failwith (Format.sprintf "Invalid opcode: %d@." n)

let next_range (x, y) =
  if y = 99 then
    if x = 99 then None
    else Some (x + 1, 0)
  else
    Some (x, y + 1)

let rec check_range (x, y) stack =
  Logs.debug (fun m -> m "Range (%d, %d)" x y);
  (* Before checking evaluating, we must copy the complete array; and update the
     value at cell [1] and [2]. *)
  let stack' = Array.copy stack in
  stack'.(1) <- x;
  stack'.(2) <- y;
  eval 0 stack';
  (* Check the value *)
  if stack'.(0) = 19690720 then
    x, y
  else
    match next_range (x, y) with
    | None ->
      failwith "Invalid range."
    | Some range ->
      check_range range stack

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
  Lwt.return (check_range (0, 0) stack)

let () =
  let (x, y) = Lwt_main.run main in
  Format.printf "RESULT: %02d%02d@." x y
