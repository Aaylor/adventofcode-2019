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
  Logs.debug (fun m -> m "Index: %d" idx);
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
  stack.(1) <- 12;
  stack.(2) <- 2;
  eval 0 stack;
  Lwt.return stack.(0)

let () =
  let result = Lwt_main.run main in
  Format.printf "RESULT: %d@." result
