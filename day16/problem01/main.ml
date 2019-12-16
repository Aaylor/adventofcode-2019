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


(** {1 SOLVER} *)

let digit_of_char = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | _ -> assert false

(** {2 PATTERNS} *)

let base_pattern = [ 0; 1; 0; -1 ]
let gen_patterns digits =
  let rec gen_all_patterns ~acc ~current ~limit () =
    if current > limit then
      List.rev acc
    else
      let rec insert_n acc elt n =
        if n = 0
        then acc
        else insert_n (elt :: acc) elt (pred n)
      in
      let rec gen_n_patterns acc pattern =
        match pattern with
        | [] -> acc
        | x :: xs -> gen_n_patterns (insert_n acc x current) xs
      in
      let generated_pattern =
        Array.of_list
        @@ List.rev
        @@ gen_n_patterns [] base_pattern
      in
      gen_all_patterns ~acc:(generated_pattern :: acc) ~current:(succ current) ~limit ()
  in
  Array.of_list base_pattern
  :: gen_all_patterns ~acc:[] ~current:2 ~limit:(List.length digits) ()


(** {2 EVAL} *)

let eval_pattern ~digits ~pattern () =
  let max_idx = Array.length pattern in
  let rec iter acc idx digits =
    match digits with
    | [] ->
      (abs acc) mod 10
    | x :: xs ->
      let pattern_idx = idx mod max_idx in
      let pattern_digit = pattern.(pattern_idx) in
      iter
        (x * pattern_digit + acc)
        (succ idx)
        xs
  in
  iter 0 1 digits

let eval_patterns ~digits ~patterns () =
  List.map
    (fun pattern -> eval_pattern ~digits ~pattern ())
    patterns

let pp_int_list =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
    Format.pp_print_int

let eval ~digits ~patterns ~limits () =
  let rec eval_n ~digits ~n () =
    if n > limits then
      digits
    else
      let digits' = eval_patterns ~digits ~patterns () in
      eval_n ~digits:digits' ~n:(succ n) ()
  in
  eval_n ~digits ~n:1 ()


let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt digits =
    let%lwt line =
      Lwt_io.of_fd ~mode:Lwt_io.input descr
      |> Lwt_io.read_line
    in
    Lwt_stream.to_list
    @@ Lwt_stream.map digit_of_char
    @@ Lwt_stream.of_string line
  in
  let patterns = gen_patterns digits in
  Lwt.return @@ eval ~digits ~patterns ~limits:100 ()

let show_8 l =
  let rec aux l n =
    if n < 8 then begin
      match l with
      | [] ->
        assert false
      | x :: xs ->
        Format.printf "%d" x;
        aux xs (succ n)
    end
  in
  aux l 0;
  Format.printf "@."

let () =
  let result = Lwt_main.run main in
  show_8 result
