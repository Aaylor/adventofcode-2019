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


(** {2 EVAL} *)

let pp_int_list =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
    Format.pp_print_int

let list_take_n l n =
  assert (n > 0);
  let rec aux acc l n =
    match l, n with
    | _, 0 -> List.rev acc
    | x :: xs, n -> aux (x :: acc) xs (pred n)
    | [], _ -> assert false
  in
  aux [] l n

let make_reverse_digits digits n =
  let reversed_digits = List.rev digits in
  let rec aux acc current_digits n =
    match current_digits, n with
    | _, 0 -> acc
    | [], n -> aux acc reversed_digits n
    | x :: xs, n -> aux (x :: acc) xs (pred n)
  in
  aux [] reversed_digits n

let eval_round ~digits () =
  (* There is no need to compute the complete list of digits (650 * 10_000,
     which is a little bit too much to compute...).
     The offset is after the half of the input length: it is easier to consider
     solving a matrix. *)
  let partial_sum = List.fold_left ( + ) 0 digits in
  let rec aux ~acc ~partial_sum digits =
    match digits with
    | [] ->
      List.rev acc
    | x :: xs ->
      let partial_sum' = partial_sum - x in
      let x' = (abs partial_sum) mod 10 in
      aux ~acc:(x' :: acc) ~partial_sum:partial_sum' xs
  in
  aux ~acc:[] ~partial_sum digits

let eval ~digits ~limits () =
  let rec eval_aux digits limits =
    if limits = 0 then
      digits
    else
      let digits' = eval_round ~digits () in
      eval_aux digits' (pred limits)
  in
  eval_aux digits limits

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
  let offset =
    list_take_n digits 7
    |> List.fold_left (fun acc i -> 10 * acc + i) 0
  in
  let digits_len = List.length digits in
  let suffix_len = (digits_len * 10_000) - offset in
  let reverse_digits = make_reverse_digits digits suffix_len in
  Lwt.return (eval ~digits:reverse_digits ~limits:100 ())

let () =
  Format.printf "%s@."
  @@ String.concat ""
  @@ List.map string_of_int
  @@ (Fun.flip list_take_n) 8
  @@ Lwt_main.run main
