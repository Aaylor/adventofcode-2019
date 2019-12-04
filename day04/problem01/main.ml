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

let is_valid n =
  let rec aux ?last_digit ~two_digit_in_a_row ~n =
    if n = 0 then
      two_digit_in_a_row
    else
      (* Get the last digit of the number. *)
      let digit = n mod 10 in
      (* Get the next number. *)
      let next_value = n / 10 in
      (* Check that the current digit respect the two rules. *)
      let le_than_last_digit, same_as_last_digit =
        match last_digit with
        | None ->
          (* When there is no last digit, consider that:
             - the current digit is less or equal than the precedent one.
             - it is not the same as the last one (since there is no digit). *)
          true, false
        | Some last_digit ->
          digit <= last_digit,
          two_digit_in_a_row || digit = last_digit
      in
      if not le_than_last_digit then
        false
      else
        aux
          ~last_digit:digit
          ~two_digit_in_a_row:same_as_last_digit
          ~n:next_value
  in
  aux ?last_digit:None ~two_digit_in_a_row:false ~n

let check ~min ~max () =
  let rec aux acc n =
    if n > max then
      acc
    else
      let acc = if is_valid n then succ acc else acc in
      aux acc (succ n)
  in
  aux 0 min

let main =
  let min = 245318 in
  let max = 765747 in
  Lwt.return (check ~min ~max ())

let () =
  let result = Lwt_main.run main in
  Logs.app (fun m -> m "Result: %d" result)
