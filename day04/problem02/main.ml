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
  let rec aux ?last_digit ~two_in_row ~count_of_same_digit ~n =
    if n = 0 then
      (* The final boolean is one of:
         - we have seen two same digit in a row.
         - the strictly last two digit encoutered are equal/ *)
      two_in_row || count_of_same_digit = 2
    else
      (* Get the last digit of the number. *)
      let digit = n mod 10 in
      (* Get the next number. *)
      let next_value = n / 10 in
      (* Check that the current digit respect the two rules. *)
      let le_than_last_digit, two_in_row, count_of_same_digit =
        match last_digit with
        | None ->
          (* When there is no last digit, consider that:
             - the current digit is less or equal than the precedent one.
             - we have not seen two digits in a row yet
             - it is not the same as the last one (since there is no digit). *)
          true, false, 1
        | Some last_digit ->
          let two_in_row, count_of_same_digit =
            if last_digit = digit then
              (* This is the same digit as the precedent one:
                 - do not update the [two_in_row] value, since we do not have
                   the information yet.
                 - increment the number of same digit encountered in a row. *)
              two_in_row, succ count_of_same_digit
            else
              (* The two digits are different:
                 - If we already encoutered the strictly two same digit in a
                   row, we continue to propagate the information; otherwise we
                   check the number of the same digit encoutered in a row.
                 - We set back the count to the default value: 1. *)
              two_in_row || count_of_same_digit = 2, 1
          in
          digit <= last_digit, two_in_row, count_of_same_digit
      in
      if not le_than_last_digit then
        false
      else
        aux
          ~last_digit:digit
          ~two_in_row
          ~count_of_same_digit
          ~n:next_value
  in
  aux ?last_digit:None ~two_in_row:false ~count_of_same_digit:1 ~n

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
