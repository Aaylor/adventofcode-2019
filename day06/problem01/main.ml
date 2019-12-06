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

(* Not tail recursive at all... But eh. It works! *)
let rec orbits ~map node depth count =
  let count' = depth + count in
  match Aoc.String_map.find_opt node map with
  | None ->
    count'
  | Some node'_set ->
    Aoc.String_set.fold
      (fun node' count'' -> orbits ~map node' (depth + 1) count'')
      node'_set
      count'

let prepare_map element map =
  (* Each key in the map represent an [object] associated to the set of objects
     orbiting around. *)
  match String.split_on_char ')' element with
  | [ a; b ] ->
    let set =
      match Aoc.String_map.find_opt a map with
      | None -> Aoc.String_set.singleton b
      | Some set -> Aoc.String_set.add b set
    in
    Aoc.String_map.add a set map
  | _ ->
    assert false

let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt orbit_map =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_lines
    |> fun s -> Lwt_stream.fold prepare_map s Aoc.String_map.empty
  in
  Lwt.return (orbits ~map:orbit_map "COM" 0 0)

let () =
  let result = Lwt_main.run main in
  Logs.app (fun m -> m "Result: %d" result)
