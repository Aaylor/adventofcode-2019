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

let path_to_com ~map node =
  let rec aux node depth acc =
    if node = "COM" then
      acc
    else
      let parent_node = Aoc.String_map.find node map in
      aux parent_node (succ depth) ((parent_node, depth) :: acc)
  in
  aux node 0 []

let find_transfer_count path1 path2 =
  let rec aux path1 path2 =
    match path1, path2 with
    | (node1, depth1) :: path1', (node2, depth2) :: path2' ->
      (* When the node is different, the number of transfer needed is the
         addition of the depth of the two nodes.
         We have to add 2 to the solution, which represent the transfer between
         the two different node and the last common node. *)
      if node1 <> node2
      then depth1 + depth2 + 2
      else aux path1' path2'
    | (_, depth1) :: _, _ ->
      depth1
    | [], (_, depth2) :: _ ->
      depth2
    | [], [] ->
      assert false
  in
  aux path1 path2

let solve ~map () =
  let path_you = path_to_com ~map "YOU" in
  let path_san = path_to_com ~map "SAN" in
  find_transfer_count path_you path_san

let prepare_map element map =
  (* Map a node with its parent. *)
  match String.split_on_char ')' element with
  | [ a; b ] -> Aoc.String_map.add b a map
  | _ -> assert false

let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt orbit_map =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_lines
    |> fun s -> Lwt_stream.fold prepare_map s Aoc.String_map.empty
  in
  Lwt.return (solve ~map:orbit_map ())

let () =
  let result = Lwt_main.run main in
  Logs.app (fun m -> m "Result: %d" result)
