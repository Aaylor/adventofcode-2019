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

open Lwt.Infix

(** {1 DATATYPES} *)

module Direction = struct
  type t = R | L | U | D

  let of_char = function
    | 'R' -> R
    | 'L' -> L
    | 'U' -> U
    | 'D' -> D
    | _ -> assert false
end

module Coordinate: sig
  type t
  val mk: int -> int -> t
  val add_x: int -> t -> t
  val add_y: int -> t -> t
  val compare: t -> t -> int
  val manhattan: t -> t -> int
end = struct
  type t = int * int
  let mk x y = x, y
  let add_x n (x, y) = n + x, y
  let add_y n (x, y) = x, n + y
  let compare (x1, y1) (x2, y2) =
    let r = compare x1 x2 in
    if r = 0 then compare y1 y2 else r
  let manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
end

module Coordinate_set = Set.Make(Coordinate)
module Coordinate_map = Map.Make(Coordinate)



(** {1 SOLUTION} *)

let next_step direction current_coordinate =
  current_coordinate |>
  match direction with
  | Direction.R -> Coordinate.add_x 1
  | Direction.L -> Coordinate.add_x (-1)
  | Direction.U -> Coordinate.add_y 1
  | Direction.D -> Coordinate.add_y (-1)

let deploy_wire data (direction, distance) =
  let rec aux distance (length, length_map, current_coordinate, set as data) =
    if distance = 0 then
      data
    else
      let next_length = succ length in
      let next_coordinate = next_step direction current_coordinate in
      aux
        (pred distance)
        (next_length,
         Coordinate_map.add next_coordinate next_length length_map,
         next_coordinate,
         Coordinate_set.add next_coordinate set)
  in
  aux distance data

let map_wire_element element =
  Scanf.sscanf
    element
    "%c%d"
    (fun direction distance -> Direction.of_char direction, distance)

let handle_wire line =
  let init = 0, Coordinate_map.empty, Coordinate.mk 0 0, Coordinate_set.empty in
  line >|= fun line ->
  String.split_on_char ',' line
  |> List.map map_wire_element
  |> List.fold_left deploy_wire init
  |> (fun (_, length_map, _, coordinate_set) -> length_map, coordinate_set)

let get_min_length ~map1 ~map2 set =
  let total_length c = Coordinate_map.(find c map1 + find c map2) in
  Coordinate_set.fold
    (fun t acc -> min (total_length t) acc)
    set
    (total_length (Coordinate_set.choose set))

let main =
  Lwt_unix.openfile "input.txt" Unix.[ O_RDONLY ] 0640 >>= fun descr ->
  let io = Lwt_io.of_fd ~mode:Lwt_io.input descr in
  let wire1 = handle_wire (Lwt_io.read_line io) in
  let wire2 = handle_wire (Lwt_io.read_line io) in
  wire1 >>= fun (wire1_length_map, wire1_set) ->
  wire2 >>= fun (wire2_length_map, wire2_set) ->
  let wire_intersections = Coordinate_set.inter wire1_set wire2_set in
  Lwt.return @@
  get_min_length
    ~map1:wire1_length_map
    ~map2:wire2_length_map
    wire_intersections

let () =
  let result = Lwt_main.run main in
  Logs.app (fun m -> m "Result: %d" result)
