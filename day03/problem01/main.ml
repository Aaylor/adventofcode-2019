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



(** {1 SOLUTION} *)

let next_step direction current_coordinate =
  current_coordinate |>
  match direction with
  | Direction.R -> Coordinate.add_x 1
  | Direction.L -> Coordinate.add_x (-1)
  | Direction.U -> Coordinate.add_y 1
  | Direction.D -> Coordinate.add_y (-1)

let deploy_wire data (direction, distance) =
  let rec aux distance (current_coordinate, set) =
    if distance = 0 then
      current_coordinate, set
    else
      let next_coordinate = next_step direction current_coordinate in
      aux
        (pred distance)
        (next_coordinate, Coordinate_set.add next_coordinate set)
  in
  aux distance data

let map_wire_element element =
  Scanf.sscanf
    element
    "%c%d"
    (fun direction distance -> Direction.of_char direction, distance)

let handle_wire line =
  line >|= fun line ->
  String.split_on_char ',' line
  |> List.map map_wire_element
  |> List.fold_left deploy_wire (Coordinate.mk 0 0, Coordinate_set.empty)
  |> snd

let get_min_manhattan set =
  let manhattan coord = Coordinate.(manhattan coord (mk 0 0)) in
  Coordinate_set.fold
    (fun t acc -> min (manhattan t) acc)
    set
    (manhattan (Coordinate_set.choose set))

let main =
  Lwt_unix.openfile "input.txt" Unix.[ O_RDONLY ] 0640 >>= fun descr ->
  let io = Lwt_io.of_fd ~mode:Lwt_io.input descr in
  let wire1 = handle_wire (Lwt_io.read_line io) in
  let wire2 = handle_wire (Lwt_io.read_line io) in
  wire1 >>= fun wire1_set ->
  wire2 >>= fun wire2_set ->
  let wire_intersections = Coordinate_set.inter wire1_set wire2_set in
  Lwt.return (get_min_manhattan wire_intersections)

let () =
  let result = Lwt_main.run main in
  Logs.app (fun m -> m "Result: %d" result)
