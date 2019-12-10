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

module Coordinate = struct
  type t = C of int * int
  let equal (C (x1, y1)) (C (x2, y2)) = x1 = x2 && y1 = y2
  let compare (C (x1, y1)) (C (x2, y2)) =
    let r = x1 - x2 in
    if r = 0 then y1 - y2 else x1 - x2
end

module Coordinate_set = Set.Make(Coordinate)


(** {1 SOLVER} *)

let height = 23
let width = 23

let rec gcd a b =
  if b = 0 then
    a
  else
    let r = a mod b in
    if r = 0 then b else gcd b r

let ( // ) a b =
  int_of_float (floor (float_of_int a /. float_of_int b))

let in_los coordinate asteroids =
  let (Coordinate.C (station_x, station_y)) = coordinate in
  Coordinate_set.fold
    (fun asteroid_coordinate detected ->
       let (Coordinate.C (asteroid_x, asteroid_y)) = asteroid_coordinate in
       if Coordinate.equal coordinate asteroid_coordinate then
         (* Ignore the current asteroid. *)
         detected
       else
         let dx = asteroid_x - station_x in
         let dy = asteroid_y - station_y in
         let dgcd = abs (gcd dx dy) in
         let reduced_x = dx // dgcd in
         let reduced_y = dy // dgcd in
         Coordinate_set.add
           (Coordinate.C (reduced_x, reduced_y))
           detected)
    asteroids
    Coordinate_set.empty

let destroyed in_los =
  List.map
    (fun (Coordinate.C (x, y)) ->
       atan2 (float_of_int y) (float_of_int x), (x, y))
  @@ List.of_seq
  @@ Coordinate_set.to_seq in_los

let find ~station ~dx ~dy ~asteroids =
  let (Coordinate.C (x, y)) = station in
  let rec aux_iterate (x, y) =
    if Coordinate_set.mem (Coordinate.C (x, y)) asteroids then
      x, y
    else
      aux_iterate (x + dx, y + dy)
  in
  aux_iterate (x + dx, y + dy)

let parse_asteroids input =
  snd @@
  List.fold_left
    (fun (x, set) line ->
       succ x,
       snd @@
       Seq.fold_left
         (fun (y, set) c ->
            if c = '#'
            then (succ y, Coordinate_set.add (C (x, y)) set)
            else (succ y, set))
         (0, set)
         (String.to_seq line))
    (0, Coordinate_set.empty)
    input

let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt input_line =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_lines
    |> Lwt_stream.npeek height
  in
  let asteroids = parse_asteroids input_line in
  let station, _size, in_los =
    let asteroids_list =
      List.of_seq @@
      Coordinate_set.to_seq asteroids
    in
    List.hd @@
    List.sort_uniq (fun (_, x, _) (_, y, _) -> y - x) @@
    List.map
      (fun coordinate ->
         let set = in_los coordinate asteroids in
         coordinate, Coordinate_set.cardinal set, set)
      asteroids_list
  in
  let _, (dx, dy) =
    (Fun.flip List.nth) 199
    @@ List.sort_uniq (fun (x, _) (y, _) -> ( ~- ) (Float.compare x y))
    @@ destroyed in_los
  in
  let (_200_x, _200_y) = find ~station ~dx ~dy ~asteroids in
  Lwt.return (_200_y * 100 + _200_x)

let () =
  let result = Lwt_main.run main in
  Format.printf "RESULT: %d@." result
