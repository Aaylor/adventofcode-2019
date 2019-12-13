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

module Coordinate3D = struct
  type t =
    { x: int;
      y: int;
      z: int; }
end

module Moon = struct
  type t =
    { coordinates: Coordinate3D.t;
      velocity: Coordinate3D.t }

  let make ~x ~y ~z =
    { coordinates = Coordinate3D.{ x; y; z };
      velocity = Coordinate3D.{ x = 0; y = 0; z = 0 } }

  let apply_gravity_aux v1 v2 =
    Format.printf "    == GRAV AUX: %d ~ %d@." v1 v2;
    if v1 = v2 then v1, v2
    else if v1 > v2 then v1 + 1, v2 - 1
    else v1 - 1, v2 + 1

  let apply_gravity (m1, m2) =
    let open Coordinate3D in
    let m1_x, m2_x =
      if m1.coordinates.x = m2.coordinates.x then
        m1.velocity.x, m2.velocity.x
      else if m1.coordinates.x > m2.coordinates.x then
        m1.velocity.x - 1, m2.velocity.x + 1
      else
        m1.velocity.x + 1, m2.velocity.x - 1
    in
    let m1_y, m2_y =
      if m1.coordinates.y = m2.coordinates.y then
        m1.velocity.y, m2.velocity.y
      else if m1.coordinates.y > m2.coordinates.y then
        m1.velocity.y - 1, m2.velocity.y + 1
      else
        m1.velocity.y + 1, m2.velocity.y - 1
    in
    let m1_z, m2_z =
      if m1.coordinates.z = m2.coordinates.z then
        m1.velocity.z, m2.velocity.z
      else if m1.coordinates.z > m2.coordinates.z then
        m1.velocity.z - 1, m2.velocity.z + 1
      else
        m1.velocity.z + 1, m2.velocity.z - 1
    in
    { m1 with velocity = { x = m1_x; y = m1_y; z = m1_z } },
    { m2 with velocity = { x = m2_x; y = m2_y; z = m2_z } }

  let apply_velocity m =
    let open Coordinate3D in
    let x = m.coordinates.x + m.velocity.x in
    let y = m.coordinates.y + m.velocity.y in
    let z = m.coordinates.z + m.velocity.z in
    { m with coordinates = { x; y; z } }

  let potential_energy m =
    let open Coordinate3D in
    abs m.coordinates.x + abs m.coordinates.y + abs m.coordinates.z

  let kinectic_energy m =
    let open Coordinate3D in
    abs m.velocity.x + abs m.velocity.y + abs m.velocity.z

  let energy m =
    potential_energy m * kinectic_energy m
end



let permutations_n2 list =
  let rec aux_permutations_n2 acc list =
    match list with
    | [] ->
      acc
    | [ x ] ->
      x :: acc
    | x1 :: xs ->
      let x1', xs' =
        List.fold_right
          (fun x2 (x1, xs) ->
             let x1', x2' = Moon.apply_gravity (x1, x2) in
             x1', x2' :: xs)
          xs
          (x1, [])
      in
      aux_permutations_n2 (x1' :: acc) xs'
  in
  aux_permutations_n2 [] list

let parse_moon s =
  Scanf.sscanf s "<x=%d, y=%d, z=%d>" (fun x y z -> Moon.make ~x ~y ~z)

let rec steps ~moons n =
  if n = 0 then
    moons
  else begin
    let moons = permutations_n2 moons in
    let moons = List.map Moon.apply_velocity moons in
    steps ~moons (pred n)
  end

let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt moons =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_lines
    |> Lwt_stream.map parse_moon
    |> Lwt_stream.to_list
  in
  let moons = steps ~moons 1_000 in
  Lwt.return @@
  List.fold_left (fun res moon -> res + Moon.energy moon) 0 moons

let () =
  let result = Lwt_main.run main in
  Format.printf "%d@." result
