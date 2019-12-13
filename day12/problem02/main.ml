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

  let equal t1 t2 = t1.x = t2.x && t1.y = t2.y && t1.z = t2.z
end

module Moon = struct
  type t =
    { coordinates: Coordinate3D.t;
      velocity: Coordinate3D.t }

  let make ~x ~y ~z =
    { coordinates = Coordinate3D.{ x; y; z };
      velocity = Coordinate3D.{ x = 0; y = 0; z = 0 } }

  let apply_gravity_aux v1 v2 =
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

  let equal m1 m2 =
    Coordinate3D.equal m1.coordinates m2.coordinates
    && Coordinate3D.equal m1.velocity m2.velocity
end



let rec gcd a b =
  if b = 0 then
    a
  else
    let r = a mod b in
    if r = 0 then b else gcd b r

let lcm a b = (a * b) / gcd a b

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

let update_repeat ~steps check repeat =
  match repeat with
  | None -> if check then Some steps else None
  | Some steps -> Some steps

let check_coordinates
    ~initial_moons
    ~moons'
    ~steps
    ~repeat:(repeat_x, repeat_y, repeat_z) =
  let check_x, check_y, check_z =
    List.fold_right2
      (fun initial_moon moon' (check_x, check_y, check_z) ->
         let open Coordinate3D in
         let open Moon in
         let check_x =
           if initial_moon.coordinates.x <> moon'.coordinates.x ||
              moon'.velocity.x <> 0
           then false
           else check_x
         in
         let check_y =
           if initial_moon.coordinates.y <> moon'.coordinates.y ||
              moon'.velocity.y <> 0
           then false
           else check_y
         in
         let check_z =
           if initial_moon.coordinates.z <> moon'.coordinates.z ||
              moon'.velocity.z <> 0
           then false
           else check_z
         in
         check_x, check_y, check_z)
      initial_moons
      moons'
      (true, true, true)
  in
  update_repeat ~steps check_x repeat_x,
  update_repeat ~steps check_y repeat_y,
  update_repeat ~steps check_z repeat_z

let steps ~moons () =
  (* Instead of doing bruteforcing (which does not work with this problem), we
     want to check the revolution foreach coordinate, and take the LCM of the
     three coordinates.

     Instead of doing three loops (one for each coordinate), do the loop only
     once, an find the revolution for the three coordinates. *)
  let rec aux_steps ~moons' ~repeat steps =
    (* Update the moons positions. *)
    let moons' =
      permutations_n2 moons'
      |> List.map Moon.apply_velocity
    in
    match check_coordinates ~initial_moons:moons ~moons' ~steps ~repeat with
    | Some repeat_x, Some repeat_y, Some repeat_z ->
      lcm repeat_x repeat_y
      |> lcm repeat_z
    | repeat ->
      aux_steps ~moons' ~repeat (succ steps)
  in
  aux_steps ~moons':moons ~repeat:(None, None, None) 1

let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt moons =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_lines
    |> Lwt_stream.map parse_moon
    |> Lwt_stream.to_list
  in
  let steps = steps ~moons () in
  Lwt.return steps

let () =
  let result = Lwt_main.run main in
  Format.printf "%d@." result
