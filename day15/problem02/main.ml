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

(** {1 MACHINE} *)

module Mode = struct
  type t =
    | Position
    | Immediate
    | Relative

  let of_int = function
    | 0 -> Position
    | 1 -> Immediate
    | 2 -> Relative
    | _ -> assert false
end

module Opcode = struct
  type t =
    | Addition of Mode.t * Mode.t * Mode.t
    | Multiply of Mode.t * Mode.t * Mode.t
    | Input of Mode.t
    | Output of Mode.t
    | Jump_if_true of Mode.t * Mode.t
    | Jump_if_false of Mode.t * Mode.t
    | Less_than of Mode.t * Mode.t * Mode.t
    | Equals of Mode.t * Mode.t * Mode.t
    | Set_relative of Mode.t
    | Exit

  let opcode_size = function
    | Addition _ | Multiply _ | Less_than _ | Equals _ -> 4
    | Jump_if_true _ | Jump_if_false _ -> 3
    | Input _ | Output _ | Set_relative _ -> 2
    | Exit -> 1

  let get_mode n =
    (* Returns the mode according to the digit. *)
    Mode.of_int (n mod 10),
    (* And the next number to check. *)
    n / 10

  let of_int n =
    (* First step, we extract the opcode. *)
    let opcode = n mod 100 in
    let n' = n / 100 in
    match opcode with
    | 1 ->
      let m1, n1 = get_mode n' in
      let m2, n2 = get_mode n1 in
      let m3, _ = get_mode n2 in
      Addition (m1, m2, m3)
    | 2 ->
      let m1, n1 = get_mode n' in
      let m2, n2 = get_mode n1 in
      let m3, _ = get_mode n2 in
      Multiply (m1, m2, m3)
    | 3 ->
      let m1, _ = get_mode n' in
      Input m1
    | 4 ->
      let m1, _ = get_mode n' in
      Output m1
    | 5 ->
      let m1, n1 = get_mode n' in
      let m2, _ = get_mode n1 in
      Jump_if_true (m1, m2)
    | 6 ->
      let m1, n1 = get_mode n' in
      let m2, _ = get_mode n1 in
      Jump_if_false (m1, m2)
    | 7 ->
      let m1, n1 = get_mode n' in
      let m2, n2 = get_mode n1 in
      let m3, _ = get_mode n2 in
      Less_than (m1, m2, m3)
    | 8 ->
      let m1, n1 = get_mode n' in
      let m2, n2 = get_mode n1 in
      let m3, _ = get_mode n2 in
      Equals (m1, m2, m3)
    | 9 ->
      let m1, _ = get_mode n' in
      Set_relative m1
    | 99 ->
      Exit
    | n ->
      failwith (Format.sprintf "Invalid opcode: %d@." n)
end


module Machine = struct
  type t =
    { id: int;
      stack: int array;
      mutable idx: int;
      mutable relative_base: int; }

  let create ?(id = 0) stack =
    { id;
      stack;
      idx = 0;
      relative_base = 0 }

  let copy m =
    { m  with stack = Array.copy m.stack }

  let get ~machine ~at ~mode () =
    match mode with
    | Mode.Immediate ->
      machine.stack.(machine.idx + at)
    | Mode.Position ->
      machine.stack.(machine.stack.(machine.idx + at))
    | Mode.Relative ->
      let position = machine.stack.(machine.idx + at) + machine.relative_base in
      machine.stack.(position)

  let store ~machine ~at ~mode ~value () =
    match mode with
    | Mode.Immediate ->
      (* NOTE: According to the documentation, the mode for [storing] parameters
         can never be immediate. *)
      assert false
    | Mode.Position ->
      machine.stack.(machine.stack.(machine.idx + at)) <- value
    | Mode.Relative ->
      let position = machine.stack.(machine.idx + at) + machine.relative_base in
      machine.stack.(position) <- value

  let rec eval ~input machine =
    assert (machine.idx >= 0);
    let opcode = Opcode.of_int machine.stack.(machine.idx) in
    let jump ~idx () = machine.idx <- idx; eval ~input machine in
    let jump_next = jump ~idx:(machine.idx + Opcode.opcode_size opcode) in
    match opcode with
    | Opcode.Addition (m1, m2, m3) ->
      let v1 = get ~machine ~at:1 ~mode:m1 () in
      let v2 = get ~machine ~at:2 ~mode:m2 () in
      store ~machine ~at:3 ~mode:m3 ~value:(v1 + v2) ();
      jump_next ()
    | Opcode.Multiply (m1, m2, m3) ->
      let v1 = get ~machine ~at:1 ~mode:m1 () in
      let v2 = get ~machine ~at:2 ~mode:m2 () in
      store ~machine ~at:3 ~mode:m3 ~value:(v1 * v2) ();
      jump_next ()
    | Opcode.Input mode ->
      let value = input () in
      store ~machine ~at:1 ~mode ~value ();
      jump_next ()
    | Opcode.Output mode ->
      let value = get ~machine ~at:1 ~mode () in
      machine.idx <- machine.idx + Opcode.opcode_size opcode;
      Some value
    | Opcode.Jump_if_true (m1, m2) ->
      let v1 = get ~machine ~at:1 ~mode:m1 () in
      if v1 = 0
      then jump_next ()
      else jump ~idx:(get ~machine ~at:2 ~mode:m2 ()) ()
    | Opcode.Jump_if_false (m1, m2) ->
      let v1 = get ~machine ~at:1 ~mode:m1 () in
      if v1 = 0
      then jump ~idx:(get ~machine ~at:2 ~mode:m2 ()) ()
      else jump_next ()
    | Opcode.Less_than (m1, m2, m3) ->
      let v1 = get ~machine ~at:1 ~mode:m1 () in
      let v2 = get ~machine ~at:2 ~mode:m2 () in
      let value = if v1 < v2 then 1 else 0 in
      store ~machine ~at:3 ~mode:m3 ~value ();
      jump_next ()
    | Opcode.Equals (m1, m2, m3) ->
      let v1 = get ~machine ~at:1 ~mode:m1 () in
      let v2 = get ~machine ~at:2 ~mode:m2 () in
      let value = if v1 = v2 then 1 else 0 in
      store ~machine ~at:3 ~mode:m3 ~value ();
      jump_next ()
    | Opcode.Set_relative m1 ->
      let value = get ~machine ~at:1 ~mode:m1 () in
      machine.relative_base <- machine.relative_base + value;
      jump_next ()
    | Opcode.Exit ->
      None

  let is_exit_opcode machine =
    match Opcode.of_int machine.stack.(machine.idx) with
    | Opcode.Exit -> true
    | _ -> false
end


(** {1 ROBOT} *)

module Coordinate = struct
  type t = C of int * int
  let compare (C (x1, y1)) (C (x2, y2)) =
    let r = x1 - x2 in
    if r = 0 then y1 - y2 else r
  let pp fmt (C (x, y)) = Format.fprintf fmt "(%d, %d)" x y
end

module Tile = struct
  type t =
    | Wall
    | Path
end

module Shared_coordinate_map = struct
  type t = (Coordinate.t, Tile.t) Hashtbl.t * Lwt_mutex.t

  let create () = Hashtbl.create 139, Lwt_mutex.create ()

  let set (tbl, mutex) coordinate tile =
    Lwt_mutex.with_lock mutex @@ fun () ->
    match Hashtbl.find_opt tbl coordinate with
    | None -> Lwt.wrap3 Hashtbl.replace tbl coordinate tile
    | Some _elt -> Lwt.fail (Invalid_argument "coordinate already set")

  let get (tbl, mutex) coordinate =
    Lwt_mutex.with_lock mutex @@ fun () ->
    Lwt.wrap2 Hashtbl.find_opt tbl coordinate

  let set_unsafe (tbl, _) = Hashtbl.replace tbl
  let get_unsafe (tbl, _) = Hashtbl.find_opt tbl
end

module Coordinate_set = Set.Make(Coordinate)

module Movement = struct
  type t =
    | North
    | South
    | West
    | East

  let to_int = function
    | North -> 1
    | South -> 2
    | West -> 3
    | East -> 4
end

module Droid_answer = struct
  type t =
    | Wall_hit
    | Moved_one_step of bool

  let of_int = function
    | 0 -> Wall_hit
    | 1 -> Moved_one_step false
    | 2 -> Moved_one_step true
    | _ -> assert false
end


(** {1 SOLVER} *)

let mvar_solution = Lwt_mvar.create_empty ()

let get_all_coordinates coordinate =
  (* In order: NORTH, SOUTH, EAST, WEST. *)
  let Coordinate.C (current_x, current_y) = coordinate in
  Coordinate.C (current_x + 1, current_y),
  Coordinate.C (current_x - 1, current_y),
  Coordinate.C (current_x, current_y + 1),
  Coordinate.C (current_x, current_y - 1)

let rec explore_move ~map ~droid ~move ~steps machine =
  match%lwt Shared_coordinate_map.get map droid with
  | Some _tile ->
    (* The tile was already explored before. We terminate immediately the
       thread by returning. *)
    Lwt.return_unit
  | None ->
    let machine' =
      (* The machine contains mutable data. We must ensure to copy the machine
         in order to copy the mutable data; otherwise it will not work with
         multiple threads. *)
      Machine.copy machine
    in
    let input () =
      (* The move to send to the robot. *)
      Movement.(to_int move)
    in
    begin
      match Machine.eval ~input machine' with
      | None ->
        (* By precondition: for that problem, the machine can't return None. *)
        assert false
      | Some answer ->
        begin
          match Droid_answer.of_int answer with
          | Droid_answer.Wall_hit ->
            (* The droid has hit a wall: there is nothing to do here else,
               except terminating the thread.
               It is not necessary to start another threads here, they were
               already created. *)
            Lwt.return_unit
          | Droid_answer.Moved_one_step is_oxygen_tile ->
            begin
              if is_oxygen_tile then
                (* On this problem, we set the coordinate of the oxygen tile in
                   the mailbox; but we let the robot continue the exploration in
                   order to get the complete map. *)
                Lwt_mvar.put mvar_solution droid
              else
                (* Otherwise, we do nothing. *)
                Lwt.return_unit
            end;%lwt
            (* An then, we continue the exploration with the robot:
               - register the tile in the coordinate map,
               - start new threads to explore all other tiles.*)
            Shared_coordinate_map.set map droid Tile.Path;%lwt
            explore ~steps ~map ~droid machine'
        end
    end

and explore ?(steps = 0) ~map ~droid machine =
  let north, south, east, west = get_all_coordinates droid in
  let steps = succ steps in
  Lwt.join
    [ explore_move ~map ~droid:north ~move:Movement.North ~steps machine;
      explore_move ~map ~droid:south ~move:Movement.South ~steps machine;
      explore_move ~map ~droid:east ~move:Movement.East ~steps machine;
      explore_move ~map ~droid:west ~move:Movement.West ~steps machine ]

let start machine =
  let droid = Coordinate.C (0, 0) in
  let map = Shared_coordinate_map.create () in
  Shared_coordinate_map.set map droid Tile.Path;%lwt
  explore ~map ~droid machine;%lwt
  Lwt.return map

let add_next_tile ~map coordinate acc =
  match Shared_coordinate_map.get_unsafe map coordinate with
  | None | Some Tile.Wall -> acc
  | Some Tile.Path -> (coordinate :: acc)

let put_next_tiles ~seen ~coordinate ~queue ~map () =
  (* Add the next tiles to the queue if necessary.
     For each next tiles:
     - if the tile is a wall, has already been seen, or does not exists, do
       not add the tile.
     - otherwise, add the tile to the queue. *)
  let add_next_tile next_coordinate =
    match Shared_coordinate_map.get_unsafe map coordinate with
    | None | Some Tile.Wall -> ()
    | Some Tile.Path ->
      if not (Coordinate_set.mem next_coordinate seen)
      then Queue.push next_coordinate queue
  in
  let north, south, east, west = get_all_coordinates coordinate in
  add_next_tile north;
  add_next_tile south;
  add_next_tile east;
  add_next_tile west

let rec spread_oxygen ~steps ~seen ~queue_coordinate ~map () =
  let next_queue_coordinate =
    (* Store the coordinates that has to be check in the next round. *)
    Queue.create ()
  in
  let rec aux_spread_oxygen ~seen () =
    match Queue.is_empty queue_coordinate with
    | true ->
      if Queue.is_empty next_queue_coordinate then
        (* There is no more coordinates to visit, returns the number of steps
           done.*)
        Lwt.return steps
      else
        (* There is no more coordinates to visit on that round; but another
           round is coming with the non-empty next queue coordinates. *)
        spread_oxygen
          ~steps:(succ steps)
          ~seen
          ~queue_coordinate:next_queue_coordinate
          ~map
          ()
    | false ->
      (* Pop the coordinate, register all near tiles when necessary, and
         continue on the current queue. *)
      let coordinate = Queue.pop queue_coordinate in
      put_next_tiles ~seen ~coordinate ~queue:next_queue_coordinate ~map ();
      let seen = Coordinate_set.add coordinate seen in
      aux_spread_oxygen ~seen ()
  in
  aux_spread_oxygen ~seen ()

let start_spread_oxygen ~coordinate ~map () =
  let tile_queue = Queue.create () in
  Queue.push coordinate tile_queue;
  spread_oxygen
    ~steps:0
    ~seen:(Coordinate_set.singleton coordinate)
    ~queue_coordinate:tile_queue
    ~map
    ()

let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt input_line =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_line
  in
  let stack =
    Array.of_list
    @@ List.map int_of_string
    @@ String.split_on_char ',' input_line
  in
  let machine = Machine.create ~id:0 stack in
  let%lwt map = start machine in
  let%lwt coordinate = Lwt_mvar.take mvar_solution in
  start_spread_oxygen ~coordinate ~map ()

let () =
  let result = Lwt_main.run main in
  Format.printf "%d@." result
