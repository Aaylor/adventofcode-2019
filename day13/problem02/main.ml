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

(** {1 DATATYPES} *)

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

module Coordinate = struct
  type t = C of int * int
  let compare (C (x1, y1)) (C (x2, y2)) =
    let r = x1 - x2 in
    if r = 0 then y1 - y2 else r
end
module Coordinate_map = Map.Make(Coordinate)
module Coordinate_set = Set.Make(Coordinate)

module Tile = struct
  type t =
    | Empty
    | Wall
    | Block
    | Horizontal_paddle
    | Ball

  let of_int = function
    | 0 -> Empty
    | 1 -> Wall
    | 2 -> Block
    | 3 -> Horizontal_paddle
    | 4 -> Ball
    | _ -> assert false

  let show t =
    Format.printf " TILE: %s@."
      (match t with
       | Empty -> "Empty"
       | Wall -> "Wall"
       | Block -> "Block"
       | Horizontal_paddle -> "Paddle"
       | Ball -> "Ball")
end

module Machine = struct
  type t =
    { id: int;
      stack: int array;
      mutable idx: int;
      mutable relative_base: int;
      input: int Queue.t }

  let create ?(id = 0) stack =
    { id;
      stack;
      idx = 0;
      relative_base = 0;
      input = Queue.create () }

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


(** {1 SOLVER} *)

type state =
  { score: int;
    horizontal_paddle: Coordinate.t;
    ball: Coordinate.t }

let start machine =
  let rec run ~map ~machine ~state () =
    let input () =
      let (Coordinate.C (paddle_x, _)) = state.horizontal_paddle in
      let (Coordinate.C (ball_x, _)) = state.ball in
      compare ball_x paddle_x
    in
    match Machine.eval ~input machine with
    | None ->
      state.score
    | Some x ->
      let y = Option.get (Machine.eval ~input machine) in
      let v = Option.get (Machine.eval ~input machine) in
      let map', state  =
        if x = -1 && y = 0 then
          map, { state with score = v }
        else
          let tile = Tile.of_int v in
          let coordinate = Coordinate.C (x, y) in
          match tile with
          | Tile.Empty ->
            Coordinate_map.remove coordinate map,
            state
          | Tile.Horizontal_paddle ->
            Coordinate_map.add coordinate tile map,
            { state with horizontal_paddle = coordinate }
          | Tile.Ball ->
            Coordinate_map.add coordinate tile map,
            { state with ball = coordinate }
          | tile ->
            Coordinate_map.add coordinate tile map,
            state
      in
      run ~map:map' ~machine ~state ()
  in
  let initial_state =
    { score = 0;
      horizontal_paddle = Coordinate.C (-1, -1);
      ball = Coordinate.C (-1, -1) }
  in
  run
    ~map:Coordinate_map.empty
    ~machine
    ~state:initial_state
    ()

let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt input_line =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_line
  in
  let stack =
    let stack' = Array.make 100_000 0 in
    List.iteri (fun i elt -> stack'.(i) <- elt)
    @@ List.map int_of_string
    @@ String.split_on_char ',' input_line;
    stack'
  in
  stack.(0) <- 2;
  let machine = Machine.create ~id:0 stack in
  Lwt.return (start machine)

let () =
  let result = Lwt_main.run main in
  Format.printf "%d@." result
