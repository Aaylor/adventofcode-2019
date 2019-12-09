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

  let of_int = function
    | 0 -> Position
    | 1 -> Immediate
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
    | Exit

  let opcode_size = function
    | Addition _ | Multiply _ | Less_than _ | Equals _ -> 4
    | Jump_if_true _ | Jump_if_false _ -> 3
    | Input _ | Output _ -> 2
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
    | 99 ->
      Exit
    | n ->
      failwith (Format.sprintf "Invalid opcode: %d@." n)
end

module Ctx = struct
  type t =
    { input: int array;
      input_ptr: int;
      output: int option; }

  let mk ?input () =
    { input = Option.value input ~default:[||];
      input_ptr = 0;
      output = None; }

  let get_input ctx =
    let data = ctx.input.(ctx.input_ptr) in
    data, { ctx with input_ptr = succ ctx.input_ptr }

  let get_output ctx =
    match ctx.output with
    | None -> assert false
    | Some output -> output

  let set_output ctx output =
    { ctx with output = Some output; }
end


(** {1 INTERPRETER} *)

let get ~stack ~idx ~mode () =
  match mode with
  | Mode.Immediate ->
    stack.(idx)
  | Mode.Position ->
    stack.(stack.(idx))

let store ~stack ~idx ~mode ~value () =
  match mode with
  | Mode.Immediate ->
    (* NOTE: According to the documentation, the mode for [storing] parameters
       can never be immediate. *)
    assert false
  | Mode.Position ->
    stack.(stack.(idx)) <- value

let rec eval ctx idx stack =
  Lwt_main.yield ();%lwt
  assert (idx >= 0);
  let opcode = Opcode.of_int stack.(idx) in
  let jump ~ctx ~idx () = eval ctx idx stack in
  let jump_next = jump ~idx:(Opcode.opcode_size opcode + idx) in
  match opcode with
  | Opcode.Addition (m1, m2, m3) ->
    let v1 = get ~stack ~idx:(idx + 1) ~mode:m1 () in
    let v2 = get ~stack ~idx:(idx + 2) ~mode:m2 () in
    store ~stack ~idx:(idx + 3) ~mode:m3 ~value:(v1 + v2) ();
    jump_next ~ctx ()
  | Opcode.Multiply (m1, m2, m3) ->
    let v1 = get ~stack ~idx:(idx + 1) ~mode:m1 () in
    let v2 = get ~stack ~idx:(idx + 2) ~mode:m2 () in
    store ~stack ~idx:(idx + 3) ~mode:m3 ~value:(v1 * v2) ();
    jump_next ~ctx ()
  | Opcode.Input mode ->
    let value, ctx' = Ctx.get_input ctx in
    store ~stack ~idx:(idx + 1) ~mode ~value ();
    jump_next ~ctx:ctx' ()
  | Opcode.Output mode ->
    let value = get ~stack ~idx:(idx + 1) ~mode () in
    let ctx' = Ctx.set_output ctx value in
    jump_next ~ctx:ctx' ()
  | Opcode.Jump_if_true (m1, m2) ->
    let v1 = get ~stack ~idx:(idx + 1) ~mode:m1 () in
    if v1 = 0
    then jump_next ~ctx ()
    else jump ~ctx ~idx:(get ~stack ~idx:(idx + 2) ~mode:m2 ()) ()
  | Opcode.Jump_if_false (m1, m2) ->
    let v1 = get ~stack ~idx:(idx + 1) ~mode:m1 () in
    if v1 = 0
    then jump ~ctx ~idx:(get ~stack ~idx:(idx + 2) ~mode:m2 ()) ()
    else jump_next ~ctx ()
  | Opcode.Less_than (m1, m2, m3) ->
    let v1 = get ~stack ~idx:(idx + 1) ~mode:m1 () in
    let v2 = get ~stack ~idx:(idx + 2) ~mode:m2 () in
    let value = if v1 < v2 then 1 else 0 in
    store ~stack ~idx:(idx + 3) ~mode:m3 ~value ();
    jump_next ~ctx ()
  | Opcode.Equals (m1, m2, m3) ->
    let v1 = get ~stack ~idx:(idx + 1) ~mode:m1 () in
    let v2 = get ~stack ~idx:(idx + 2) ~mode:m2 () in
    let value = if v1 = v2 then 1 else 0 in
    store ~stack ~idx:(idx + 3) ~mode:m3 ~value ();
    jump_next ~ctx ()
  | Opcode.Exit ->
    Lwt.return (Ctx.get_output ctx)


(** {1 SOLVER} *)

let rec permutations l =
  let n = List.length l in
  if n = 1 then
    [ l ]
  else
    let rec sub e = function
      | [] -> failwith "sub"
      | h :: t -> if h = e then t else h :: sub e t
    in
    let rec aux k =
      let e = List.nth l k in
      let subperms = permutations (sub e l) in
      let t = List.map (fun a -> e :: a) subperms in
      if k < n - 1
      then List.rev_append t (aux (k + 1))
      else t
    in
    aux 0

let do_eval stack input =
  let stack = Array.copy stack in
  let rec aux_eval input output =
    match input with
    | [] ->
      Lwt.return output
    | x :: xs ->
      let ctx = Ctx.mk ~input:[| x; output |] () in
      let%lwt output' = eval ctx 0 stack in
      aux_eval xs output'
  in
  aux_eval input 0

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
  let inputs = permutations [ 0; 1; 2; 3; 4 ] in
  let%lwt results = Lwt_list.map_p (do_eval stack) inputs in
  Lwt.return (List.hd (List.sort_uniq (fun x y -> y - x) results))

let () =
  let result = Lwt_main.run main in
  Logs.app (fun m -> m "RESULT: %d" result)
