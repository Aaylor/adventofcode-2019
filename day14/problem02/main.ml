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

module Surplus: sig
  val add: string -> int64 -> unit
  val get: string -> int64
  val remove: string -> int64 -> unit
end = struct
  let surplus_tbl = Hashtbl.create 7

  let add element surplus =
    match Hashtbl.find_opt surplus_tbl element with
    | None ->
      Hashtbl.replace surplus_tbl element surplus
    | Some old_surplus ->
      Hashtbl.replace surplus_tbl element (Int64.add old_surplus surplus)

  let get element =
    match Hashtbl.find_opt surplus_tbl element with
    | None -> 0L
    | Some surplus -> surplus

  let remove element surplus =
    match Hashtbl.find_opt surplus_tbl element with
    | None ->
      Hashtbl.replace surplus_tbl element Int64.(mul surplus minus_one)
    | Some old ->
      Hashtbl.replace surplus_tbl element (Int64.sub old surplus)
end

let int64_min i1 i2 = if i1 < i2 then i1 else i2

let rec get_needed_ore ~reactions ~element ~units () =
  if element = "ORE" then
    (* We do not need to go further: just return the number of required ORE. *)
    units
  else
    (* We first check the surplus, and check how many elements have to be
       procuded. *)
    let reuse = int64_min units (Surplus.get element) in
    let units = Int64.sub units reuse in
    Surplus.remove element reuse;
    let to_produce, ingredients = Aoc.String_map.find element reactions in
    let multiple =
      Int64.of_float
      @@ ceil
      @@ Int64.to_float units /. Int64.to_float to_produce
    in
    (* Evaluate the needed ore, by iterating over all necessary ingredients. *)
    let needed_ore =
      Aoc.String_map.fold
        (fun element units ore ->
           Int64.add ore @@
           get_needed_ore
             ~reactions
             ~element
             ~units:(Int64.mul multiple units)
             ())
        ingredients
        0L
    in
    Surplus.add
      element
      Int64.(sub (mul multiple to_produce) units);
    needed_ore

let get_needed_ore_for_fuel ~reactions ~units () =
  get_needed_ore
    ~reactions
    ~element:"FUEL"
    ~units
    ()

let rec binary_search ~reactions ~low ~high () =
  (* Simply do a binary tree in order to find what is the number of fuel units
     created with the ore. *)
  if Int64.sub high low <> 1L then
    let mid = Int64.(div (add high low) 2L) in
    let needed_ore = get_needed_ore_for_fuel ~reactions ~units:mid () in
    if Int64.compare needed_ore 1_000_000_000_000L > 0 then
      binary_search ~reactions ~low ~high:mid ()
    else
      binary_search ~reactions ~low:mid ~high ()
  else
    low



(** {1 SOLVER} *)

let parse_line input acc =
  match Str.(split (regexp " => ") input) with
  | [ ingredients; input ] ->
    Scanf.sscanf input "%Ld %s" @@ fun units element ->
    let ingredients =
      List.fold_left
        (fun acc ingredient ->
           let ingredient = String.trim ingredient in
           Scanf.sscanf ingredient "%Ld %s" @@ fun icost ielement ->
           Aoc.String_map.add ielement icost acc)
        Aoc.String_map.empty
        (String.split_on_char ',' ingredients)
    in
    Aoc.String_map.add element (units, ingredients) acc
  | _ ->
    assert false

let main =
  let%lwt descr = Lwt_unix.openfile "input.txt"  Unix.[ O_RDONLY ] 0640 in
  let%lwt reactions =
    Lwt_io.of_fd ~mode:Lwt_io.input descr
    |> Lwt_io.read_lines
    |> Fun.flip (Lwt_stream.fold parse_line) Aoc.String_map.empty
  in
  Lwt.return (binary_search ~reactions ~low:0L ~high:1_000_000_000_000L ())

let () =
  let result = Lwt_main.run main in
  Format.printf "%Ld@." result
