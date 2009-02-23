(*
 * encoding_table.ml
 * -----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

type t =
  | Empty
      (* An empty table *)

  | Single of int * char
      (* [Single(key, value)] is a single key-value association *)

  | Segment of int * int * string
      (* [Segment(min, max, map)] is a segment. It maps integers
         [k+min] to the value [map.[k]] *)

  | Node of t * int * t
      (* [Node(left, m, right)] A node. The following property must be
         verified:

         max(keys(left)) <= m < min(keys(right)) *)

let string_of_rev_list l =
  let len = List.length l in
  let str = String.create len in
  let rec aux i = function
    | [] ->
        str
    | x :: l ->
        str.[i] <- x;
        aux (i - 1) l
  in
  aux (len - 1) l

(* - start is a key

   - values is a list of characters [vn; vn-1; ...; v1; v0] where v0
   is associated to start, v1 is associated to start+1, ..., vn is
   associated to start+n

   It returns returns corresponding table.
*)
let table_of_values start values = match values with
  | [] ->
      Empty

  | [value] ->
      Single(start, value)

  | values ->
      let str = string_of_rev_list values in
      Segment(start, start + String.length str - 1, str)

(* Returns an list of [Single] or [Segment] from an associative list
   sorted in increasing order.

   In the resulting list, segments are the largest as possible *)
let tables_of_assoc l =
  (* - [values] is a list of values associated to consecutive keys
     - [start] is the the first key of this list
     - [last] is the last visited key *)
  let rec aux start values last = function
    | [] ->
        [table_of_values start values]
    | (key, value) :: rest ->
        (* If two keys are equal, then drop one of them *)
        if key = last then
          aux start values last rest

        else begin
          (* There is no values, start a new list *)
          if values = [] then
            aux key [value] key rest

          (* If [key] is the successor of [last], then [value] can be
             added to [values] *)
          else if key = succ last then
            aux start (value :: values) key rest

          (* Otherwise we need to ``commit'' current values and continue *)
          else
            table_of_values start values :: aux key [value] key rest
        end
  in
  match l with
    | [] ->
        []
    | (key, value) :: l ->
        (* Remove empty sets *)
        List.filter ((!=) Empty) (aux key [value] key l)

(* - n is a integer
   - l is a list

   it returns the list of the firsts [n] elements of [l] and the
   rest *)
let rec split_at n l = match n, l with
  | 0, l ->
      ([], l)
  | n, x :: l ->
      let left, right = split_at (n - 1) l in
      (x :: left, right)
  | _, [] ->
      (* This should never happen *)
      assert false

(* Return the maximum of a non-empty sorted list of tables *)
let rec max_key = function
  | [Single(key, value)] ->
      key
  | [Segment(min, max, values)] ->
      max
  | _ :: tables ->
      max_key tables
  | _ ->
      assert false

(* Create a balanced table from a increasing list of tables *)
let rec table_of_tables = function
  | [] ->
      Empty
  | [table] ->
      table
  | tables ->
      let len = List.length tables in
      let left, right = split_at (len/2) tables in
      Node(table_of_tables left, max_key left, table_of_tables right)

let of_assoc l =
  table_of_tables (tables_of_assoc (List.sort Pervasives.compare l))

let rec make_reverse table i acc =
  if i = Array.length table then
    acc
  else
    make_reverse table (i + 1) ((table.(i), char_of_int i) :: acc)

let of_decoding_table table = of_assoc (make_reverse table 0 [])

let rec lookup key = function
  | Empty ->
      None
  | Single(key', value) ->
      if key = key' then
        Some value
      else
        None
  | Segment(min, max, values) ->
      if min <= key && key <= max then
        Some(String.unsafe_get values (key - min))
      else
        None
  | Node(left, m, right) ->
      if key <= m then
        lookup key left
      else
        lookup key right
