(*
 * pa_text_regexp.ml
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

open Camlp4.PreCast
open Pa_text_types

module Ast : sig

  type charset_atom =
    | Ca_range of Text.t * Text.t
    | Ca_literal of Text.t
    | Ca_posix of Text.t * bool
    | Ca_meta of Text.t

  type charset = charset_atom list

  type t =
      private
    | Literal of Text.t
    | Group of t
    | Capture of t
    | Repeat of t * int * int option * greediness
    | Concat of t list
    | Alternatives of t list
    | Charset of charset * bool
    | Posix of Text.t * bool
    | Meta of Text.t * Text.t option
    | Backward_reference of int
    | Mode of mode * bool
    | Look of direction * t * bool
    | Condition of int * t * t option

  val epsilon : t
  val literal : Text.t -> t
  val group : t -> t
  val capture : t -> t
  val repeat : t -> int -> int option -> greediness -> t
  val concat : t list -> t
  val alternatives : t list -> t
  val charset : charset -> bool -> t
  val posix : Text.t -> bool -> t
  val meta : Text.t -> Text.t option -> t
  val backward_reference : int -> t
  val mode : mode -> bool -> t
  val look : direction -> t -> bool -> t
  val condition : int -> t -> t option -> t

end = struct

  type charset_atom =
    | Ca_range of Text.t * Text.t
    | Ca_literal of Text.t
    | Ca_posix of Text.t * bool
    | Ca_meta of Text.t

  type charset = charset_atom list

  type t =
    | Literal of Text.t
    | Group of t
    | Capture of t
    | Repeat of t * int * int option * greediness
    | Concat of t list
    | Alternatives of t list
    | Charset of charset * bool
    | Posix of Text.t * bool
    | Meta of Text.t * Text.t option
    | Backward_reference of int
    | Mode of mode * bool
    | Look of direction * t * bool
    | Condition of int * t * t option

  (* +---------------------------------------------------------------+
     | Constructors                                                  |
     +---------------------------------------------------------------+ *)

  let epsilon = Literal ""
  let literal text = Literal text

  let group r =
    match r with
      | Group _ | Capture _ | Condition _ | Charset _ | Meta(".", None) -> r
      | _ -> Group r

  let capture r =
    match r with
      | Group r -> Capture r
      | _ -> Capture r

  let repeat r min max greediness =
    match r with
      | Literal "" -> epsilon
      | _ -> Repeat(r, min, max, greediness)

  let concat = function
    | [] ->
        epsilon
    | [r] ->
        r
    | l ->
        (* Inline concatenations: *)
        Concat(List.flatten
                 (List.map
                    (function
                       | Concat l -> l
                       | Group(Concat l) -> l
                       | re -> [re])
                    l))

  let alternatives = function
    | [] ->
        epsilon
    | [r] ->
        r
    | l ->
        (* Inline non-grouped alternatives: *)
        Alternatives(List.flatten
                       (List.map
                          (function
                             | Alternatives l -> l
                             | re -> [re])
                          l))

  let charset l state = Charset(l, state)
  let posix name state = Posix(name, state)
  let meta text ntext = Meta(text, ntext)
  let backward_reference n = Backward_reference n
  let mode mode state = Mode(mode, state)
  let look dir r state = Look(dir, r, state)
  let condition id r_then r_else =
    let r_then =
      match r_then with
        | Group r -> r
        | r -> r
    and r_else =
      match r_else with
        | Some(Group r) -> Some r
        | x -> x
    in
    Condition(id, r_then, r_else)
end

include Ast

(* +-----------------------------------------------------------------+
   | Manipulation                                                    |
   +-----------------------------------------------------------------+ *)

let rec negate = function
  | Literal _ -> None
  | Group r -> begin
      match negate r with
        | Some r -> Some(group r)
        | None -> None
    end
  | Capture r -> begin
      match negate r with
        | Some r -> Some(capture r)
        | None -> None
    end
  | Repeat _ -> None
  | Concat _ -> None
  | Alternatives _ -> None
  | Charset(cs, state) -> Some(charset cs (not state))
  | Posix(name, state) -> Some(posix name (not state))
  | Meta(a, None) -> None
  | Meta(a, Some b) -> Some(meta b (Some a))
  | Backward_reference _ -> None
  | Mode _ -> None
  | Look _ -> None
  | Condition _ -> None

(* +-----------------------------------------------------------------+
   | Parse tree -> regular expression                                |
   +-----------------------------------------------------------------+ *)

module P = Pa_text_parse

let of_parse_tree ~env ~parse_tree =
  (* [vars] is the mapping from capture to their index, and [n] is the
     next available index: *)
  let rec loop vars n = function
    | P.Group(_, r) ->
        let vars, n, r = loop vars n r in
        (vars, n, group r)
    | P.Literal(_, lit) ->
        (vars, n, literal lit)
    | P.Repeat(_, r, min, max, greediness) ->
        let vars, n, r = loop vars n r in
        (vars, n, repeat (group r) min max greediness)
    | P.Concat(_, r1, r2) ->
        let vars, n, r1 = loop vars n r1 in
        let vars, n, r2 = loop vars n r2 in
        (vars, n, concat [r1; r2])
    | P.Alternative(_, r1, r2) ->
        let vars, n, r1 = loop vars n r1 in
        let vars, n, r2 = loop vars n r2 in
        (vars, n, alternatives [r1; r2])
    | P.Bind(_, r, id, _) ->
        let vars = Pa_text_env.add id n vars in
        let vars, n, r = loop vars (n + 1) r in
        (vars, n, capture r)
    | P.Charset(_, cs, state) ->
        let l =
          List.map begin function
            | P.Ca_variable(_loc, var, state) -> begin
                match Pa_text_env.lookup var env with
                  | Some re ->
                       (* Try to negate the regular expression if
                          required: *)
                       let re =
                         if state then
                           re
                         else
                           match negate re with
                             | Some re ->
                                 re
                             | None ->
                                 Loc.raise _loc (Failure "cannot negate this regular expression")
                       in
                       (* Inline the variable if possible *)
                       let rec loop = function
                         | Group r | Capture r -> loop r
                         | Posix(name, state) ->
                             [Ca_posix(name, state)]
                         | Charset(atoms, state') ->
                             if state <> state' then
                               Loc.raise _loc (Failure "cannot inline a charset with a difference state")
                             else
                               atoms
                         | Literal txt ->
                             [Ca_literal txt]
                         | Meta(txt, _) ->
                             [Ca_meta txt]
                         | _ ->
                             Loc.raise _loc (Failure(var ^ " is not a charset or a literal"))
                       in
                       loop re
                  | None ->
                      Loc.raise _loc (Failure("unbounded variable: " ^ var))
              end
            | P.Ca_range(_loc, a, b) ->
                [Ca_range(a, b)]
            | P.Ca_literal(_loc, lit) ->
                [Ca_literal lit]
          end cs
        in
        (vars, n, charset (List.flatten l) state)
    | P.Meta(_, text, ntext) ->
        (vars, n, meta text ntext)
    | P.Variable(loc, id, state) -> begin
        match Pa_text_env.lookup id env with
          | Some re ->
              let re =
                if state then
                  re
                else
                  match negate re with
                    | Some re ->
                        re
                    | None ->
                        Loc.raise loc (Failure "cannot negate this regular expression")
              in
              (vars, n, re)
          | None ->
              Loc.raise loc (Failure("unbounded variable: " ^ id))
      end
    | P.Backward_reference(loc, id) -> begin
        try
          (vars, n, backward_reference (Pa_text_env.find id vars))
        with Not_found ->
          Loc.raise loc (Failure "invalid backward reference")
      end
    | P.Condition(loc, id, r_then, None) -> begin
        try
          let vars, n, r_then = loop vars n r_then in
          (vars, n, condition (Pa_text_env.find id vars) r_then None)
        with Not_found ->
          Loc.raise loc (Failure "invalid backward reference")
      end
    | P.Condition(loc, id, r_then, Some r_else) -> begin
        try
          let vars, n, r_then = loop vars n r_then in
          let vars, n, r_else = loop vars n r_else in
          (vars, n, condition (Pa_text_env.find id vars) r_then None)
        with Not_found ->
          Loc.raise loc (Failure "invalid backward reference")
      end
    | P.Mode(loc, mode, state) ->
        (vars, n, Ast.mode mode state)
    | P.Look(_, dir, r, state) ->
        let vars, n, r = loop vars n r in
        (vars, n, look dir r state)
  in
  let vars, n, re = loop Pa_text_env.empty 1 parse_tree in
  re

(* +-----------------------------------------------------------------+
   | Literal escaping                                                |
   +-----------------------------------------------------------------+ *)

(* Escape special characters in literals, and restore unicode
   quotations: *)
let escape text =
  let rec loop acc = function
    | ("\\" | "^" | "$" | "." | "[" | "|" | "(" | ")" | "?" | "*" | "+" | "{" as ch) :: l ->
        loop (ch :: "\\" :: acc) l
    | "\x00" :: "\x00" :: l -> begin
        match Pa_text_util.split_hexa_quotation l with
          | Some(txt, l) ->
              loop (txt :: "\\\\x" :: acc) l
          | None ->
              loop ("\x00\x00" :: acc) l
      end
    | x :: l ->
        loop (x :: acc) l
    | [] ->
        Text.rev_implode acc
  in
  loop [] (Text.explode text)

(* Same as [espace] but for text in charset (between "[" and "]"), and
   restore unicode quotations: *)
let escape_in_charset text =
  let rec loop acc = function
    | ("\\" | "-" | "[" | "]" | "^" as ch) :: l ->
        loop (ch :: "\\" :: acc) l
    | "\x00" :: "\x00" :: l -> begin
        match Pa_text_util.split_hexa_quotation l with
          | Some(txt, l) ->
              loop (txt :: "\\\\x" :: acc) l
          | None ->
              loop ("\x00\x00" :: acc) l
      end
    | x :: l ->
        loop (x :: acc) l
    | [] ->
        Text.rev_implode acc
  in
  loop [] (Text.explode text)

(* +-----------------------------------------------------------------+
   | Marshaling                                                      |
   +-----------------------------------------------------------------+ *)

let string_of_mode = function
  | Caseless -> "i"
  | Multiline -> "m"
  | Dot_all -> "s"

let to_string re =
  let buffer = Buffer.create 42 in
  let add str = Buffer.add_string buffer str in
  let addg = function
    | Greedy -> ()
    | Lazy -> add "?"
    | Possessive -> add "+"
  in
  let rec loop = function
    | Literal lit ->
        add (escape lit)
    | Group re ->
        add "(?:";
        loop re;
        add ")"
    | Capture re ->
        add "(";
        loop re;
        add ")"
    | Repeat(re, 0, None, g) ->
        loop re;
        add "*";
        addg g
    | Repeat(re, 1, None, g) ->
        loop re;
        add "+";
        addg g
    | Repeat(re, 0, Some 1, g) ->
        loop re;
        add "?";
        addg g
    | Repeat(re, min, Some max, g) ->
        loop re;
        add "{";
        add (string_of_int min);
        add ",";
        add (string_of_int max);
        add "}";
        addg g
    | Repeat(re, min, None, g) ->
        loop re;
        add "{";
        add (string_of_int min);
        add ",}";
        addg g
    | Concat l ->
        List.iter loop l
    | Alternatives(r :: l) ->
        loop r;
        List.iter (fun r -> add "|"; loop r) l
    | Alternatives [] ->
        assert false
    | Charset(cs, true) ->
        add "[";
        List.iter loop_charset cs;
        add "]"
    | Charset(cs, false) ->
        add "[^";
        List.iter loop_charset cs;
        add "]"
    | Posix(name, true) ->
        add "[[:";
        add name;
        add ":]]"
    | Posix(name, false) ->
        add "[[:^";
        add name;
        add ":]]"
    | Meta(t, nt) ->
        add t
    | Backward_reference n ->
        add "\\g{";
        add (string_of_int n);
        add "}"
    | Mode(mode, true) ->
        add "(?";
        add (string_of_mode mode);
        add ")"
    | Mode(mode, false) ->
        add "(?-";
        add (string_of_mode mode);
        add ")"
    | Look(Ahead, r, true) ->
        add "(?=";
        loop r;
        add ")"
    | Look(Ahead, r, false) ->
        add "(?!";
        loop r;
        add ")"
    | Look(Behind, r, true) ->
        add "(?<=";
        loop r;
        add ")"
    | Look(Behind, r, false) ->
        add "(?<!";
        loop r;
        add ")"
    | Condition(n, r_then, r_else) ->
        add "(?(";
        add (string_of_int n);
        add ")";
        loop r_then;
        begin
          match r_else with
            | Some r ->
                add "|";
                loop r;
            | None ->
                ()
        end;
        add ")"
  and loop_charset = function
    | Ca_range(a, b) ->
        add (escape_in_charset a);
        add "-";
        add (escape_in_charset b)
    | Ca_literal lit ->
        add (escape_in_charset lit)
    | Ca_posix(name, state) ->
        if state then
          add "[:"
        else
          add "[:^";
        add name;
        add ":]"
    | Ca_meta t ->
        add t
  in
  loop re;
  Buffer.contents buffer
