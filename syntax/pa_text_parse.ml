(*
 * pa_text_parse.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

open Camlp4.PreCast
open Syntax
open Pa_text_types

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type converter =
  | Constant of Ast.expr
  | Function of Ast.expr
  | Position
  | Identity

type charset_atom =
  | Ca_variable of Loc.t * string * bool
  | Ca_range of Loc.t * Text.t * Text.t
  | Ca_literal of Loc.t * Text.t

type charset = charset_atom list

type parse_tree =
  | Literal of Loc.t * Text.t
  | Repeat of Loc.t * parse_tree * int * int option * greediness
  | Concat of Loc.t * parse_tree * parse_tree
  | Alternative of Loc.t * parse_tree * parse_tree
  | Bind of Loc.t * parse_tree * string * converter
  | Charset of Loc.t * charset * bool
  | Meta of Loc.t * Text.t * Text.t option
  | Variable of Loc.t * string * bool
  | Backward_reference of Loc.t * string
  | Mode of Loc.t * mode * bool
  | Look of Loc.t * direction * parse_tree * bool
  | Group of Loc.t * parse_tree
  | Condition of Loc.t * string * parse_tree * parse_tree option

(* +-----------------------------------------------------------------+
   | Grammar of regular expression                                   |
   +-----------------------------------------------------------------+ *)

let regexp_eoi = Gram.Entry.mk "regexp_eoi"

EXTEND Gram
  GLOBAL: regexp_eoi;

  utf8_string:
    [ [ s = STRING ->
          match Text.check s with
            | Some error ->
                Loc.raise _loc (Failure("invalid UTF-8 string: " ^ error))
            | None ->
                s
      ] ];

  range:
    [ [ a = INT ->
          let a = int_of_string a in
          if a < 0 then
            Loc.raise _loc (Failure "range bounds must be positive number")
          else
            (a, Some a)
      | a = INT; "-"; b = INT ->
          let a = int_of_string a and b = int_of_string b in
          if a < 0 || b < a then
            Loc.raise _loc (Failure "invalid range bounds")
          else
            (a, Some b)
      | a = INT; "+" ->
          let a = int_of_string a in
          if a < 0 then
            Loc.raise _loc (Failure "range bounds must be positive number")
          else
            (a, None)
      ] ];

  state:
    [ [ "!" -> false | -> true ] ];

  charset_atom:
    [ [ a = utf8_string; ["-" | ".."]; b = utf8_string ->
          if Text.length a <> 1 || Text.length b <> 1 then
            Loc.raise _loc (Failure("UTF-8 string literals in charset range must contain only one unicode character"))
          else if Text.code a < Text.code b then
            Ca_range(_loc, a, b)
          else
            Loc.raise _loc (Failure "invalid charset: the upper limit must be greater than the lower limit")
      | s = utf8_string ->
          Ca_literal(_loc, s)
      | st = state; id = LIDENT ->
          Ca_variable(_loc, id, st)
      | st = state; id = UIDENT ->
          Ca_variable(_loc, id, st)
      ] ];

  charset:
    [ [ l = LIST0 charset_atom -> l ] ];

  mode:
    [ [ mode = LIDENT ->
          match mode with
            | "i" | "caseless" -> Caseless
            | "m" | "multiline" -> Multiline
            | "s" | "singleline" | "dotall" -> Dot_all
            | _ -> Loc.raise _loc (Failure(Printf.sprintf "invalid mode: '%s'" mode))
      ] ];

  regexp:
    [ [ r = SELF; "as"; i = LIDENT;
        conv =
          OPT [ ":"; s = LIDENT -> Function <:expr< $lid: s ^ "_of_string"$ >>
              | ":="; e = expr -> Function e
              | "="; e = expr -> Constant e ] ->
            Bind(_loc, r, i, match conv with Some c -> c | None -> Identity)
      | r1 = SELF; "|"; r2 = SELF -> Alternative(_loc, r1, r2)
      | r1 = SELF; r2 = SELF -> Concat(_loc, r1, r2) ]

    | "postop" NONA
        [ r = SELF; "*" -> Repeat(_loc, r, 0, None, Greedy)
        | r = SELF; "+" -> Repeat(_loc, r, 1, None, Greedy)
        | r = SELF; "?" -> Repeat(_loc, r, 0, Some 1, Greedy)
        | r = SELF; "{"; (a, b) = range; "}" -> Repeat(_loc, r, a, b, Greedy)
        | r = SELF; "*?" -> Repeat(_loc, r, 0, None, Lazy)
        | r = SELF; "+?" -> Repeat(_loc, r, 1, None, Lazy)
        | r = SELF; "??" -> Repeat(_loc, r, 0, Some 1, Lazy)
        | r = SELF; "{"; (a, b) = range; "}"; "?" -> Repeat(_loc, r, a, b, Lazy)
        | r = SELF; "*+" -> Repeat(_loc, r, 0, None, Possessive)
        | r = SELF; "++" -> Repeat(_loc, r, 1, None, Possessive)
        | r = SELF; "?+" -> Repeat(_loc, r, 0, Some 1, Possessive)
        | r = SELF; "{"; (a, b) = range; "}"; "+" -> Repeat(_loc, r, a, b, Possessive) ]

    | "preop" NONA
        [ "\\"; id = LIDENT -> Backward_reference (_loc, id) ]

    | "simple" NONA
        [ "["; cs = charset; "]" ->
            Charset(_loc, cs, true)
        | "[^"; cs = charset; "]" ->
            Charset(_loc, cs, false)
        | s = utf8_string ->
            Literal(_loc, s)
        | "_" ->
            Meta(_loc, ".", None)
        | st = state; i = LIDENT ->
            Variable(_loc, i, st)
        | st = state; i = UIDENT ->
            Variable(_loc, i, st)
        | "^" ->
            Meta(_loc, "^", None)
        | "$" ->
            Meta(_loc, "$", None)
        | "&+"; mode = mode ->
            Mode(_loc, mode, true)
        | "&-"; mode = mode ->
            Mode(_loc, mode, false)
        | "@"; name = LIDENT ->
            Bind(_loc, Literal(_loc, ""), name, Position)
        | "("; r = SELF; ")" ->
            Group(_loc, r)
        | "<"; r = SELF ->
            Look(_loc, Behind, r, true)
        | "<!"; r = SELF ->
            Look(_loc, Behind, r, false)
        | ">"; r = SELF ->
            Look(_loc, Ahead, r, true)
        | ">!"; r = SELF ->
            Look(_loc, Ahead, r, false)
        | "if"; id = LIDENT; "then"; r_then = SELF; r_else = maybe_else ->
            Condition(_loc, id, r_then, r_else)
        ] ];

  maybe_else:
    [ [ "else"; r = regexp -> Some r
      | -> None ] ];

  regexp_eoi:
    [ [ re = regexp; `EOI -> re ] ];
END

(* +-----------------------------------------------------------------+
   | Manipulation                                                    |
   +-----------------------------------------------------------------+ *)

let collect_regexp_bindings ast =
  let rec loop n acc = function
    | Literal _ | Variable _ | Charset _ | Meta _ | Backward_reference _ | Mode _ ->
        (n, acc)
    | Group(_, r) ->
        loop n acc r
    | Look(_, _, r, _) ->
        loop n acc r
    | Repeat(_, r, _, _, _) ->
        loop n acc r
    | Concat(_, r1, r2) ->
        let n, acc = loop n acc r1 in
        loop n acc r2
    | Alternative(_, r1, r2) ->
        let n, acc = loop n acc r1 in
        loop n acc r2
    | Bind(_loc, r, id, conv) ->
        loop (n + 1) ((_loc, id, n, conv) :: acc) r
    | Condition(_, _, r_then, None) ->
        loop n acc r_then
    | Condition(_, _, r_then, Some r_else) ->
        let n, acc = loop n acc r_then in
        loop n acc r_else
  in
  snd (loop 1 [] ast)

(* +-----------------------------------------------------------------+
   | Parsing                                                         |
   +-----------------------------------------------------------------+ *)

let parse loc contents =
  Gram.parse_string regexp_eoi loc contents
