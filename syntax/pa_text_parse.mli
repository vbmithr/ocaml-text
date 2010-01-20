(*
 * pa_text_parse.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(** Parsing of regular expression quotations *)

open Camlp4.PreCast
open Pa_text_types

(** {6 Parse tree} *)

(** Variable converters. A converter is used to set how capture
    variables are bound. *)
type converter =
  | Constant of Ast.expr
      (** Set the variable to this expression, independantly of the
          matched string. *)
  | Function of Ast.expr
      (** Maps the result of a capture with the given function. *)
  | Position
      (** Returns the position of the matched string. *)
  | Identity
      (** No convertion. Binds the variable to captured string. *)

(** Atom in a range (between "[" and "]"): *)
type charset_atom =
  | Ca_variable of Loc.t * string * bool
      (** [Ca_variable(loc, id, state)] *)
  | Ca_range of Loc.t * Text.t * Text.t
      (** [Ca_range(loc, min, max)] matches characters with a
          code-point between [min] and [max]. *)
  | Ca_literal of Loc.t * Text.t
      (** A literal text, that match any character that belong to
          him *)

type charset = charset_atom list

(** AST of a parsed regular expression *)
type parse_tree =
  | Literal of Loc.t * Text.t
      (** A literal string. *)
  | Repeat of Loc.t * parse_tree * int * int option * greediness
      (** [Pt_repeat(loc, pt, min, max, greediness)] *)
  | Concat of Loc.t * parse_tree * parse_tree
      (** Concatenates two regular expression *)
  | Alternative of Loc.t * parse_tree * parse_tree
      (** Union of regular expression *)
  | Bind of Loc.t * parse_tree * string * converter
      (** [Pt_bind(loc, pt, id, conv)] Bind a regular expression to
          an identifier *)
  | Charset of Loc.t * charset * bool
      (** [Pt_charset(loc, charset, state)] defines a character
          set. If [state] is [false], the charset is negated. *)
  | Meta of Loc.t * Text.t * Text.t option
      (** [Pt_meta(loc, normal, negated)] negated is the negation of
          normal *)
  | Variable of Loc.t * string * bool
      (** [Pt_variable(loc, id, state)] inline the variable [id] from
          an environment. If [state] is [false], it tries to negate
          the contents of [id]. *)
  | Backward_reference of Loc.t * string
      (** [Pt_backward_reference(loc, id)] *)
  | Mode of Loc.t * mode * bool
      (** [Pt_mode(loc, mode, state)] enable or disable a mode. If
          [state] is [true], the mode is enabled, otherwise it is
          disabled. *)
  | Look of Loc.t * direction * parse_tree * bool
      (** [Pt_look(loc, dir, pt, state)] defines a look around. *)
  | Group of Loc.t * parse_tree
      (** [Group pt] represent a regular expression that cannot be
          inlined. *)
  | Condition of Loc.t * string * parse_tree * parse_tree option
      (** [Condition(loc, id, r_then, r_else)] is [r_then] if the
          capture variable [id] matched something, and r_else
          otherwise. *)

(** {6 Manipulation} *)

val collect_regexp_bindings : parse_tree -> (Loc.t * string * int * converter) list
  (** Returns the list of variables contained in the given parse-tree,
      with their group number and converter *)

(** {6 Parsing} *)

val parse : Loc.t -> string -> parse_tree
  (** [parse loc string] parses the given string into a parse-tree. *)
