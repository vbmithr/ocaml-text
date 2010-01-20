(*
 * pa_text_regexp.mli
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(** Manipulation of regular expressions *)

open Pa_text_types

(** {6 AST of regular expression} *)

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

(** {6 Constructors} *)

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

(** {6 T manipulation} *)

val negate : t -> t option
  (** Try to negate the given regular expression. *)

(** {6 parse-tree --> ast} *)

val of_parse_tree : env : t Pa_text_env.t -> parse_tree : Pa_text_parse.parse_tree -> t
  (** [of_parse_tree ~env ~parse_tree] returns the t
      corresponding to the given parse-tree, with all variables
      inlined (using [env]) and backward reference resolved. *)

(** {6 Marshaling} *)

val to_string : t -> string
  (** [to_string regexp] returns the string representation of [regexp]
      that will be passed to PCRE. *)
