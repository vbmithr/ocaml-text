(*
 * pa_text_pcre.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

open Camlp4.PreCast
open Syntax

let lookup tbl key =
  try
    Some(Hashtbl.find tbl key)
  with
      Not_found -> None

(* +-----------------------------------------------------------------+
   | Regular expression AST                                          |
   +-----------------------------------------------------------------+ *)

type converter =
  | Constant of Ast.expr
  | Function of Ast.expr

type regexp =
  | Literal of Ast.loc * string
  | Any of Ast.loc
  | Repeat of Ast.loc * regexp * int (* minimum *) * int option (* maximum *)
  | Concat of Ast.loc * regexp * regexp
  | Alternative of Ast.loc * regexp * regexp
  | Bind of Ast.loc * regexp * string * converter option
  | Bind_position of Ast.loc * string

let star loc regexp = Repeat(loc, regexp, 0, None)
let plus loc regexp = Repeat(loc, regexp, 1, None)
let opt loc regexp = Repeat(loc, regexp, 0, Some 1)

let loc_of_regexp = function
  | Literal(l, _) -> l
  | Any l -> l
  | Repeat(l, _, _, _) -> l
  | Concat(l, _, _) -> l
  | Alternative(l, _, _) -> l
  | Bind(l, _, _, _) -> l
  | Bind_position(l, _) -> l

(* +-----------------------------------------------------------------+
   | Regular expression parsing                                      |
   +-----------------------------------------------------------------+ *)

let regexp_eoi = Gram.Entry.mk "regexp_eoi"

EXTEND Gram
  GLOBAL: regexp_eoi;

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
            (a, None) ] ];

  regexp:
    [ [ r = SELF; "as"; i = LIDENT;
        conv =
          OPT [ ":"; s = LIDENT -> Function <:expr< $lid: s ^ "_of_string"$ >>
              | ":="; e = expr -> Function e
              | "="; e = expr -> Constant e ] ->
            Bind(_loc, r, i, conv)
      | r1 = SELF; "|"; r2 = SELF -> Alternative(_loc, r1, r2)
      | r1 = SELF; r2 = SELF -> Concat(_loc, r1, r2) ]

    | "postop" NONA
        [ r = SELF; "*" -> star _loc r
        | r = SELF; "+" -> plus _loc r
        | r = SELF; "?" -> opt _loc r
        | r = SELF; "{"; (a, b) = range; "}" -> Repeat(_loc, r, a, b) ]

    | "simple" NONA
        [ s = STRING -> Literal(_loc, s)
        | "_" -> Any _loc
        | "%"; name = LIDENT -> Bind_position(_loc, name)
        | "("; r = SELF; ")" -> r ] ];

  regexp_eoi:
    [ [ re = regexp; `EOI -> re ] ];
END

(* +-----------------------------------------------------------------+
   | AST --> pcre regular expressions                                |
   +-----------------------------------------------------------------+ *)

let string_of_regexp ast =
  let buffer = Buffer.create 42 in
  let rec loop = function
    | Literal(_, lit) ->
        Buffer.add_string buffer lit
    | Any _ ->
        Buffer.add_char buffer '.'
    | Repeat(_, r, 0, None) ->
        loop r;
        Buffer.add_char buffer '*'
    | Repeat(_, r, 1, None) ->
        loop r;
        Buffer.add_char buffer '+'
    | Repeat(_, r, 0, Some 1) ->
        loop r;
        Buffer.add_char buffer '?'
    | Repeat(_, r, min, None) ->
        loop r;
        Printf.bprintf buffer "{%d+}" min
    | Repeat(_, r, min, Some max) when min = max ->
        loop r;
        Printf.bprintf buffer "{%d}" min
    | Repeat(_, r, min, Some max) ->
        loop r;
        Printf.bprintf buffer "{%d-%d}" min max
    | Concat(_, r1, r2) ->
        loop r1;
        loop r2
    | Alternative(_, r1, r2) ->
        loop r1;
        Buffer.add_char buffer '|';
        loop r2
    | Bind(_, r, i, c) ->
        Buffer.add_char buffer '(';
        loop r;
        Buffer.add_char buffer ')'
    | Bind_position(_, i) ->
        Buffer.add_string buffer "()"
  in
  loop ast;
  Buffer.contents buffer

(* +-----------------------------------------------------------------+
   | Quotation expansion                                             |
   +-----------------------------------------------------------------+ *)

let prefix = "__pa_text_pcre_"

(* Mapping from unique identifier of the form [__pa_text_pcre_NNN] to
   its corresponding regular expression ast *)
let regexps : (string, regexp) Hashtbl.t = Hashtbl.create 42

let gen_id =
  let nb = ref 0 in
  fun () ->
    let x = !nb in
    nb := x + 1;
    prefix ^ string_of_int x

let expand_regexp _loc _loc_name_opt quotation_contents =
  let ast = Gram.parse_string regexp_eoi _loc quotation_contents in
  let id = gen_id () in
  Hashtbl.add regexps id ast;
  (<:patt< $lid:id$ >>)

(* +-----------------------------------------------------------------+
   | Code generation via ast filters                                 |
   +-----------------------------------------------------------------+ *)

type collector = {
  prefix : string;
  mutable next_id : int;
  mutable collect : (Loc.t * string * Ast.expr) list;
}

let collect collector expr =
  let id = Printf.sprintf "__pa_text_pcre_%s_%d" collector.prefix collector.next_id in
  collector.next_id <- collector.next_id + 1;
  let _loc = Ast.loc_of_expr expr in
  collector.collect <- (_loc, id, expr) :: collector.collect;
  <:ident< $lid:id$ >>

(* Verify that [id] is an id generated by [gen_id] *)
let is_special_id id =
  let rec aux1 i =
    if i = String.length prefix then
      aux2 i
    else
      i < String.length id && id.[i] = prefix.[i] && aux1 (i + 1)
  and aux2 i =
    (i < String.length id) && match id.[i] with
      | '0' .. '9' -> aux3 (i + 1)
      | _ -> false
  and aux3 i =
    if i = String.length id then
      true
    else match id.[i] with
      | '0' .. '9' -> aux3 (i + 1)
      | _ -> false
  in
  aux1 0

(* Generate the expression for the given regular expression: *)
let gen_compile_regexp _loc regexp =
  <:expr< lazy(Text_pcre.regexp $str:string_of_regexp regexp$) >>

(* Collects all regular expressions in the pattern of a match case
   branch. [global_regexp_collector] collect all regular expression
   found in the toplevel expression, and [local_regexp_collector]
   collects all regular expression of the current branch. *)
class map_pattern global_regexp_collector local_regexp_collector = object
  inherit Ast.map as super

  method patt p = match super#patt p with
    | <:patt@_loc< $lid:id$ >> as p when is_special_id id -> begin
        match lookup regexps id with
          | Some regexp ->
              (* [regexp_id] is the variable which will appears at the toplevel: *)
              let regexp_id = collect global_regexp_collector (gen_compile_regexp _loc regexp) in
              (* [capture_id] is the variable which will capture the string in the pattern: *)
              let capture_id = collect local_regexp_collector <:expr< $id:regexp_id$ >> in
              <:patt< $id:capture_id$ >>
          | None ->
              p
      end
    | p ->
        p
end

(* Maps all branch of the given match case. It returns [(b, mc)]
   where [b] is [true] iff at least one branch have been modified
   and [mc] is the result. *)
let rec map_match mapper global_regexp_collector = function
  | <:match_case@_loc< $patt$ when $cond$ -> $expr$ >> as mc ->
      let local_regexp_collector = { prefix = "var"; next_id = 0; collect = [] } in
      (* Map the pattern and collect regexp it contains *)
      let patt = (new map_pattern global_regexp_collector local_regexp_collector)#patt patt in
      if local_regexp_collector.collect = [] then
        (* If nothing has changed, keep the branch unchanged *)
        (false, mc)
      else
        let cond = mapper#expr cond and expr = mapper#expr expr in
        (* Generate the array [|(__pa_text_pcre_var_0, __pa_text_pcre_regexp_0); ... |] *)
        let arr = Ast.ExArr(_loc, Ast.exSem_of_list
                              (List.rev_map
                                 (fun (_loc, id, expr) -> <:expr< ($expr$, $lid:id$) >>)
                                 local_regexp_collector.collect)) in
        (* The when condition: *)
        let check_expr = <:expr< Text_pcre.exec __pa_text_pcre_result $arr$ >> in
        (* Merge the original condition with our new one: *)
        let cond = match cond with
          | <:expr< >> -> check_expr
          | _ -> <:expr< $cond$ && $check_expr$ >>
        in
        (true, <:match_case< $patt$ when $cond$ -> $expr$ >>)
  | <:match_case@_loc< $mc1$ | $mc2$ >> ->
      let (b1, mc1) = map_match mapper global_regexp_collector mc1
      and (b2, mc2) = map_match mapper global_regexp_collector mc2 in
      (b1 || b2, <:match_case< $mc1$ | $mc2$ >>)
  | mc ->
      (false, mc)

(* [global_regexp_collector] collects all regular expression found in the expression *)
class map global_regexp_collector = object(self)
  inherit Ast.map as super

  method expr expr = match super#expr expr with
    | <:expr@_loc< match $e$ with $mc$ >> ->
        let modified, mc = map_match self global_regexp_collector mc in
        if modified then
          <:expr< let __pa_text_pcre_result = ref [||] in match $e$ with $mc$ >>
        else
          expr
    | <:expr@_loc< function $mc$ >> ->
        let modified, mc = map_match self global_regexp_collector mc in
        if modified then
          <:expr< let __pa_text_pcre_result = ref [||] in function $mc$ >>
        else
          expr
    | expr ->
        expr
end

(* map expressions:

   [expr]

   becomes:

   {[
     let __pa_text_pcre_regexp_0 = lazy(Pcre.compile "....") in
     let __pa_text_pcre_regexp_1 = lazy(Pcre.compile "....") in
     ...
     let __pa_text_pcre_regexp_N = lazy(Pcre.compile "....") in
     expr
   ]}
*)
let map_expr e =
  let collector = { prefix = "regexp"; next_id = 0; collect = [] } in
  let e = (new map collector)#expr e in
  List.fold_left
    (fun acc (_loc, id, expr) -> <:expr< let $lid:id$ = $expr$ in $acc$ >>)
    e collector.collect

(* Map class expresions:

   {[
     class class_expr
   ]}

   becomes:

   {[
     class
       let __pa_text_pcre_regexp_0 = lazy(Pcre.compile "....") in
       let __pa_text_pcre_regexp_1 = lazy(Pcre.compile "....") in
       ...
       let __pa_text_pcre_regexp_N = lazy(Pcre.compile "....") in
       class_expr
   ]}
*)
let map_class_expr e =
  let collector = { prefix = "regexp"; next_id = 0; collect = [] } in
  let e = (new map collector)#class_expr e in
  List.fold_left
    (fun acc (_loc, id, expr) -> <:class_expr< let $lid:id$ = $expr$ in $acc$ >>)
    e collector.collect

(* map let bindings:

   {[
     let id = expr
   ]}

   becomes:

   {[
     let id =
       let __pa_text_pcre_regexp_0 = lazy(Pcre.compile "....") in
       let __pa_text_pcre_regexp_1 = lazy(Pcre.compile "....") in
       ...
       let __pa_text_pcre_regexp_N = lazy(Pcre.compile "....") in
       expr
   ]}
*)
let rec map_binding = function
  | <:binding@_loc< $id$ = $e$ >> ->
      <:binding< $id$ = $map_expr e$ >>
  | <:binding@_loc< $a$ and $b$ >> ->
      <:binding< $map_binding a$ and $map_binding b$ >>
  | x ->
      x

(* Map top-level definitions *)
let map_def = function
  | Ast.StVal(loc, is_rec, binding) ->
      (* let id = expr *)
      Ast.StVal(loc, is_rec, map_binding binding)
  | Ast.StExp(loc, expr) ->
      (* expr *)
      Ast.StExp(loc, map_expr expr)
  | Ast.StCls(loc, ce) ->
      (* class class_expr *)
      Ast.StCls(loc, map_class_expr ce)
  | x ->
      x

(* +-----------------------------------------------------------------+
   | Registration                                                    |
   +-----------------------------------------------------------------+ *)

let () =
  Quotation.add "re" Quotation.DynAst.patt_tag expand_regexp
  let map = (Ast.map_str_item map_def)#str_item in
  AstFilters.register_str_item_filter map;
  AstFilters.register_topphrase_filter map
