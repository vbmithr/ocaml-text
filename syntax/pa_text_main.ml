(*
 * pa_text_main.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

open Camlp4.PreCast
open Syntax
open Pa_text_types
open Pa_text_regexp

let lookup tbl key =
  try
    Some(Hashtbl.find tbl key)
  with
      Not_found -> None

(* +-----------------------------------------------------------------+
   | Unicode quotations                                              |
   +-----------------------------------------------------------------+ *)

(* The syntax extension added the escape sequence "\u{XXXX}". To
   prevent the lexer to fail and die since this is not a valid ocaml
   escape sequece, we replace the backslash and "u" by a null
   character.

   The restoring of the escape sequenc will be done later, during
   printing of regular expression by [Pa_text_regexp.to_string].
*)

(* replace "\\u{XXXX}" -> "\x00\x00{XXXX}" *)
let hide_unicode_quotations txt =
  let rec loop_in_string acc = function
    | [] ->
        Text.rev_implode acc
    | "\\" :: "\"" :: l ->
        loop_in_string ("\\\"" :: acc) l
    | "\"" :: l ->
        loop_search_string ("\"" :: acc) l
    | "\\" :: "\\" :: l ->
        loop_in_string ( "\\\\" :: acc) l
    | "\\" :: "u" :: l -> begin
        match Pa_text_util.split_hexa_quotation l with
          | Some(txt, l) ->
              loop_in_string (txt :: "\x00\x00" :: acc) l
          | None ->
              loop_in_string ("\\u" :: acc) l
      end
    | x :: l ->
        loop_in_string (x :: acc) l
  and loop_search_string acc = function
    | [] ->
        Text.rev_implode acc
    | "\"" :: l ->
        loop_in_string ("\"" :: acc) l
    | x :: l ->
        loop_search_string (x :: acc) l
  in
  loop_search_string [] (Text.explode txt)

(* +-----------------------------------------------------------------+
   | Initial environment                                             |
   +-----------------------------------------------------------------+ *)

let global_env = ref Pa_text_env.empty
let add_vars l =
  global_env := List.fold_left (fun env (id, exp_regexp) -> Pa_text_env.add id exp_regexp env) !global_env l

let () =
  add_vars [
    ("lower", posix "lower" true);
    ("upper", posix "upper" true);
    ("alpha", posix "alpha" true);
    ("digit", posix "digit" true);
    ("alnum", posix "alnum" true);
    ("punct", posix "punct" true);
    ("graph", posix "graph" true);
    ("print", posix "print" true);
    ("blank", posix "blank" true);
    ("cntrl", posix "cntrl" true);
    ("xdigit", posix "xdigit" true);
    ("space", posix "space" true);
    ("ascii", posix "ascii" true);
    ("word", posix "word" true);
    ("newline", meta "\\R" None);
    ("hspace", meta "\\h" (Some "\\H"));
    ("vspace", meta "\\v" (Some "\\V"));
    ("bound", meta "\\b" (Some "\\B"));
    ("bos", meta "\\A" None);
    ("eos", meta "\\z" None);
  ];

  (* Unicode properties *)
  add_vars
    (List.map (fun name -> (name, meta ("\\p{" ^ name ^ "}") (Some ("\\P{" ^ name ^ "}")))) [
       "C"; "Cc"; "Cf"; "Cn"; "Co"; "Cs";
       "L"; "Ll"; "Lm"; "Lo"; "Lt"; "Lu";
       "M"; "Mc"; "Me"; "Mn";
       "N"; "Nd"; "Nl"; "No";
       "P"; "Pc"; "Pd"; "Pe"; "Pf"; "Pi"; "Po"; "Ps";
       "S"; "Sc"; "Sk"; "Sm"; "So";
       "Z"; "Zl"; "Zp"; "Zs";
     ]);

  (* Scripts *)
  add_vars
    (List.map (fun name -> (name, meta ("\\p{" ^ name ^ "}") (Some ("\\P{" ^ name ^ "}")))) [
       "Arabic";
       "Armenian";
       "Balinese";
       "Bengali";
       "Bopomofo";
       "Braille";
       "Buginese";
       "Buhid";
       "Canadian_boriginal";
       "Cherokee";
       "Common";
       "Coptic";
       "Cuneiform";
       "Cypriot";
       "Cyrillic";
       "Deseret";
       "Devanagari";
       "Ethiopic";
       "Georgian";
       "Glagolitic";
       "Gothic";
       "Greek";
       "Gujarati";
       "Gurmukhi";
       "Han";
       "Hangul";
       "Hanunoo";
       "Hebrew";
       "Hiragana";
       "Inherited";
       "Kannada";
       "Katakana";
       "Kharoshthi";
       "Khmer";
       "Lao";
       "Latin";
       "Limbu";
       "Linear_B";
       "Malayalam";
       "Mongolian";
       "Myanmar";
       "New_Tai_Lue";
       "Nko";
       "Ogham";
       "Old_Italic";
       "Old_Persian";
       "Oriya";
       "Osmanya";
       "Phags_Pa";
       "Phoenician";
       "Runic";
       "Shavian";
       "Sinhala";
       "Syloti_Nagri";
       "Syriac";
       "Tagalog";
       "Tagbanwa";
       "Tai_Le";
       "Tamil";
       "Telugu";
       "Thaana";
       "Thai";
       "Tibetan";
       "Tifinagh";
       "Ugaritic";
       "Yi";
     ])

(* +-----------------------------------------------------------------+
   | Quotation expansion                                             |
   +-----------------------------------------------------------------+ *)

let prefix = "__pa_text_pcre_"

(* Mapping from unique identifier of the form [__pa_text_pcre_NNN] to
   its corresponding regular expression ast *)
let regexps : (string, Pa_text_parse.parse_tree * [ `text | `regexp ]) Hashtbl.t = Hashtbl.create 42

let gen_id =
  let nb = ref 0 in
  fun () ->
    let x = !nb in
    nb := x + 1;
    prefix ^ string_of_int x

let expand_patt_regexp _loc _loc_name_opt quotation_contents =
  let ast = Pa_text_parse.parse _loc (hide_unicode_quotations quotation_contents) in
  let id = gen_id () in
  Hashtbl.add regexps id (ast, `regexp);
  <:patt< $lid:id$ >>

let expand_expr_regexp _loc _loc_name_opt quotation_contents =
  let ast = Pa_text_parse.parse _loc (hide_unicode_quotations quotation_contents) in
  let id = gen_id () in
  Hashtbl.add regexps id (ast, `regexp);
  <:expr< $lid:id$ >>

let expand_expr_text _loc _loc_name_opt quotation_contents =
  let ast = Pa_text_parse.parse _loc (hide_unicode_quotations quotation_contents) in
  let id = gen_id () in
  Hashtbl.add regexps id (ast, `text);
  <:expr< $lid:id$ >>

(* +-----------------------------------------------------------------+
   | Code generation via ast filters                                 |
   +-----------------------------------------------------------------+ *)

type 'a collector = {
  prefix : string;
  mutable next_id : int;
  mutable collect : (Loc.t * string * 'a) list;
}

let collect collector _loc data =
  let id = Printf.sprintf "%s%s_%d" prefix collector.prefix collector.next_id in
  collector.next_id <- collector.next_id + 1;
  collector.collect <- (_loc, id, data) :: collector.collect;
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
let gen_compile_regexp _loc env regexp =
  <:expr< lazy(Text_pcre.regexp $str:Pa_text_regexp.to_string (Pa_text_regexp.of_parse_tree env regexp)$) >>

(* Collects all regular expressions in the pattern of a match case
   branch. [global_regexp_collector] collect all regular expression
   found in the toplevel expression, and [local_regexp_collector]
   collects all regular expression of the current branch. *)
class map_pattern env global_regexp_collector local_regexp_collector = object
  inherit Ast.map as super

  method patt p = match super#patt p with
    | <:patt@_loc< $lid:id$ >> as p when is_special_id id -> begin
        match lookup regexps id with
          | Some(regexp, `regexp) ->
              (* [regexp_id] is the variable which will appears at the toplevel: *)
              let regexp_id = collect global_regexp_collector _loc (gen_compile_regexp _loc env regexp) in
              (* [capture_id] is the variable which will capture the string in the pattern: *)
              let capture_id = collect local_regexp_collector _loc (<:expr< $id:regexp_id$ >>, regexp) in
              <:patt< $id:capture_id$ >>
          | _ ->
              p
      end
    | p ->
        p
end

module StringSet = Set.Make(String)

class collect_pattern_lids set = object
  inherit Ast.map as super

  method patt patt = match super#patt patt with
    | <:patt< $lid:id$ >> as patt ->
        set := StringSet.add id !set;
        patt
    | p ->
        patt
end

(* Check that all variables contained in variables are distincts *)
let check_collision patt variables =
  let add set (_loc, id, n, conv) =
    if StringSet.mem id set then
      Loc.raise _loc (Failure (Printf.sprintf "Variable %s is bound several times in this matching" id))
    else
      StringSet.add id set
  in
  let set = ref StringSet.empty in
  let _ = (new collect_pattern_lids set)#patt patt in
  let _ = List.fold_left (fun set vars -> List.fold_left add set vars) !set variables in
  ()

(* Maps all branch of the given match case. It returns [(b, mc)]
   where [b] is [true] iff at least one branch have been modified
   and [mc] is the result. *)
let rec map_match mapper env global_regexp_collector = function
  | <:match_case@_loc< $patt$ when $cond$ -> $expr$ >> as mc ->
      let local_regexp_collector = { prefix = "var"; next_id = 0; collect = [] } in
      (* Map the pattern and collect regexp it contains *)
      let patt = (new map_pattern env global_regexp_collector local_regexp_collector)#patt patt in
      if local_regexp_collector.collect = [] then
        (* If nothing has changed, keep the branch unchanged *)
        (false, mc)
      else
        let cond = mapper#expr cond and expr = mapper#expr expr in
        (* Generate the array [|(__pa_text_pcre_var_0, __pa_text_pcre_regexp_0); ... |] *)
        let arr = Ast.ExArr(_loc, Ast.exSem_of_list
                              (List.rev_map
                                 (fun (_loc, id, (expr, regexp)) -> <:expr< ($expr$, $lid:id$) >>)
                                 local_regexp_collector.collect)) in
        (* The when condition: *)
        let check_expr = <:expr< Text_pcre.exec __pa_text_pcre_result $arr$ >> in
        (* Merge the original condition with our new one: *)
        let cond = match cond with
          | <:expr< >> -> check_expr
          | _ -> <:expr< $cond$ && $check_expr$ >>
        in
        (* Collect all capture variables in regexps: *)
        let variables_by_regexp =
          List.map
            (fun (_loc, id, (expr, regexp)) -> (Pa_text_parse.collect_regexp_bindings regexp))
            local_regexp_collector.collect
        in
        (* Check for conflicts *)
        check_collision patt variables_by_regexp;
        (* Bind pattern variables *)
        let rec make_bindings regexp_number acc = function
          | [] -> acc
          | variables :: rest ->
              let acc = List.fold_left begin fun acc (_loc, id, n, conv) ->
                let binding = match conv with
                  | Pa_text_parse.Identity ->
                      <:binding< $lid:id$ = Text_pcre.get_substring
                                              (Array.unsafe_get !__pa_text_pcre_result $int:string_of_int regexp_number$)
                                              $int:string_of_int n$ >>
                  | Pa_text_parse.Constant e ->
                      <:binding< $lid:id$ = $e$ >>
                  | Pa_text_parse.Function f ->
                      <:binding< $lid:id$ = $f$ (Text_pcre.get_substring
                                                   (Array.unsafe_get !__pa_text_pcre_result $int:string_of_int regexp_number$)
                                                   $int:string_of_int n$) >>
                  | Pa_text_parse.Position ->
                      <:binding< $lid:id$ = Text_pcre.get_substring_ofs
                                              (Array.unsafe_get !__pa_text_pcre_result $int:string_of_int regexp_number$)
                                              $int:string_of_int n$ >>
                in
                binding :: acc
              end acc variables in
              make_bindings (regexp_number + 1) acc rest
        in
        (* Make the compiler happy (no "unused variables") *)
        let expr =
          List.fold_left
            (fun expr vars ->
               List.fold_left
                 (fun expr (_loc, id, _, _) ->
                    <:expr< ignore $lid:id$; $expr$ >>)
                 expr vars)
            expr variables_by_regexp
        in
        (true, <:match_case< $patt$ when $cond$ -> let $Ast.biAnd_of_list (make_bindings 0 [] variables_by_regexp)$ in $expr$ >>)
  | <:match_case@_loc< $mc1$ | $mc2$ >> ->
      let (b1, mc1) = map_match mapper env global_regexp_collector mc1
      and (b2, mc2) = map_match mapper env global_regexp_collector mc2 in
      (b1 || b2, <:match_case< $mc1$ | $mc2$ >>)
  | mc ->
      (false, mc)

(* [global_regexp_collector] collects all regular expression found in the expression *)
class map env global_regexp_collector = object(self)
  inherit Ast.map as super

  method expr expr =
    let expr = super#expr expr in
    match expr with
      | <:expr@_loc< match $e$ with $mc$ >> ->
          let modified, mc = map_match self env global_regexp_collector mc in
          if modified then
            <:expr< let __pa_text_pcre_result = ref [||] in match $e$ with $mc$ >>
          else
            expr
      | <:expr@_loc< function $mc$ >> ->
          let modified, mc = map_match self env global_regexp_collector mc in
          if modified then
            <:expr< let __pa_text_pcre_result = ref [||] in function $mc$ >>
          else
            expr
      | <:expr@_loc< $lid:id$ >> when is_special_id id -> begin
          match lookup regexps id with
            | Some(regexp, `regexp) ->
                let regexp_id = collect global_regexp_collector _loc (gen_compile_regexp _loc env regexp) in
                <:expr< Lazy.force $id:regexp_id$ >>
            | Some(regexp, `text) ->
                <:expr< $str:Pa_text_regexp.to_string (Pa_text_regexp.of_parse_tree env regexp)$ >>
            | None ->
                expr
        end
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
  let e = (new map !global_env collector)#expr e in
  match collector.collect with
    | [] ->
        e
    | (_loc, id, expr) :: collect ->
        let binding =
          List.fold_left
            (fun acc (_loc, id, expr) -> <:binding< $acc$ and $lid:id$ = $expr$ >>)
            <:binding< $lid:id$ = $expr$ >> collect
        in
        <:expr< let $binding$ in $e$ >>

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
  let e = (new map !global_env collector)#class_expr e in
  match collector.collect with
    | [] ->
        e
    | (_loc, id, expr) :: collect ->
        let binding =
          List.fold_left
            (fun acc (_loc, id, expr) -> <:binding< $acc$ and $lid:id$ = $expr$ >>)
            <:binding< $lid:id$ = $expr$ >> collect
        in
        <:class_expr< let $binding$ in $e$ >>

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
let rec map_binding new_env = function
  | <:binding@_loc< $lid:id$ = $lid:id_re$ >> as binding when is_special_id id_re -> begin
      match lookup regexps id_re with
        | Some(parse_tree, `regexp) ->
            let regexp = Pa_text_regexp.of_parse_tree !global_env parse_tree in
            (Pa_text_env.add id regexp new_env,
             <:binding< $lid:id$ = Text_pcre.regexp $str:Pa_text_regexp.to_string regexp$ >>)
        | Some(parse_tree, `text) ->
            let regexp = Pa_text_regexp.of_parse_tree !global_env parse_tree in
            (new_env,
             <:binding< $lid:id$ = $str:Pa_text_regexp.to_string regexp$ >>)
        | None ->
            (new_env, binding)
    end
  | <:binding@_loc< $id$ = $e$ >> ->
      (new_env, <:binding< $id$ = $map_expr e$ >>)
  | <:binding@_loc< $a$ and $b$ >> ->
      let new_env, binding_a = map_binding new_env a in
      let new_env, binding_b = map_binding new_env b in
      (new_env, <:binding< $binding_a$ and $binding_b$ >>)
  | binding ->
      (new_env, binding)

(* Map top-level definitions *)
let map_def = function
  | Ast.StVal(loc, is_rec, binding) ->
      (* let id = expr *)
      let new_env, binding = map_binding !global_env binding in
      global_env := new_env;
      Ast.StVal(loc, is_rec, binding)
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
  Quotation.add "re" Quotation.DynAst.patt_tag expand_patt_regexp;
  Quotation.add "re" Quotation.DynAst.expr_tag expand_expr_regexp;
  Quotation.add "re_text" Quotation.DynAst.expr_tag expand_expr_text;
  let map = (Ast.map_str_item map_def)#str_item in
  AstFilters.register_str_item_filter map;
  AstFilters.register_topphrase_filter map
