(*
 * text.ml
 * -------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

type t = string
exception Invalid of string * string

let byte str i = Char.code (String.unsafe_get str i)
let set_byte str i n = String.unsafe_set str i (Char.unsafe_chr n)

let unsafe_sub str ofs len =
  let res = String.create len in
  String.unsafe_blit str ofs res 0 len;
  res

let check str =
  let len = String.length str in
  let fail i msg = Some(Printf.sprintf "at offset %d: %s" i msg) in
  let rec trail i minimum acc = function
    | 0 ->
        if acc < minimum then
          fail i "overlong UTF-8 sequence"
        else
          loop i
    | count ->
        if i = len then
          fail i "unterminated UTF-8 sequence"
        else
          let n = byte str i in
          if n land 0xc0 = 0x80 then
            trail (i + 1) minimum ((acc lsl 6) lor (n land 0x3f)) (count - 1)
          else
            fail i "unterminated UTF-8 sequence"
  and loop i =
    if i = len then
      None
    else
      let n = byte str i in
      let i = i + 1 in
      if n land 0x80 = 0 then
        loop i
      else if n land 0xe0 = 0xc0 then
        trail i 0x80 (n land 0x1f) 1
      else if n land 0xf0 = 0xe0 then
        trail i 0x800 (n land 0x0f) 2
      else if n land 0xf8 = 0xf0 then
        trail i 0x10000 (n land 0x07) 3
      else
        fail i "invalid start of UTF-8 sequence"
  in
  loop 0

let invalid str = match check str with
  | None -> raise (Invalid("", str))
  | Some msg -> raise (Invalid(msg, str))

let validate str = match check str with
  | None -> ()
  | Some msg -> raise (Invalid(msg, str))

let encode ?fallback ?(encoding=Encoding.system) txt = Encoding.recode_string ?fallback ~src:"UTF-8" ~dst:encoding txt
let decode ?(encoding=Encoding.system) txt = Encoding.recode_string ~src:encoding ~dst:"UTF-8" txt

let to_ascii txt = encode ~encoding:"ASCII//TRANSLIT" txt

(* +----------+
   | Pointers |
   +----------+ *)

type pointer = {
  txt : t;
  (* The text to which we are pointing *)
  ofs : int;
  (* The position in bytes *)
}

let pointer_l txt = { txt = txt; ofs = 0 }
let pointer_r txt = { txt = txt; ofs = String.length txt }

let offset ptr = ptr.ofs

let rec ofs_next txt ofs len =
  if ofs < len && byte txt ofs land 0xc0 = 0x80 then
    ofs_next txt (ofs + 1) len
  else
    ofs

let next ptr =
  let len = String.length ptr.txt in
  if ptr.ofs = len then
    None
  else
    let ofs = ofs_next ptr.txt (ptr.ofs + 1) len in
    Some(unsafe_sub ptr.txt ptr.ofs (ofs - ptr.ofs), { txt = ptr.txt; ofs = ofs })

let rec ofs_prev txt ofs =
  if ofs > 0 && byte txt ofs land 0xc0 = 0x80 then
    ofs_prev txt (ofs - 1)
  else
    ofs

let prev ptr =
  if ptr.ofs = 0 then
    None
  else
    let ofs = ofs_prev ptr.txt (ptr.ofs - 1) in
    Some(unsafe_sub ptr.txt ofs (ptr.ofs - ofs), { txt = ptr.txt; ofs = ofs })

let move idx ptr =
  let len = String.length ptr.txt in
  if idx >= 0 then begin
    let ofs = ref ptr.ofs and idx = ref idx in
    while !idx > 0 do
      if !ofs = len then
        invalid_arg "Text.move"
      else begin
        ofs := ofs_next ptr.txt (!ofs + 1) len;
        decr idx
      end
    done;
    { txt = ptr.txt; ofs = !ofs }
  end else begin
    let ofs = ref ptr.ofs and idx = ref idx in
    while !idx < 0 do
      if !ofs = 0 then
        invalid_arg "Text.move"
      else begin
        ofs := ofs_prev ptr.txt (!ofs - 1);
        incr idx
      end
    done;
    { txt = ptr.txt; ofs = !ofs }
  end

let chunk a b =
  if a.txt != b.txt then
    invalid_arg "Text.chunk"
  else
    if a.ofs < b.ofs then
      unsafe_sub a.txt a.ofs (b.ofs - a.ofs)
    else
      unsafe_sub a.txt b.ofs (a.ofs - b.ofs)

let pointer_at txt idx =
  if idx < 0 then
    move idx (pointer_r txt)
  else
    move idx (pointer_l txt)

let position ptr =
  let count = ref 0 in
  for i = 0 to ptr.ofs - 1 do
    if byte ptr.txt i land 0xc0 <> 0x80 then incr count
  done;
  !count

let rec ptr_equal_at_aux txt sub ofs = function
  | -1 -> true
  | n -> byte txt (ofs + n) = byte sub n && ptr_equal_at_aux txt sub ofs (n - 1)

let ptr_equal_at txt ofs sub len sub_len =
  if ofs + sub_len > len then
    false
  else
    ptr_equal_at_aux txt sub ofs (sub_len - 1)

let equal_at ptr sub =
  ptr_equal_at ptr.txt ptr.ofs sub (String.length ptr.txt) (String.length sub)

(* +----------------------+
   | High-level functions |
   +----------------------+ *)

let length t =
  let len = ref 0 in
  for i = 0 to String.length t - 1 do
    if byte t i land 0xc0 <> 0x80 then incr len
  done;
  !len

let code txt =
  let len = String.length txt in
  if len = 0 then
    invalid_arg "Text.code"
  else
    let n = byte txt 0 in
    if n land 0x80 = 0 then
      n
    else if n land 0xe0 = 0xc0 then
      if len < 2 then
        invalid txt
      else
        ((n land 0x1f) lsl 6)
        lor (byte txt 1)
    else if n land 0xf0 = 0xe0 then
      if len < 3 then
        invalid txt
      else
        ((n land 0x0f) lsl 12)
        lor ((byte txt 1 land 0x3f) lsl 6)
        lor (byte txt 2 land 0x3f)
    else
      if len < 4 then
        invalid txt
      else
        ((n land 0x07) lsl 18)
        lor ((byte txt 1 land 0x3f) lsl 12)
        lor ((byte txt 2 land 0x3f) lsl 6)
        lor (byte txt 3 land 0x3f)

let char code =
  if code < 0 then
    invalid_arg "Text.char"
  else if code < 0x80 then begin
    let s = String.create 1 in
    set_byte s 0 code;
    s
  end else if code <= 0x800 then begin
    let s = String.create 2 in
    set_byte s 0 ((code lsr 6) lor 0xc0);
    set_byte s 1 ((code land 0x3f) lor 0x80);
    s
  end else if code <= 0x10000 then begin
    let s = String.create 3 in
    set_byte s 0 ((code lsr 12) lor 0xe0);
    set_byte s 1 (((code lsr 6) land 0x3f) lor 0x80);
    set_byte s 2 ((code land 0x3f) lor 0x80);
    s
  end else if code <= 0x10ffff then begin
    let s = String.create 4 in
    set_byte s 0 ((code lsr 18) lor 0xf0);
    set_byte s 1 (((code lsr 12) land 0x3f) lor 0x80);
    set_byte s 2 (((code lsr 6) land 0x3f) lor 0x80);
    set_byte s 3 ((code land 0x3f) lor 0x80);
    s
  end else
    invalid_arg "Text.char"

let get txt idx =
  match next (pointer_at txt idx) with
    | Some(t, _) -> t
    | None -> invalid_arg "Text.get"

let sub txt idx len =
  let a = pointer_at txt idx in
  let b = move len a in
  if a.ofs > b.ofs then
    invalid_arg "Text.sub"
  else
    unsafe_sub txt a.ofs (b.ofs - a.ofs)

let slice txt a b =
  let a = pointer_at txt a and b = pointer_at txt b in
  if a.ofs > b.ofs then
    invalid_arg "Text.slice"
  else
    unsafe_sub txt a.ofs (b.ofs - a.ofs)

let splice txt a b repl =
  let a = pointer_at txt a and b = pointer_at txt b in
  if a.ofs > b.ofs then
    invalid_arg "Text.slice"
  else
    String.concat "" [unsafe_sub txt 0 a.ofs; repl; unsafe_sub txt b.ofs (String.length txt - b.ofs)]

let repeat n txt =
  let len = String.length txt in
  let res = String.create (n * len) in
  let ofs = ref 0 in
  for i = 1 to n do
    String.unsafe_blit txt 0 res !ofs len;
    ofs := !ofs + len
  done;
  res

let iter f txt =
  let rec loop ptr = match next ptr with
    | Some(t, ptr) -> f t; loop ptr
    | None -> ()
  in
  loop (pointer_l txt)

let rev_iter f txt =
  let rec loop ptr = match prev ptr with
    | Some(t, ptr) -> f t; loop ptr
    | None -> ()
  in
  loop (pointer_r txt)

let rev txt =
  let len = String.length txt in
  let ofs_src = ref len and ofs_dst = ref 0 in
  let res = String.create len in
  while !ofs_src > 0 do
    let ofs = ofs_prev txt (!ofs_src - 1) in
    let len = !ofs_src - ofs in
    String.unsafe_blit txt ofs res !ofs_dst len;
    ofs_src := ofs;
    ofs_dst := !ofs_dst + len
  done;
  res

let init n f =
  let buf = Buffer.create n in
  for i = 0 to n - 1 do
    Buffer.add_string buf (f i)
  done;
  Buffer.contents buf

let rev_init n f =
  let buf = Buffer.create n in
  for i = n - 1 downto 0 do
    Buffer.add_string buf (f i)
  done;
  Buffer.contents buf

let concat = String.concat
let rev_concat sep l = String.concat sep (List.rev l)

let explode txt =
  let l = ref [] in
  rev_iter (fun t -> l := t :: !l) txt;
  !l

let rev_explode txt =
  let l = ref [] in
  iter (fun t -> l := t :: !l) txt;
  !l

let implode l = concat "" l
let rev_implode l = rev_concat "" l

let map f txt =
  let buf = Buffer.create (String.length txt) in
  iter (fun ch -> Buffer.add_string buf (f ch)) txt;
  Buffer.contents buf

let rev_map f txt =
  let buf = Buffer.create (String.length txt) in
  rev_iter (fun ch -> Buffer.add_string buf (f ch)) txt;
  Buffer.contents buf

let fold f txt acc =
  let acc = ref acc in
  iter (fun ch -> acc := f ch !acc) txt;
  !acc

let rev_fold f txt acc =
  let acc = ref acc in
  rev_iter (fun ch -> acc := f ch !acc) txt;
  !acc

let filter f txt = map (fun ch -> if f ch then ch else "") txt
let rev_filter f txt = rev_map (fun ch -> if f ch then ch else "") txt

let for_all f txt =
  let rec loop ptr = match next ptr with
    | None -> true
    | Some(t, ptr) -> f t && loop ptr
  in
  loop (pointer_l txt)

let exists f txt =
  let rec loop ptr = match next ptr with
    | None -> false
    | Some(t, ptr) -> f t || loop ptr
  in
  loop (pointer_l txt)

let count f txt =
  let c = ref 0 in
  iter (fun ch -> if f ch then incr c) txt;
  !c

let words txt =
  let len = String.length txt in
  let rec loop ofs =
    if ofs = len then
      []
    else
      match txt.[ofs] with
        | '\r' | '\n' | '\t' | ' ' ->
            loop (ofs + 1)
        | _ ->
            loop_word ofs (ofs_next txt (ofs + 1) len)
  and loop_word ofs_start ofs =
    if ofs = len then
      [String.sub txt ofs_start (ofs - ofs_start)]
    else
      match txt.[ofs] with
        | '\r' | '\n' | '\t' | ' ' ->
            String.sub txt ofs_start (ofs - ofs_start) :: loop (ofs + 1)
        | _ ->
            loop_word ofs_start (ofs_next txt (ofs + 1) len)
  in
  loop 0

let split ?(max=max_int) ?(sep=" ") txt =
  let len = String.length txt and sep_len = String.length sep in
  let rec loop ofs = function
    | 0 -> []
    | 1 -> [String.sub txt ofs (len - ofs)]
    | rem -> loop_word ofs ofs rem
  and loop_word start_ofs ofs rem =
    if ofs = len then
      [String.sub txt start_ofs (ofs - start_ofs)]
    else
      if ptr_equal_at txt ofs sep len sep_len then
        String.sub txt start_ofs (ofs - start_ofs) :: loop (ofs + sep_len) (rem - 1)
      else
        loop_word start_ofs (ofs_next txt (ofs + 1) len) rem
  in
  if sep = "" then
    explode txt
  else
    loop 0 max

let rev_split ?(max=max_int) ?(sep=" ") txt =
  let len = String.length txt and sep_len = String.length sep in
  let rec loop ofs = function
    | 0 -> []
    | 1 -> [String.sub txt 0 ofs]
    | rem -> loop_word ofs ofs rem
  and loop_word end_ofs ofs rem =
    if ofs = 0 then
      [String.sub txt 0 end_ofs]
    else
      if ptr_equal_at txt ofs sep len sep_len then
        String.sub txt ofs (end_ofs - ofs) :: loop (ofs - sep_len) (rem - 1)
      else
        loop_word end_ofs (ofs_prev txt (ofs - 1)) rem
  in
  if sep = "" then
    explode txt
  else
    loop 0 max

let find ?from txt patt =
  let len = String.length txt and patt_len = String.length patt in
  let rec loop ofs =
    if ofs = len then
      None
    else
      if ptr_equal_at txt ofs patt len patt_len then
        Some{ txt = txt; ofs = ofs }
      else
        loop (ofs_next txt (ofs + 1) len)
  in
  loop (match from with
          | Some ptr -> ptr.ofs
          | None -> 0)

let rev_find ?from txt patt =
  let len = String.length txt and patt_len = String.length patt in
  let rec loop ofs =
    if ofs = 0 then
      None
    else
      if ptr_equal_at txt ofs patt len patt_len then
        Some{ txt = txt; ofs = ofs }
      else
        loop (ofs_prev txt (ofs - 1))
  in
  loop (match from with
          | Some ptr -> ptr.ofs
          | None -> len)

let replace text ~patt ~repl =
  let len = String.length text and patt_len = String.length patt in
  let res = Buffer.create len in
  let rec loop ofs_start ofs =
    if ofs = len then begin
      Buffer.add_substring res text ofs_start (ofs - ofs_start);
      Buffer.contents res
    end else
      if ptr_equal_at text ofs patt len patt_len then begin
        Buffer.add_substring res text ofs_start (ofs - ofs_start);
        Buffer.add_string res repl;
        let ofs = ofs + patt_len in
        loop ofs ofs
      end else
        loop ofs_start (ofs_next text (ofs + 1) len)
  in
  match patt, text with
    | "", "" ->
        repl
    | "", _ ->
        concat "" [repl; concat repl (explode text); repl]
    | _ ->
        loop 0 0

let contains txt sub =
  let len = String.length txt and sub_len = String.length sub in
  let rec loop ofs =
    if ofs = len then
      false
    else
      ptr_equal_at txt ofs sub len sub_len || loop (ofs_next txt (ofs + 1) len)
  in
  loop 0

let starts_with txt sub = ptr_equal_at txt 0 sub (String.length txt) (String.length sub)
let ends_with txt sub =
  let len = String.length txt and sub_len = String.length sub in
  let ofs = sub_len - len in
  if ofs < 0 then
    false
  else
    ptr_equal_at txt ofs sub len sub_len

let strip ?(chars=[" "; "\t"; "\r"; "\n"]) txt =
  let rec loop_a a =
    match next a with
      | Some(t, a) when List.mem t chars ->
          loop_a a
      | _ ->
          a
  in
  let a = loop_a (pointer_l txt) in
  let rec loop_b b =
    if b.ofs = a.ofs then
      b
    else
      match prev b with
        | Some(t, b) when List.mem t chars ->
            loop_b b
        | _ ->
            b
  in
  let b = loop_b (pointer_r txt) in
  chunk a b

let rstrip ?(chars=[" "; "\t"; "\r"; "\n"]) txt =
  let rec loop_b b =
    match prev b with
      | Some(t, b) when List.mem t chars ->
          loop_b b
      | _ ->
          chunk (pointer_l txt) b
  in
  loop_b (pointer_r txt)

let lstrip ?(chars=[" "; "\t"; "\r"; "\n"]) txt =
  let rec loop_a a =
    match next a with
      | Some(t, a) when List.mem t chars ->
          loop_a a
      | _ ->
          chunk a (pointer_r txt)
  in
  loop_a (pointer_l txt)

let rchop = function
  | "" -> ""
  | txt ->
      let len = String.length txt in
      let ofs = ofs_prev txt (len - 1) in
      unsafe_sub txt 0 ofs

let lchop = function
  | "" -> ""
  | txt ->
      let len = String.length txt in
      let ofs = ofs_next txt 1 len in
      unsafe_sub txt ofs (len - ofs)

(* +-----------------+
   | Character class |
   +-----------------+ *)

external ml_is_alnum : int -> bool = "ml_text_is_alnum"
external ml_is_alpha : int -> bool = "ml_text_is_alpha"
external ml_is_blank : int -> bool = "ml_text_is_blank"
external ml_is_cntrl : int -> bool = "ml_text_is_cntrl"
external ml_is_digit : int -> bool = "ml_text_is_digit"
external ml_is_graph : int -> bool = "ml_text_is_graph"
external ml_is_lower : int -> bool = "ml_text_is_lower"
external ml_is_print : int -> bool = "ml_text_is_print"
external ml_is_punct : int -> bool = "ml_text_is_punct"
external ml_is_space : int -> bool = "ml_text_is_space"
external ml_is_upper : int -> bool = "ml_text_is_upper"
external ml_is_digit : int -> bool = "ml_text_is_digit"

let for_all_code f txt = for_all (fun ch -> f (code ch)) txt

let is_ascii s =
  let rec loop = function
    | -1 -> true
    | i -> byte s i < 128 && loop (i - 1)
  in
  loop (String.length s - 1)

let is_alnum = for_all_code ml_is_alnum
let is_alpha = for_all_code ml_is_alpha
let is_blank = for_all_code ml_is_blank
let is_cntrl = for_all_code ml_is_cntrl
let is_digit = for_all_code ml_is_digit
let is_graph = for_all_code ml_is_graph
let is_lower = for_all_code ml_is_lower
let is_print = for_all_code ml_is_print
let is_punct = for_all_code ml_is_punct
let is_space = for_all_code ml_is_space
let is_upper = for_all_code ml_is_upper
let is_digit = for_all_code ml_is_digit

(* +--------------------+
   | Upper/lower casing |
   +--------------------+ *)

let map_code f txt = map (fun ch -> char (f (code ch))) txt

external ml_upper : int -> int = "ml_text_upper"
external ml_lower : int -> int = "ml_text_lower"

let upper = map_code ml_upper
let lower = map_code ml_lower

let map_first_code f = function
  | "" -> ""
  | txt ->
      let len = String.length txt in
      let ptr = ofs_next txt 1 len in
      char (f (code (unsafe_sub txt 0 ptr))) ^ unsafe_sub txt ptr (len - ptr)

let capitalize = map_first_code ml_upper
let uncapitalize = map_first_code ml_lower

(* +------------+
   | Comparison |
   +------------+ *)

external ml_compare : string -> string -> int = "ml_text_compare"

let compare t1 t2 = ml_compare (encode t1) (encode t2)
let icompare t1 t2 = ml_compare (encode (lower t1)) (encode (lower t2))
