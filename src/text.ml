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

let fail str pos msg = raise (Invalid(Printf.sprintf "at position %d: %s" pos msg, str))

(* +-----------------------------------------------------------------+
   | Unsafe primitives (use with caution!)                           |
   +-----------------------------------------------------------------+ *)

let byte str i = Char.code (String.unsafe_get str i)
let set_byte str i n = String.unsafe_set str i (Char.unsafe_chr n)

let unsafe_sub str ofs len =
  let res = String.create len in
  String.unsafe_blit str ofs res 0 len;
  res

(* +-----------------------------------------------------------------+
   | UTF-8 validation                                                |
   +-----------------------------------------------------------------+ *)

let check s =
  let fail i msg = Some(Printf.sprintf "at position %d: %s" i msg) in
  let len = String.length s in
  let rec main i =
    if i = len then
      None
    else
      let ch = String.unsafe_get s i in
      match ch with
        | '\x00' .. '\x7f' ->
            main (i + 1)
        | '\xc0' .. '\xdf' ->
            if i + 1 >= len then
              fail len "premature end of UTF8 sequence"
            else begin
              let byte1 = Char.code (String.unsafe_get s (i + 1)) in
              if byte1 land 0xc0 != 0x80 then
                fail (i + 1) "malformed UTF8 sequence"
              else if ((Char.code ch land 0x1f) lsl 6) lor (byte1 land 0x3f) < 0x80 then
                fail i "overlong UTF8 sequence"
              else
                main (i + 2)
            end
        | '\xe0' .. '\xef' ->
            if i + 2 >= len then
              fail len "premature end of UTF8 sequence"
            else begin
              let byte1 = Char.code (String.unsafe_get s (i + 1))
              and byte2 = Char.code (String.unsafe_get s (i + 2)) in
              if byte1 land 0xc0 != 0x80 then
                fail (i + 1) "malformed UTF8 sequence"
              else if byte2 land 0xc0 != 0x80 then
                fail (i + 2) "malformed UTF8 sequence"
              else if ((Char.code ch land 0x0f) lsl 12) lor ((byte1 land 0x3f) lsl 6) lor (byte2 land 0x3f) < 0x800 then
                fail i "overlong UTF8 sequence"
              else
                main (i + 3)
            end
        | '\xf0' .. '\xf7' ->
            if i + 3 >= len then
              fail len "premature end of UTF8 sequence"
            else begin
              let byte1 = Char.code (String.unsafe_get s (i + 1))
              and byte2 = Char.code (String.unsafe_get s (i + 2))
              and byte3 = Char.code (String.unsafe_get s (i + 3)) in
              if byte1 land 0xc0 != 0x80 then
                fail (i + 1) "malformed UTF8 sequence"
              else if byte2 land 0xc0 != 0x80 then
                fail (i + 2) "malformed UTF8 sequence"
              else if byte3 land 0xc0 != 0x80 then
                fail (i + 3) "malformed UTF8 sequence"
              else if ((Char.code ch land 0x07) lsl 18) lor ((byte1 land 0x3f) lsl 12) lor ((byte2 land 0x3f) lsl 6) lor (byte3 land 0x3f) < 0x10000 then
                fail i "overlong UTF8 sequence"
              else
                main (i + 4)
            end
        | _ ->
            fail i "invalid start of UTF8 sequence"
  in
  main 0

let invalid str = match check str with
  | None -> raise (Invalid("", str))
  | Some msg -> raise (Invalid(msg, str))

let validate str = match check str with
  | None -> ()
  | Some msg -> raise (Invalid(msg, str))

(* +-----------------------------------------------------------------+
   | Encoding/decoding                                               |
   +-----------------------------------------------------------------+ *)

let sys_encoding = Encoding.system ^ "//TRANSLIT"

let encode ?(encoding=sys_encoding) txt = Encoding.recode_string ~src:"UTF-8" ~dst:encoding txt
let decode ?(encoding=sys_encoding) txt = Encoding.recode_string ~src:encoding ~dst:"UTF-8" txt

let to_ascii txt = encode ~encoding:"ASCII//TRANSLIT" txt

(* +-----------------------------------------------------------------+
   | Unsafe UTF-8 naviguation                                        |
   +-----------------------------------------------------------------+ *)

let unsafe_next str ofs =
  match String.unsafe_get str ofs with
    | '\x00' .. '\x7f' ->
        ofs + 1
    | '\xc0' .. '\xdf' ->
        if ofs + 2 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          ofs + 2
    | '\xe0' .. '\xef' ->
        if ofs + 3 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          ofs + 3
    | '\xf0' .. '\xf7' ->
        if ofs + 4 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          ofs + 4
    | _ ->
        fail str ofs "invalid start of UTF-8 sequence"

let unsafe_prev str ofs =
  match String.unsafe_get str (ofs - 1) with
    | '\x00' .. '\x7f' ->
        ofs - 1
    | '\x80' .. '\xbf' ->
        if ofs >= 2 then
          match String.unsafe_get str (ofs - 2) with
            | '\xc0' .. '\xdf' ->
                ofs - 2
            | '\x80' .. '\xbf' ->
                if ofs >= 3 then
                  match String.unsafe_get str (ofs - 3) with
                    | '\xe0' .. '\xef' ->
                        ofs - 3
                    | '\x80' .. '\xbf' ->
                        if ofs >= 4 then
                          match String.unsafe_get str (ofs - 4) with
                            | '\xf0' .. '\xf7' ->
                                ofs - 4
                            | _ ->
                                fail str (ofs - 4) "invalid start of UTF-8 sequence"
                        else
                          fail str (ofs - 3) "invalid start of UTF-8 string"
                    | _ ->
                        fail str (ofs - 3) "invalid middle of UTF-8 sequence"
                else
                  fail str (ofs - 2) "invaild start of UTF-8 string"
            | _ ->
                fail str (ofs - 2) "invalid middle of UTF-8 sequence"
        else
          fail str (ofs - 1) "invalid start of UTF-8 string"
    | _ ->
        fail str (ofs - 1) "invalid end of UTF-8 sequence"

let unsafe_extract_next str ofs =
  let ch = String.unsafe_get str ofs in
  match ch with
    | '\x00' .. '\x7f' ->
        (Char.code ch, ofs + 1)
    | '\xc0' .. '\xdf' ->
        if ofs + 2 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          (((Char.code ch land 0x1f) lsl 6) lor (byte str (ofs + 1) land 0x3f), ofs + 2)
    | '\xe0' .. '\xef' ->
        if ofs + 3 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          (((Char.code ch land 0x0f) lsl 12) lor ((byte str (ofs + 1) land 0x3f) lsl 6) lor (byte str (ofs + 2) land 0x3f), ofs + 3)
    | '\xf0' .. '\xf7' ->
        if ofs + 4 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          (((Char.code ch land 0x07) lsl 18) lor ((byte str (ofs + 1) land 0x3f) lsl 12) lor ((byte str (ofs + 2) land 0x3f) lsl 6) lor (byte str (ofs + 3) land 0x3f), ofs + 4)
    | _ ->
        fail str ofs "invalid start of UTF-8 sequence"

(* +-----------------------------------------------------------------+
   | Pointers                                                        |
   +-----------------------------------------------------------------+ *)

type pointer = {
  txt : t;
  (* The text to which we are pointing *)
  ofs : int;
  (* The position in bytes *)
}

let pointer_l txt = { txt = txt; ofs = 0 }
let pointer_r txt = { txt = txt; ofs = String.length txt }

let offset ptr = ptr.ofs

let next ptr =
  let len = String.length ptr.txt in
  if ptr.ofs = len then
    None
  else
    let ofs = unsafe_next ptr.txt ptr.ofs in
    Some(unsafe_sub ptr.txt ptr.ofs (ofs - ptr.ofs), { ptr with ofs = ofs })

let prev ptr =
  if ptr.ofs = 0 then
    None
  else
    let ofs = unsafe_prev ptr.txt ptr.ofs in
    Some(unsafe_sub ptr.txt ofs (ptr.ofs - ofs), { ptr with ofs = ofs })

let rec move_offset_l str ofs len =
  if len = 0 then
    ofs
  else if ofs = String.length str then
    invalid_arg "Text.move"
  else
    move_offset_l str (unsafe_next str ofs) (len - 1)

let rec move_offset_r str ofs len =
  if len = 0 then
    ofs
  else if ofs = 0 then
    invalid_arg "Text.move"
  else
    move_offset_r str (unsafe_prev str ofs) (len + 1)

let move_offset str ofs len =
  if len = 0 then
    ofs
  else if len > 0 then
    move_offset_l str ofs len
  else
    move_offset_r str ofs len

let move len ptr =
  { ptr with ofs = move_offset ptr.txt ptr.ofs len }

let chunk a b =
  if a.txt != b.txt then
    invalid_arg "Text.chunk"
  else
    if a.ofs < b.ofs then
      unsafe_sub a.txt a.ofs (b.ofs - a.ofs)
    else
      unsafe_sub a.txt b.ofs (a.ofs - b.ofs)

let offset_at txt idx =
  if idx < 0 then
    move_offset_r txt (String.length txt) idx
  else
    move_offset_l txt 0 idx

let pointer_at txt idx =
  { txt = txt; ofs = offset_at txt idx }

let rec position_rec ptr ofs pos =
  if ofs >= ptr.ofs then
    pos
  else
    position_rec ptr (unsafe_next ptr.txt ofs) (pos + 1)

let position ptr = position_rec ptr 0 0

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

(* +-----------------------------------------------------------------+
   | High-level functions                                            |
   +-----------------------------------------------------------------+ *)

let rec length_rec str ofs len =
  if ofs = String.length str then
    len
  else
    length_rec str (unsafe_next str ofs) (len + 1)

let length str = length_rec str 0 0

let code str =
  if str = "" then
    invalid_arg "Text.code"
  else
    let ch = String.unsafe_get str 0 in
    match ch with
      | '\x00' .. '\x7f' ->
          Char.code ch
      | '\xc0' .. '\xdf' ->
          if 2 > String.length str then
            fail str 0 "unterminated UTF-8 sequence"
          else
            ((Char.code ch land 0x1f) lsl 6) lor (byte str 1 land 0x3f)
      | '\xe0' .. '\xef' ->
          if 3 > String.length str then
            fail str 0 "unterminated UTF-8 sequence"
          else
            ((Char.code ch land 0x0f) lsl 12) lor ((byte str 1 land 0x3f) lsl 6) lor (byte str 2 land 0x3f)
      | '\xf0' .. '\xf7' ->
          if 4 > String.length str then
            fail str 0 "unterminated UTF-8 sequence"
          else
            ((Char.code ch land 0x07) lsl 18) lor ((byte str 1 land 0x3f) lsl 12) lor ((byte str 2 land 0x3f) lsl 6) lor (byte str 3 land 0x3f)
      | _ ->
          fail str 0 "invalid start of UTF-8 sequence"

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
  let ofs = offset_at txt idx in
  if ofs = String.length txt then
    invalid_arg "Text.get"
  else
    let ofs' = unsafe_next txt ofs in
    unsafe_sub txt ofs (ofs' - ofs)

let sub txt idx len =
  let a = offset_at txt idx in
  let b = move_offset txt a len in
  if a > b then
    unsafe_sub txt b (a - b)
  else
    unsafe_sub txt a (b - a)

let slice txt a b =
  let a = offset_at txt a and b = offset_at txt b in
  if a > b then
    invalid_arg "Text.slice"
  else
    unsafe_sub txt a (b - a)

let splice txt a b repl =
  let a = offset_at txt a and b = offset_at txt b in
  if a > b then
    invalid_arg "Text.slice"
  else begin
    let res = String.create (a + String.length repl + String.length txt - b) in
    String.unsafe_blit txt 0 res 0 a;
    String.unsafe_blit repl 0 res a (String.length repl);
    String.unsafe_blit txt b res (a + String.length repl) (String.length txt - b);
    res
  end

let repeat n txt =
  let len = String.length txt in
  let res = String.create (n * len) in
  let ofs = ref 0 in
  for i = 1 to n do
    String.unsafe_blit txt 0 res !ofs len;
    ofs := !ofs + len
  done;
  res

let rec iter_rec f txt ofs =
  if ofs <> String.length txt then begin
    let ofs' = unsafe_next txt ofs in
    f (unsafe_sub txt ofs (ofs' - ofs));
    iter_rec f txt ofs'
  end

let iter f txt = iter_rec f txt 0

let rec rev_iter_rec f txt ofs =
  if ofs <> 0 then begin
    let ofs' = unsafe_prev txt ofs in
    f (unsafe_sub txt ofs' (ofs - ofs'));
    rev_iter_rec f txt ofs'
  end

let rev_iter f txt = rev_iter_rec f txt (String.length txt)

let rev txt =
  let len = String.length txt in
  let ofs_src = ref len and ofs_dst = ref 0 in
  let res = String.create len in
  while !ofs_src > 0 do
    let ofs = unsafe_prev txt !ofs_src in
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


let concat sep l =
  match l with
    | [] ->
        ""
    | x :: l ->
        let sep_len = String.length sep in
        let len = List.fold_left (fun len str -> len + sep_len + String.length str) (String.length x) l in
        let res = String.create len in
        String.unsafe_blit x 0 res 0 (String.length x);
        ignore
          (List.fold_left
             (fun ofs str ->
                String.unsafe_blit sep 0 res ofs sep_len;
                let ofs = ofs + sep_len in
                let len = String.length str in
                String.unsafe_blit str 0 res ofs len;
                ofs + len)
             (String.length x) l);
        res

let rev_concat sep l =
  match l with
    | [] ->
        ""
    | x :: l ->
        let sep_len = String.length sep in
        let len = List.fold_left (fun len str -> len + sep_len + String.length str) (String.length x) l in
        let res = String.create len in
        let ofs = len - String.length x in
        String.unsafe_blit x 0 res ofs (String.length x);
        ignore
          (List.fold_left
             (fun ofs str ->
                let ofs = ofs - sep_len in
                String.unsafe_blit sep 0 res ofs sep_len;
                let len = String.length str in
                let ofs = ofs - len in
                String.unsafe_blit str 0 res ofs len;
                ofs)
             ofs l);
        res

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

let rec for_all_rec f txt ofs =
  if ofs = String.length txt then
    true
  else begin
    let ofs' = unsafe_next txt ofs in
    f (unsafe_sub txt ofs (ofs' - ofs)) && for_all_rec f txt ofs'
  end

let for_all f txt = for_all_rec f txt 0

let rec exists_rec f txt ofs =
  if ofs = String.length txt then
    false
  else begin
    let ofs' = unsafe_next txt ofs in
    f (unsafe_sub txt ofs (ofs' - ofs)) || exists_rec f txt ofs'
  end

let exists f txt = exists_rec f txt 0

let count f txt =
  let c = ref 0 in
  iter (fun ch -> if f ch then incr c) txt;
  !c

(* +-----------------------------------------------------------------+
   | Character class                                                 |
   +-----------------------------------------------------------------+ *)

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
external ml_is_xdigit : int -> bool = "ml_text_is_xdigit"

let rec for_all_code_rec f txt ofs =
  if ofs = String.length txt then
    true
  else begin
    let code, ofs' = unsafe_extract_next txt ofs in
    f code && for_all_code_rec f txt ofs'
  end

let for_all_code f txt = for_all_code_rec f txt 0

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
let is_xdigit = for_all_code ml_is_xdigit

(* +-----------------------------------------------------------------+
   | Searching, Splitting, ...                                       |
   +-----------------------------------------------------------------+ *)

let words txt =
  let rec loop ptr =
    match next ptr with
      | Some(ch, ptr') ->
          if is_punct ch || is_space ch then
            loop ptr'
          else
            loop_word ptr ptr'
      | None ->
          []
  and loop_word ptr_start ptr =
    match next ptr with
      | Some(ch, ptr') ->
          if is_punct ch || is_space ch then
            chunk ptr_start ptr :: loop ptr'
          else
            loop_word ptr_start ptr'
      | None ->
          [chunk ptr_start ptr]
  in
  loop (pointer_l txt)

let lines txt =
  let rec loop start_ptr ptr =
    match next ptr with
      | Some("\n", ptr') ->
          chunk start_ptr ptr :: loop ptr' ptr'
      | Some("\r", ptr') ->
          begin match next ptr' with
            | Some("\n", ptr') ->
                chunk start_ptr ptr :: loop ptr' ptr'
            | Some(ch, ptr') ->
                loop start_ptr ptr'
            | None ->
                match chunk start_ptr ptr with
                  | "" -> []
                  | t -> [t]
          end
      | Some(ch, ptr) ->
          loop start_ptr ptr
      | None ->
          match chunk start_ptr ptr with
            | "" -> []
            | t -> [t]
  in
  let ptr = pointer_l txt in
  loop ptr ptr

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
        loop_word start_ofs (unsafe_next txt ofs) rem
  in
  if sep = "" then
    explode txt
  else
    loop 0 max

let rev_split ?(max=max_int) ?(sep=" ") txt =
  let len = String.length txt and sep_len = String.length sep in
  let rec loop acc ofs = function
    | 0 -> acc
    | 1 -> String.sub txt 0 ofs :: acc
    | rem -> loop_word acc ofs ofs rem
  and loop_word acc ofs end_ofs rem =
    if ofs = 0 then
      String.sub txt 0 end_ofs :: acc
    else
      let ofs = unsafe_prev txt ofs in
      if ptr_equal_at txt ofs sep len sep_len then
        loop (String.sub txt (ofs + sep_len) (end_ofs - ofs - sep_len) :: acc) ofs (rem - 1)
      else
        loop_word acc ofs end_ofs rem
  in
  if sep = "" then
    explode txt
  else
    loop [] len max

let find ?from txt patt =
  let len = String.length txt and patt_len = String.length patt in
  let rec loop ofs =
    if ofs + patt_len > len then
      None
    else
      if ptr_equal_at txt ofs patt len patt_len then
        Some{ txt = txt; ofs = ofs }
      else
        loop (unsafe_next txt ofs)
  in
  loop (match from with
          | Some ptr -> ptr.ofs
          | None -> 0)

let rev_find ?from txt patt =
  let len = String.length txt and patt_len = String.length patt in
  let rec loop ofs =
    if ofs < 0 then
      None
    else
      if ptr_equal_at txt ofs patt len patt_len then
        Some{ txt = txt; ofs = ofs }
      else
        loop (unsafe_prev txt ofs)
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
        loop ofs_start (unsafe_next text ofs)
  in
  match patt, text with
    | "", "" ->
        repl
    | "", _ ->
        concat "" [repl; concat repl (explode text); repl]
    | _ ->
        loop 0 0

let contains txt = function
  | "" ->
      true
  | sub ->
      let len = String.length txt and sub_len = String.length sub in
      let rec loop ofs =
        if ofs = len then
          false
        else
          ptr_equal_at txt ofs sub len sub_len || loop (unsafe_next txt ofs)
      in
      loop 0

let starts_with txt sub =
  equal_at (pointer_l txt) sub

let ends_with txt sub =
  let idx = length txt - length sub in
  if idx < 0 then
    false
  else
    equal_at (pointer_at txt idx) sub

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
      let ofs = unsafe_prev txt len in
      unsafe_sub txt 0 ofs

let lchop = function
  | "" -> ""
  | txt ->
      let len = String.length txt in
      let ofs = unsafe_next txt 0 in
      unsafe_sub txt ofs (len - ofs)

(* +-----------------------------------------------------------------+
   | Upper/lower casing                                              |
   +-----------------------------------------------------------------+ *)

let rec map_code_rec f buf txt ofs =
  if ofs = String.length txt then
    Buffer.contents buf
  else begin
    let code, ofs = unsafe_extract_next txt ofs in
    Buffer.add_string buf (char (f code));
    map_code_rec f buf txt ofs
  end

let map_code f txt =
  let buf = Buffer.create (String.length txt) in
  map_code_rec f buf txt 0

external ml_upper : int -> int = "ml_text_upper"
external ml_lower : int -> int = "ml_text_lower"

let upper = map_code ml_upper
let lower = map_code ml_lower

let map_first_code f = function
  | "" -> ""
  | txt ->
      let code, ofs = unsafe_extract_next txt 0 in
      char (f code) ^ unsafe_sub txt ofs (String.length txt - ofs)

let capitalize = map_first_code ml_upper
let uncapitalize = map_first_code ml_lower

(* +-----------------------------------------------------------------+
   | Comparison                                                      |
   +-----------------------------------------------------------------+ *)

external ml_compare : string -> string -> int = "ml_text_compare"

let compare t1 t2 = ml_compare (encode t1) (encode t2)
let icompare t1 t2 = ml_compare (encode (lower t1)) (encode (lower t2))

external ml_transform : t -> t = "ml_text_strxfrm"
let transform str = ml_transform (encode str)
