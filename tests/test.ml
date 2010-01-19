(*
 * test.ml
 * -------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

open Printf
open Text

let success = ref 0
let failure = ref 0

let test name func =
  try
    match func () with
      | true ->
          incr success
      | false ->
          printf "test %s failed\n%!" name;
          incr failure
  with exn ->
    incr failure;
    printf "test %s raised an exception: %s" name (Printexc.to_string exn)

DEFINE TEST(name, expr) = (test name (fun () -> expr))

let () =
  TEST("length", length "abc" = 3);
  TEST("length", length "ééé" = 3);
  TEST("code", code "é" = 0xe9);
  TEST("char", char 0xe9 = "é");
  TEST("get", get "éà" 1 = "à");
  TEST("get", get "abc" (-1) = "c");
  TEST("sub", sub "ocaml" 1 2 = "ca");
  TEST("sub", sub "ocaml" 3 (-2) = "ca");
  TEST("sub", sub "ocaml" (-2) 1 = "m");
  TEST("slice", slice "abc" 0 1 = "a");
  TEST("slice", slice "abcdef" 1 (-1) = "bcde");
  TEST("splice", splice "abcd" 1 2 "plop" = "aplopcd");
  TEST("splice", splice "abcd" 1 2 "" = "acd");
  TEST("repeat", repeat 5 "a" = "aaaaa");
  TEST("init", init 5 string_of_int = "01234");
  TEST("rev_init", rev_init 5 string_of_int = "43210");
  TEST("upper", upper "abcd" = "ABCD");
  TEST("lower", lower "ABCD" = "abcd");
  TEST("capitalize", capitalize "oCaml" = "OCaml");
  TEST("uncapitalize", uncapitalize "OCaml" = "oCaml");
  TEST("capitalize", capitalize "" = "");
  TEST("uncapitalize", uncapitalize "" = "");
  TEST("compare", compare "abc" "abd" = -1);
  TEST("icompare", icompare "abc" "ABC" = 0);
  TEST("rev", rev "ocaml" = "lmaco");
  TEST("rev", rev "héhé" = "éhéh");
  TEST("concat", concat "/" ["a"; "b"; "c"] = "a/b/c");
  TEST("rev_concat", rev_concat "/" ["a"; "b"; "c"] = "c/b/a");
  TEST("explode", explode "" = []);
  TEST("explode", explode "abé" = ["a"; "b"; "é"]);
  TEST("rev_explode", rev_explode "ocaml" = ["l"; "m"; "a"; "c"; "o"]);
  TEST("implode", implode ["o"; "c"; "a"; "m"; "l"] = "ocaml");
  TEST("implode", implode ["abc"; "def"] = "abcdef");
  TEST("implode", rev_implode ["o"; "c"; "a"; "m"; "l"] = "lmaco");
  TEST("implode", rev_implode ["abc"; "def"] = "defabc");
  TEST("map", map (function "a" -> "x" | t -> t) "abc" = "xbc");
  TEST("rev_map", rev_map (function "a" -> "x" | t -> t) "abc" = "cbx");
  TEST("fold", fold (fun t acc -> acc + code t) "ABC" 0 = 198);
  TEST("rev_fold", rev_fold (fun t acc -> acc + code t) "ABC" 0 = 198);
  TEST("filter", filter is_alpha "1a2E" = "aE");
  TEST("rev_filter", rev_filter is_alpha "1a2E" = "Ea");
  TEST("for_all", for_all is_ascii "abcd" = true);
  TEST("for_all", for_all is_digit "1234" = true);
  TEST("for_all", for_all is_digit "12a" = false);
  TEST("exists", exists is_alpha "1234" = false);
  TEST("exists", exists is_alpha "123a" = true);
  TEST("words", words "Hello, world!" = ["Hello"; "world"]);
  TEST("lines", lines "foo\nbar" = ["foo"; "bar"]);
  TEST("lines", lines "foo\nbar\n" = ["foo"; "bar"]);
  TEST("lines", lines "foo\r\nbar\r\n" = ["foo"; "bar"]);
  TEST("split", split ~sep:"/" "/usr/share/doc" = [""; "usr"; "share"; "doc"]);
  TEST("split", split ~sep:"." "192.168.1.1" = ["192"; "168"; "1"; "1"]);
  TEST("split", split ~sep:"." ~max:2 "192.168.1.1" = ["192"; "168.1.1"]);
  TEST("split", split ~sep:"/" "a/b/c" = ["a"; "b"; "c"]);
  TEST("split", split ~sep:".." "a..b..c" = ["a"; "b"; "c"]);
  TEST("split", split ~max:1 "a b c" = ["a b c"]);
  TEST("split", split ~max:2 "a b c" = ["a"; "b c"]);
  TEST("rev_split", rev_split ~sep:"." ~max:2 "192.168.1.1" = ["192.168.1"; "1"]);
  TEST("replace", replace "abcd" ~patt:"b" ~repl:"x" = "axcd");
  TEST("replace", replace "Hello world!" ~patt:"world" ~repl:"you" = "Hello you!");
  TEST("contains", contains "bbaacc" "aa"  = true);
  TEST("contains", contains "" "" = true);
  TEST("contains", contains "aaa" "" = true);
  TEST("contains", contains "abc" "aa" = false);
  TEST("starts_with", starts_with "abcd" "ab" = true);
  TEST("starts_with", starts_with "abcd" "af" = false);
  TEST("starts_with", starts_with "ab" "abcd" = false);
  TEST("ends_with", ends_with "abcd" "cd" = true);
  TEST("ends_with", ends_with "abcd" "hd" = false);
  TEST("ends_with", ends_with "ab" "abc" = false);
  TEST("strip", strip "   \t\r\n toto \r\n \t" = "toto");
  TEST("rstrip", rstrip "  foo  " = "  foo");
  TEST("lstrip", lstrip "  foo  " = "foo  ");
  TEST("rchop", rchop "abcd" = "abc");
  TEST("lchop", lchop "abcd" = "bcd");
  Printf.printf "\nsuccess: %d\nfailure: %d\n%!" !success !failure
