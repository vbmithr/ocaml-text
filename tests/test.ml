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

let () =
  test "length" (fun () -> length "abc" = 3);
  test "length" (fun () -> length "ééé" = 3);
  test "code" (fun () -> code "é" = 0xe9);
  test "char" (fun () -> char 0xe9 = "é");
  test "get" (fun () -> get "éà" 1 = "à");
  test "get" (fun () -> get "abc" (-1) = "c");
  test "sub" (fun () -> sub "ocaml" 1 2 = "ca");
  test "sub" (fun () -> sub "ocaml" 3 (-2) = "ca");
  test "sub" (fun () -> sub "ocaml" (-2) 1 = "m");
  test "slice" (fun () -> slice "abc" 0 1 = "a");
  test "slice" (fun () -> slice "abcdef" 1 (-1) = "bcde");
  test "splice" (fun () -> splice "abcd" 1 2 "plop" = "aplopcd");
  test "splice" (fun () -> splice "abcd" 1 2 "" = "acd");
  test "repeat" (fun () -> repeat 5 "a" = "aaaaa");
  test "init" (fun () -> init 5 string_of_int = "01234");
  test "rev_init" (fun () -> rev_init 5 string_of_int = "43210");
  test "upper" (fun () -> upper "abcd" = "ABCD");
  test "lower" (fun () -> lower "ABCD" = "abcd");
  test "capitalize" (fun () -> capitalize "oCaml" = "OCaml");
  test "uncapitalize" (fun () -> uncapitalize "OCaml" = "oCaml");
  test "capitalize" (fun () -> capitalize "" = "");
  test "uncapitalize" (fun () -> uncapitalize "" = "");
  test "compare" (fun () -> compare "abc" "abd" = -1);
  test "icompare" (fun () -> icompare "abc" "ABC" = 0);
  test "rev" (fun () -> rev "ocaml" = "lmaco");
  test "rev" (fun () -> rev "héhé" = "éhéh");
  test "concat" (fun () -> concat "/" ["a"; "b"; "c"] = "a/b/c");
  test "rev_concat" (fun () -> rev_concat "/" ["a"; "b"; "c"] = "c/b/a");
  test "explode" (fun () -> explode "" = []);
  test "explode" (fun () -> explode "abé" = ["a"; "b"; "é"]);
  test "rev_explode" (fun () -> rev_explode "ocaml" = ["l"; "m"; "a"; "c"; "o"]);
  test "implode" (fun () -> implode ["o"; "c"; "a"; "m"; "l"] = "ocaml");
  test "implode" (fun () -> implode ["abc"; "def"] = "abcdef");
  test "implode" (fun () -> rev_implode ["o"; "c"; "a"; "m"; "l"] = "lmaco");
  test "implode" (fun () -> rev_implode ["abc"; "def"] = "defabc");
  test "map" (fun () -> map (function "a" -> "x" | t -> t) "abc" = "xbc");
  test "rev_map" (fun () -> rev_map (function "a" -> "x" | t -> t) "abc" = "cbx");
  test "fold" (fun () -> fold (fun t acc -> acc + code t) "ABC" 0 = 198);
  test "rev_fold" (fun () -> rev_fold (fun t acc -> acc + code t) "ABC" 0 = 198);
  test "filter" (fun () -> filter is_alpha "1a2E" = "aE");
  test "rev_filter" (fun () -> rev_filter is_alpha "1a2E" = "Ea");
  test "for_all" (fun () -> for_all is_ascii "abcd" = true);
  test "for_all" (fun () -> for_all is_digit "1234" = true);
  test "for_all" (fun () -> for_all is_digit "12a" = false);
  test "exists" (fun () -> exists is_alpha "1234" = false);
  test "exists" (fun () -> exists is_alpha "123a" = true);
  test "words" (fun () -> words "Hello, world!" = ["Hello"; "world"]);
  test "lines" (fun () -> lines "foo\nbar" = ["foo"; "bar"]);
  test "lines" (fun () -> lines "foo\nbar\n" = ["foo"; "bar"]);
  test "lines" (fun () -> lines "foo\r\nbar\r\n" = ["foo"; "bar"]);
  test "split" (fun () -> split ~sep:"/" "/usr/share/doc" = [""; "usr"; "share"; "doc"]);
  test "split" (fun () -> split ~sep:"." "192.168.1.1" = ["192"; "168"; "1"; "1"]);
  test "split" (fun () -> split ~sep:"." ~max:2 "192.168.1.1" = ["192"; "168.1.1"]);
  test "split" (fun () -> split ~sep:"/" "a/b/c" = ["a"; "b"; "c"]);
  test "split" (fun () -> split ~sep:".." "a..b..c" = ["a"; "b"; "c"]);
  test "split" (fun () -> split ~max:1 "a b c" = ["a b c"]);
  test "split" (fun () -> split ~max:2 "a b c" = ["a"; "b c"]);
  test "rev_split" (fun () -> rev_split ~sep:"." ~max:2 "192.168.1.1" = ["192.168.1"; "1"]);
  test "replace" (fun () -> replace "abcd" ~patt:"b" ~repl:"x" = "axcd");
  test "replace" (fun () -> replace "Hello world!" ~patt:"world" ~repl:"you" = "Hello you!");
  test "contains" (fun () -> contains "bbaacc" "aa"  = true);
  test "contains" (fun () -> contains "" "" = true);
  test "contains" (fun () -> contains "aaa" "" = true);
  test "contains" (fun () -> contains "abc" "aa" = false);
  test "starts_with" (fun () -> starts_with "abcd" "ab" = true);
  test "starts_with" (fun () -> starts_with "abcd" "af" = false);
  test "starts_with" (fun () -> starts_with "ab" "abcd" = false);
  test "ends_with" (fun () -> ends_with "abcd" "cd" = true);
  test "ends_with" (fun () -> ends_with "abcd" "hd" = false);
  test "ends_with" (fun () -> ends_with "ab" "abc" = false);
  test "strip" (fun () -> strip "   \t\r\n toto \r\n \t" = "toto");
  test "rstrip" (fun () -> rstrip "  foo  " = "  foo");
  test "lstrip" (fun () -> lstrip "  foo  " = "foo  ");
  test "rchop" (fun () -> rchop "abcd" = "abc");
  test "lchop" (fun () -> lchop "abcd" = "bcd");
  Printf.printf "\nsuccess: %d\nfailure: %d\n%!" !success !failure
