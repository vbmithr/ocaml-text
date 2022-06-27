(lang dune 2.0)
(name text)
(generate_opam_files true)

(source (github vbmithr/ocaml-text))
(license  BSD-3-Clause)
(authors "Jérémie Dimino")
(maintainers "vb@luminar.eu.org")

(package
  (name text)
  (synopsis
    "Library for dealing with \"text\", i.e. sequence of unicode characters, in a convenient way")
  (description "It supports:
  - character encoding/decoding using iconv
  - manipulation of text as UTF-8 encoded strings
  - localised text functions such as compare, upper, ...
  - human readable regular expression inside patterns
")
  (depends
    (ocaml (>= 4.03))))

(package
  (name text_pcre)
  (depends
    (text (= version))
    pcre))

(package
  (name text_pcre_syntax)
  (depends
    (text (= version))
    camlp4
    pcre))
