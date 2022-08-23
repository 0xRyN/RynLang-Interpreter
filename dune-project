(lang dune 3.4)

(name RynLang-Interpreter)

(generate_opam_files true)

(source
 (github 0xRyn/RynLang-Interpreter))

(authors "0xRyN")

(maintainers "0xRyN")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name RynLang-Interpreter)
 (synopsis "RynLang Interpreter")
 (description
  "An OCaml interpreter for an imperative all purpose programming language. (Personal project, all from scratch, WIP)")
 (depends ocaml dune)
 (tags
  (topics "ocaml parser lexer interpreter" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
