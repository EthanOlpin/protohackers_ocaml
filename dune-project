(lang dune 3.10)

(name protohackers_ocaml)

(generate_opam_files true)

(source
 (github ethanolpin/protohackers_ocaml))

(authors "Ethan Olpin")

(maintainers "Ethan Olpin")

(package
 (name protohackers_ocaml)
 (synopsis "Protohackers problem set solutions")
 (depends ocaml dune core core_unix eio eio_main yojson zarith re)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
