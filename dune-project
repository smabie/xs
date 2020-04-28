(lang dune 2.5)
(name xs)

(generate_opam_files true)

(source (github smabie/xs))
(license ISC)
(authors "Sturm Mabie")
(maintainers "sturm@cryptm.org")

(package
 (name xs)
 (synopsis "Xs is a concatenative array language inspired by kdb+/q")
 (description "Xs is a unique concatenative language that aims to match the power of q with more flexibility and expressiveness")
 (depends
  ocaml dune core angstrom res ppx_jane ppx_inline_test))
