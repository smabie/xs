(lang dune 2.5)
(name xs)

(generate_opam_files true)

(source (github smabie/xs))
(license ISC)
(authors "Sturm Mabie")
(maintainers "sturm@cryptm.org")

(package
 (name xs)
 (synopsis "xs is a concatenative array language inspired by kdb+/q")
 (description "xs is a unique concatenative language that aims to match the power of q with more flexibility and expressiveness.")
 (depends
  (dune :any)
  (core :any)
  (angstrom :any)
  (res :any)
  (ppx_jane :any)
  (ppx_inline_tests :any)))
