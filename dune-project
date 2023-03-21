(lang dune 3.4)

(name SimPL)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github gaganchandan/SimPL))

(authors "Gagan Chandan")

(maintainers "Gagan Chandan")

(license LICENSE)

(package
 (name SimPL)
 (synopsis "An interpreter for the SimPL programming language")
 (depends ocaml dune)
