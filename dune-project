(lang dune 2.7)
(name feather)
(version 0.2.0)
(license MIT)
(homepage "https://github.com/charlesetc/feather")
(source (github charlesetc/feather))
(bug_reports "https://github.com/charlesetc/feather/issues")
(authors "Charles Chamberlain")
(maintainers "charles@nrwhl.xyz")
(generate_opam_files true)

(package
  (name "feather")
  (synopsis "A minimal shell interface")
  (depends
   (ocaml (>= 4.08))
   dune
   base
   ppx_expect
   stdio
   spawn))
