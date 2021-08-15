(lang dune 2.7)
(name feather)
(version 0.3.0)
(license MIT)
(homepage "https://github.com/charlesetc/feather")
(source (github charlesetc/feather))
(bug_reports "https://github.com/charlesetc/feather/issues")
(authors "Charles Chamberlain")
(maintainers "charles@nrwhl.xyz")
(generate_opam_files true)

(package
  (name "feather")
  (documentation "https://www.charlesetc.com/feather/feather/Feather/index.html")
  (synopsis "A minimal shell interface")
  (depends
   (ocaml (>= 4.08))
   dune
   base
   ppx_expect
   stdio
   spawn))

(package
  (name "feather_async")
  (synopsis "Async interface to Feather")
  (depends
    (ocaml (>= 4.08))
    (feather (= 0.3.0))
    dune
    base
    async))

