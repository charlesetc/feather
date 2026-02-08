(lang dune 2.7)
(name feather)
(version 0.4.0)
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
   (base (>= 0.13.0))
   ppx_expect
   (ppx_string :with-test)
   (ppx_let :with-test)
   (ppx_sexp_message :with-test)
   stdio
   (spawn (>= 0.13.0))))

(package
  (name "feather_async")
  (synopsis "Async interface to Feather")
  (depends
    (ocaml (>= 4.08))
    (feather (= version))
    dune
    base
    async))

