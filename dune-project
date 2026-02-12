(lang dune 2.7)

(name feather)

(version 0.4.0)

(license MIT)

(homepage "https://github.com/charlesetc/feather")

(source
 (github charlesetc/feather))

(bug_reports "https://github.com/charlesetc/feather/issues")

(authors "Charles Chamberlain")

(maintainers "charles@nrwhl.xyz")

(generate_opam_files true)

(package
 (name "feather")
 (documentation
   "https://www.charlesetc.com/feather/feather/Feather/index.html")
 (synopsis "A minimal shell interface")
 (depends
  (ocaml
   (>= 4.08))
  (base
   (>= v0.13.0))
  ppx_expect
  ppx_sexp_conv
  stdio
  (spawn
   (>= v0.13.0))))

(package
 (name "feather_async")
 (synopsis "Async interface to Feather")
 (depends
  (ocaml
   (>= 4.08))
  (feather
   (= :version))
  base
  async))

(package
 (name "feather_tests")
 (synopsis "Extended tests and dev package for feather")
 (depends
  base
  core
  core_unix
  (feather
   (= :version))
  (feather_async
   (= :version))
  ppx_expect
  ppx_let
  ppx_sexp_message
  ppx_string
  stdio))
