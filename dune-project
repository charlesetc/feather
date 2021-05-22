(lang dune 2.6)
(name feather)
(version b1fb1b724cc0)
(license MIT)
(homepage "https://hg.sr.ht/~etc/feather")
(bug_reports "https://todo.sr.ht/~etc/feather")
(authors "Charles Chamberlain")
(maintainers "charles@nrwhl.xyz")
(generate_opam_files true)

(package
  (name "feather")
  (synopsis "A minimal shell interface")
  (depends dune base spawn))
