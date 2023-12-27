(lang dune 2.8)
(name levenfind)
(version 0.2.1)
(source (github smimram/levenfind))
(license GPL-3.0)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")

(generate_opam_files true)
(package
 (name levenfind)
 (synopsis "Find similar files")
 (description "Find pairs of similar files according to Levenshtein distance.")
 (depends
   domainslib
 )
)
