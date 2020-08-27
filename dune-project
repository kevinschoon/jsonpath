(lang dune 2.7)

(name jsonpath)
(version 0.0.1)

(generate_opam_files true)

(source (github kevinschoon/jsonpath))
(authors "Kevin Schoon <kevinschoon@gmail.com>")
(maintainers "Kevin Schoon <kevinschoon@gmail.com>")

(package
 (name jsonpath)
 (synopsis "JSON data extraction utility")
 (description "JSON data extraction utility")
 (depends
  (dune
   (>= 2.7))
  (yojson
   (>= 1.7.0))))
(using menhir 2.1)
