(lang dune 2.7)

(cram enable)
(name ez-header)
(allow_approximate_merlin)
(generate_opam_files true)
(version 0.1.0)
(formatting (enabled_for ocaml reason))

(package
 (name ez-header)
 (synopsis "This tool simply updates headers of an OCaml project.")
 (description "This tool simply updates headers of an OCaml project.")
 (depends
   (ocaml (>= 4.14.0))
  )
 )


