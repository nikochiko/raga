(lang dune 3.20)

(name raga)

(version 0.1.0)

(generate_opam_files true)

(source
 (github nikochiko/raga))

(authors "kaustubh m <k@kaustubh.page>")

(maintainers "kaustubh m <k@kaustubh.page>")

(license MIT)

(documentation https://github.com/nikochiko/raga)

(package
 (name raga)
 (synopsis "simple static site generator")
 (description "Simple static site generator with Handlebars templating")
 (depends
   ocaml
   (handlebars-ml (>= 0.2.0))
   (huml-ml (>= v0.1.1))
   re
   (omd (>= 2.0.0))
   uri
   ptime)
 (tags
  ("handlebars" "huml" "markdown" "static-site-generator")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
