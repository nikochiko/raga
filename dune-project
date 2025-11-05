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
 (synopsis "Simple static site generator")
 (description "Simple static site generator with Handlebars templating")
 (depends
   ocaml
   (handlebars-ml (>= 0.3.0))
   (huml (>= 0.1.1))
   (re (>= 1.10.3))
   (omd (>= 2.0.0~alpha1))
   uri
   ptime)
 (tags
  ("handlebars" "huml" "markdown" "static-site-generator")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
