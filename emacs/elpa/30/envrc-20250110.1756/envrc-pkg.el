;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "envrc" "20250110.1756"
  "Support for `direnv' that operates buffer-locally."
  '((emacs      "26.1")
    (inheritenv "0.1")
    (seq        "2.24"))
  :url "https://github.com/purcell/envrc"
  :commit "2b818ca6e4a2f723e7cab70cd0101c2728581c3a"
  :revdesc "2b818ca6e4a2"
  :keywords '("processes" "tools")
  :authors '(("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Steve Purcell" . "steve@sanityinc.com")))
