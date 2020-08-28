;;; julia-snail-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "julia-snail" "julia-snail.el" (0 0 0 0))
;;; Generated autoloads from julia-snail.el

(autoload 'julia-snail "julia-snail" "\
Start a Julia REPL and connect to it, or switch if one already exists.
The following buffer-local variables control it:
- `julia-snail-repl-buffer' (default: *julia*)
- `julia-snail-port' (default: 10011)
To create multiple REPLs, give these variables distinct values (e.g.:
*julia my-project-1* and 10012)." t nil)

(autoload 'julia-snail-mode "julia-snail" "\
A minor mode for interactive Julia development. Should only be turned on in source buffers.

If called interactively, enable Julia-Snail mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'julia-snail-repl-mode "julia-snail" "\
A minor mode for interactive Julia development. Should only be
turned on in REPL buffers.

If called interactively, enable Julia-Snail-Repl mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "julia-snail" '("julia-snail-")))

;;;***

;;;### (autoloads nil "julia-snail-parser" "julia-snail-parser.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from julia-snail-parser.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "julia-snail-parser" '("julia-snail-parser-")))

;;;***

;;;### (autoloads nil nil ("julia-snail-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; julia-snail-autoloads.el ends here
