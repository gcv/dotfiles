;;; inheritenv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "inheritenv" "inheritenv.el" (0 0 0 0))
;;; Generated autoloads from inheritenv.el

(autoload 'inheritenv-apply "inheritenv" "\
Apply FUNC such that the environment it sees will match the current value.
This is useful if FUNC creates a temp buffer, because that will
not inherit any buffer-local values of variables `exec-path' and
`process-environment'.

This function is designed for convenient use as an \"around\" advice.

ARGS is as for ORIG.

\(fn FUNC &rest ARGS)" nil nil)

(autoload 'inheritenv "inheritenv" "\
Wrap BODY so that the environment it sees will match the current value.
This is useful if BODY creates a temp buffer, because that will
not inherit any buffer-local values of variables `exec-path' and
`process-environment'.

\(fn &rest BODY)" nil t)

(autoload 'inheritenv-add-advice "inheritenv" "\
Advise function FUNC with `inheritenv-apply'.
This will ensure that any buffers (including temporary buffers)
created by FUNC will inherit the caller's environment.

\(fn FUNC)" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; inheritenv-autoloads.el ends here
