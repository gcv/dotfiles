;;; pfuture-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "pfuture" "../../../../../.emacs.d/elpa/pfuture-1.2/pfuture.el"
;;;;;;  "696e82f4fa0c3be9f25b2942b012b90b")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/pfuture-1.2/pfuture.el

(autoload 'pfuture-new "pfuture" "\
Create a new future process for command CMD and arguments CMD-ARGS.
This will return a process object with one additional 'result property which
can be read via (process-get process 'result) or alternatively with
\(pfuture-result process).

Note that CMD-ARGS must be a *sequence* of strings, such that
this is wrong: (pfuture-new \"git status\")
this is right: (pfuture-new \"git\" \"status\")

\(fn CMD &rest CMD-ARGS)" nil nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/pfuture-1.2/pfuture-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/pfuture-1.2/pfuture.el") (23508
;;;;;;  57450 298992 160000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pfuture-autoloads.el ends here
