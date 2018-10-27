;;; tide-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "tide" "../../../../../.emacs.d/elpa/tide-2.8.3.1/tide.el"
;;;;;;  "49f99a3341cd7d840e97aa758ffc1bc8")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/tide-2.8.3.1/tide.el

(autoload 'company-tide "tide" "\


\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'tide-format-before-save "tide" "\
Before save hook to format the buffer before each save.

\(fn)" t nil)

(autoload 'tide-format "tide" "\
Format the current region or buffer.

\(fn)" t nil)

(autoload 'tide-setup "tide" "\
Setup `tide-mode' in current buffer.

\(fn)" t nil)

(autoload 'tide-mode "tide" "\
Minor mode for Typescript Interactive Development Environment.

\\{tide-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'tide-project-errors "tide" "\


\(fn)" t nil)

(autoload 'tide-unhighlight-identifiers "tide" "\
Remove highlights from previously highlighted identifier.

\(fn)" nil nil)

(autoload 'tide-hl-identifier "tide" "\
Highlight all instances of the identifier under point. Removes
highlights from previously highlighted identifier.

\(fn)" t nil)

(autoload 'tide-hl-identifier-mode "tide" "\
Highlight instances of the identifier at point after a short
timeout.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/tide-2.8.3.1/tide-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/tide-2.8.3.1/tide-lv.el" "../../../../../.emacs.d/elpa/tide-2.8.3.1/tide-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/tide-2.8.3.1/tide.el") (23508
;;;;;;  57696 437439 352000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; tide-autoloads.el ends here
