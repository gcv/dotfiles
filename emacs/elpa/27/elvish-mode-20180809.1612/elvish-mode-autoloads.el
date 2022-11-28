;;; elvish-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elvish-mode" "elvish-mode.el" (0 0 0 0))
;;; Generated autoloads from elvish-mode.el

(autoload 'elvish-mode "elvish-mode" "\
Major mode for the Elvish language

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.elv\\'" . elvish-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elvish-mode" '("elvish-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elvish-mode-autoloads.el ends here
