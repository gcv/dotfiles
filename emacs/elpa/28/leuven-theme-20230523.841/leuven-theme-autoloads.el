;;; leuven-theme-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "leuven-dark-theme" "leuven-dark-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from leuven-dark-theme.el

(autoload 'leuven-dark-scale-font "leuven-dark-theme" "\
Function for splicing optional font heights into face descriptions.
CONTROL can be a number, nil, or t.  When t, use DEFAULT-HEIGHT.

\(fn CONTROL DEFAULT-HEIGHT)" nil nil)

(when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(register-definition-prefixes "leuven-dark-theme" '("leuven-dark"))

;;;***

;;;### (autoloads nil "leuven-theme" "leuven-theme.el" (0 0 0 0))
;;; Generated autoloads from leuven-theme.el

(autoload 'leuven-scale-font "leuven-theme" "\
Function for splicing optional font heights into face descriptions.
CONTROL can be a number, nil, or t.  When t, use DEFAULT-HEIGHT.

\(fn CONTROL DEFAULT-HEIGHT)" nil nil)

(when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(register-definition-prefixes "leuven-theme" '("leuven"))

;;;***

;;;### (autoloads nil nil ("leuven-theme-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; leuven-theme-autoloads.el ends here
