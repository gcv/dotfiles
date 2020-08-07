;;; selectrum-prescient-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "selectrum-prescient" "selectrum-prescient.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from selectrum-prescient.el

(defvar selectrum-prescient-mode nil "\
Non-nil if Selectrum-Prescient mode is enabled.
See the `selectrum-prescient-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `selectrum-prescient-mode'.")

(custom-autoload 'selectrum-prescient-mode "selectrum-prescient" nil)

(autoload 'selectrum-prescient-mode "selectrum-prescient" "\
Minor mode to use prescient.el in Selectrum menus.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "selectrum-prescient" '("selectrum-prescient--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; selectrum-prescient-autoloads.el ends here
