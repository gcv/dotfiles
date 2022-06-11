;;; fountain-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fountain-mode" "fountain-mode.el" (0 0 0 0))
;;; Generated autoloads from fountain-mode.el

(defvar fountain-completion-auto-update-mode nil "\
Non-nil if Fountain-Completion-Auto-Update mode is enabled.
See the `fountain-completion-auto-update-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fountain-completion-auto-update-mode'.")

(custom-autoload 'fountain-completion-auto-update-mode "fountain-mode" nil)

(autoload 'fountain-completion-auto-update-mode "fountain-mode" "\
Updates `fountain-mode' completion candidates when idle.
Calls `fountain-completion-update' in `fountain-mode' buffers
after `fountain-completion-auto-update-delay'.

This is a minor mode.  If called interactively, toggle the
`Fountain-Completion-Auto-Update mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the mode if ARG
is nil, omitted, or is a positive number.  Disable the mode if ARG is a negative
number.

To check whether the minor mode is enabled in the current buffer, evaluate
`(default-value \\='fountain-completion-auto-update-mode)'.

The mode's hook is called both when the mode is enabled and when it is disabled.

\(fn &optional ARG)" t nil)

(add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode))

(autoload 'fountain-mode "fountain-mode" "\
Major mode for screenwriting in Fountain markup.

\(fn)" t nil)

(register-definition-prefixes "fountain-mode" '("define-fountain-font-lock-matcher" "fountain-"))

;;;***

;;;### (autoloads nil "fountain-theme" "fountain-theme.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from fountain-theme.el

(register-definition-prefixes "fountain-theme" '("fountain"))

;;;***

;;;### (autoloads nil nil ("fountain-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fountain-mode-autoloads.el ends here
