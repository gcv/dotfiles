;;; fountain-mode-autoloads.el --- automatically extracted autoloads
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

If called interactively, enable Fountain-Completion-Auto-Update mode if ARG is
positive, and disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is `toggle';
disable the mode otherwise.

\(fn &optional ARG)" t nil)

(add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode))

(autoload 'fountain-mode "fountain-mode" "\
Major mode for screenwriting in Fountain markup.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fountain-mode" '("define-fountain-font-lock-matcher" "fountain-")))

;;;***

;;;### (autoloads nil "fountain-theme" "fountain-theme.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from fountain-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fountain-theme" '("fountain")))

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
