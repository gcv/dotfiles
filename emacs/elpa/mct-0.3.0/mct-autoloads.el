;;; mct-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mct" "mct.el" (0 0 0 0))
;;; Generated autoloads from mct.el

(autoload 'mct-focus-minibuffer "mct" "\
Focus the active minibuffer." t nil)

(autoload 'mct-focus-mini-or-completions "mct" "\
Focus the active minibuffer or the completions' window.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`mct-focus-minibuffer' and `switch-to-completions' in
succession.

What constitutes a completions' window is ultimately determined
by `mct-completion-windows-regexp'." t nil)

(autoload 'mct-list-completions-toggle "mct" "\
Toggle the presentation of the completions' buffer." t nil)

(defvar mct-mode nil "\
Non-nil if Mct mode is enabled.
See the `mct-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mct-mode'.")

(custom-autoload 'mct-mode "mct" nil)

(autoload 'mct-mode "mct" "\
Set up opinionated default completion UI.

If called interactively, enable Mct mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mct" '("mct-")))

;;;***

;;;### (autoloads nil nil ("mct-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mct-autoloads.el ends here
