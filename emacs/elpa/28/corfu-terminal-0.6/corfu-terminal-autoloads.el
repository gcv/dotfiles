;;; corfu-terminal-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "corfu-terminal" "corfu-terminal.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from corfu-terminal.el

(defvar corfu-terminal-mode nil "\
Non-nil if Corfu-Terminal mode is enabled.
See the `corfu-terminal-mode' command
for a description of this minor mode.")

(custom-autoload 'corfu-terminal-mode "corfu-terminal" nil)

(autoload 'corfu-terminal-mode "corfu-terminal" "\
Corfu popup on terminal.

This is a minor mode.  If called interactively, toggle the `Corfu-Terminal mode'
mode.  If the prefix argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the mode if ARG
is nil, omitted, or is a positive number.  Disable the mode if ARG is a negative
number.

To check whether the minor mode is enabled in the current buffer, evaluate
`(default-value \\='corfu-terminal-mode)'.

The mode's hook is called both when the mode is enabled and when it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "corfu-terminal" '("corfu-terminal-"))

;;;***

;;;### (autoloads nil nil ("corfu-terminal-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; corfu-terminal-autoloads.el ends here
