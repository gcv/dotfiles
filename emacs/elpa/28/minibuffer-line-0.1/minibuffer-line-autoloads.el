;;; minibuffer-line-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "minibuffer-line" "minibuffer-line.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from minibuffer-line.el

(defvar minibuffer-line-mode nil "\
Non-nil if Minibuffer-Line mode is enabled.
See the `minibuffer-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `minibuffer-line-mode'.")

(custom-autoload 'minibuffer-line-mode "minibuffer-line" nil)

(autoload 'minibuffer-line-mode "minibuffer-line" "\
Display status info in the minibuffer window.

This is a minor mode.  If called interactively, toggle the
`Minibuffer-Line mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='minibuffer-line-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "minibuffer-line" '("minibuffer-line-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; minibuffer-line-autoloads.el ends here
