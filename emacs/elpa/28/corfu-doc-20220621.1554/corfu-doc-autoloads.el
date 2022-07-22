;;; corfu-doc-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "corfu-doc" "corfu-doc.el" (0 0 0 0))
;;; Generated autoloads from corfu-doc.el

(defvar corfu-doc-mode nil "\
Non-nil if Corfu-Doc mode is enabled.
See the `corfu-doc-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `corfu-doc-mode'.")

(custom-autoload 'corfu-doc-mode "corfu-doc" nil)

(autoload 'corfu-doc-mode "corfu-doc" "\
Corfu doc minor mode.

This is a minor mode.  If called interactively, toggle the
`Corfu-Doc mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='corfu-doc-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'corfu-doc-scroll-up "corfu-doc" "\
Scroll text of doc popup window upward N lines.

If ARG is omitted or nil, scroll upward by a near full screen.

\(fn &optional N)" t nil)

(autoload 'corfu-doc-scroll-down "corfu-doc" "\
Scroll text of doc popup window down N lines.

If ARG is omitted or nil, scroll down by a near full screen.

\(fn &optional N)" t nil)

(autoload 'corfu-doc-toggle "corfu-doc" "\
Toggle the doc popup display or hide.

When using this command to manually hide the doc popup, it will
not be displayed until this command is called again. Even if the
corfu doc mode is turned on and `corfu-doc-auto' is set to Non-nil." t nil)

(define-obsolete-function-alias 'toggle-corfu-doc-mode #'corfu-doc-mode "0.7")

(register-definition-prefixes "corfu-doc" '("corfu-doc-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; corfu-doc-autoloads.el ends here
