;;; vertico-posframe-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vertico-posframe" "vertico-posframe.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from vertico-posframe.el

(defvar vertico-posframe-mode nil "\
Non-nil if Vertico-Posframe mode is enabled.
See the `vertico-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-posframe-mode'.")

(custom-autoload 'vertico-posframe-mode "vertico-posframe" nil)

(autoload 'vertico-posframe-mode "vertico-posframe" "\
Display Vertico in posframe instead of the minibuffer.

This is a minor mode.  If called interactively, toggle the
`Vertico-Posframe mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-posframe-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'vertico-posframe-cleanup "vertico-posframe" "\
Remove frames and buffers used for vertico-posframe." t nil)

(register-definition-prefixes "vertico-posframe" '("vertico-posframe-"))

;;;***

;;;### (autoloads nil nil ("vertico-posframe-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vertico-posframe-autoloads.el ends here
