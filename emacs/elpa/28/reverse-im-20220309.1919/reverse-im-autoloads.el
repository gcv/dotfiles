;;; reverse-im-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "reverse-im" "reverse-im.el" (0 0 0 0))
;;; Generated autoloads from reverse-im.el

(autoload 'reverse-im-add-input-method "reverse-im" "\
Add INPUT-METHOD to `reverse-im-input-methods' list using `customize'.

\(fn INPUT-METHOD)" t nil)

(autoload 'reverse-im-which-key-show "reverse-im" "\
Show translation bindings for INPUT-METHOD using `which-key'.

\(fn INPUT-METHOD)" t nil)

(defvar reverse-im-mode nil "\
Non-nil if Reverse-Im mode is enabled.
See the `reverse-im-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `reverse-im-mode'.")

(custom-autoload 'reverse-im-mode "reverse-im" nil)

(autoload 'reverse-im-mode "reverse-im" "\
Toggle reverse-im mode.

This is a minor mode.  If called interactively, toggle the
`Reverse-Im mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='reverse-im-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'reverse-im-translate-region "reverse-im" "\
Translate active region from START to END.  FORCE translate even if the region isn't active.

\(fn START END &optional FORCE)" t nil)

(autoload 'reverse-im-translate-word "reverse-im" "\
Translate word before the point.  With prefix ARG translates ARG words instead of the last one, if ARG is 0 - translate until the beginning of line.

\(fn ARG)" t nil)

(register-definition-prefixes "reverse-im" '("reverse-im-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; reverse-im-autoloads.el ends here
