;;; dtrt-indent-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from dtrt-indent.el

(autoload 'dtrt-indent-mode "dtrt-indent" "\
Toggle dtrt-indent mode.

With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When dtrt-indent mode is enabled, the proper indentation offset
and `indent-tabs-mode' will be guessed for newly opened files and
adjusted transparently.

This is a minor mode.  If called interactively, toggle the `Dtrt-Indent
mode' mode.  If the prefix argument is positive, enable the mode, and if
it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `dtrt-indent-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(put 'dtrt-indent-global-mode 'globalized-minor-mode t)
(defvar dtrt-indent-global-mode nil "\
Non-nil if Dtrt-Indent-Global mode is enabled.
See the `dtrt-indent-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dtrt-indent-global-mode'.")
(custom-autoload 'dtrt-indent-global-mode "dtrt-indent" nil)
(autoload 'dtrt-indent-global-mode "dtrt-indent" "\
Toggle Dtrt-Indent mode in all buffers.
With prefix ARG, enable Dtrt-Indent-Global mode if ARG is positive; otherwise,
disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Dtrt-Indent mode is enabled in all buffers where `dtrt-indent--mode' would do
it.

See `dtrt-indent-mode' for more information on Dtrt-Indent mode.

(fn &optional ARG)" t)
(defvar dtrt-indent-mode nil "\
Toggle adaptive indentation mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `dtrt-indent-mode'.")
(custom-autoload 'dtrt-indent-mode "dtrt-indent" nil)
(register-definition-prefixes "dtrt-indent" '("dtrt-indent-"))


;;; Generated autoloads from dtrt-indent-diag.el

(register-definition-prefixes "dtrt-indent-diag" '("dtrt-indent-" "save-buffer-state"))

;;; End of scraped data

(provide 'dtrt-indent-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; dtrt-indent-autoloads.el ends here
