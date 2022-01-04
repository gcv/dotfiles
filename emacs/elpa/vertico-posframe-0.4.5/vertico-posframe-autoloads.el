;;; vertico-posframe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vertico-posframe" "vertico-posframe.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from vertico-posframe.el

(autoload 'vertico-posframe-cleanup "vertico-posframe" "\
Remove frames and buffers used for vertico-posframe." t nil)

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

If called interactively, enable Vertico-Posframe mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vertico-posframe" '("vertico-posframe-")))

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
