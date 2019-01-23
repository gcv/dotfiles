;;; emojify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emojify" "emojify.el" (0 0 0 0))
;;; Generated autoloads from emojify.el

(autoload 'emojify-set-emoji-styles "emojify" "\
Set the type of emojis that should be displayed.\n\nSTYLES is the styles emoji styles that should be used, see `emojify-emoji-styles'\n\n(fn STYLES)" nil nil)

(autoload 'emojify-mode "emojify" "\
Emojify mode\n\n(fn &optional ARG)" t nil)

(defvar global-emojify-mode nil "\
Non-nil if Global Emojify mode is enabled.\nSee the `global-emojify-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-emojify-mode'.")

(custom-autoload 'global-emojify-mode "emojify" nil)

(autoload 'global-emojify-mode "emojify" "\
Toggle Emojify mode in all buffers.\nWith prefix ARG, enable Global Emojify mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nEmojify mode is enabled in all buffers where\n`emojify-mode' would do it.\nSee `emojify-mode' for more information on Emojify mode.\n\n(fn &optional ARG)" t nil)

(autoload 'emojify-mode-line-mode "emojify" "\
Emojify mode line\n\n(fn &optional ARG)" t nil)

(defvar global-emojify-mode-line-mode nil "\
Non-nil if Global Emojify-Mode-Line mode is enabled.\nSee the `global-emojify-mode-line-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-emojify-mode-line-mode'.")

(custom-autoload 'global-emojify-mode-line-mode "emojify" nil)

(autoload 'global-emojify-mode-line-mode "emojify" "\
Toggle Emojify-Mode-Line mode in all buffers.\nWith prefix ARG, enable Global Emojify-Mode-Line mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nEmojify-Mode-Line mode is enabled in all buffers where\n`emojify-mode-line-mode' would do it.\nSee `emojify-mode-line-mode' for more information on Emojify-Mode-Line mode.\n\n(fn &optional ARG)" t nil)

(autoload 'emojify-apropos-emoji "emojify" "\
Show Emojis that match PATTERN.\n\n(fn PATTERN)" t nil)

(autoload 'emojify-insert-emoji "emojify" "\
Interactively prompt for Emojis and insert them in the current buffer.\n\nThis respects the `emojify-emoji-styles' variable.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emojify" '("emojify-")))

;;;***

;;;### (autoloads nil nil ("emojify-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emojify-autoloads.el ends here
