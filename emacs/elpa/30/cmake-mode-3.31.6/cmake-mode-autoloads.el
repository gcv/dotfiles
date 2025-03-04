;;; cmake-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from cmake-mode.el

(autoload 'cmake-mode "cmake-mode" "\
Major mode for editing CMake source files.

(fn)" t)
(autoload 'cmake-command-run "cmake-mode" "\
Runs the command cmake with the arguments specified.  The
optional argument topic will be appended to the argument list.

(fn TYPE &optional TOPIC BUFFER)" t)
(autoload 'cmake-command-run-help "cmake-mode" "\
`cmake-command-run' but rendered in `rst-mode'.

(fn TYPE &optional TOPIC BUFFER)" t)
(autoload 'cmake-help-list-commands "cmake-mode" "\
Prints out a list of the cmake commands." t)
(autoload 'cmake-help-command "cmake-mode" "\
Prints out the help message for the command the cursor is on." t)
(autoload 'cmake-help-module "cmake-mode" "\
Prints out the help message for the module the cursor is on." t)
(autoload 'cmake-help-variable "cmake-mode" "\
Prints out the help message for the variable the cursor is on." t)
(autoload 'cmake-help-property "cmake-mode" "\
Prints out the help message for the property the cursor is on." t)
(autoload 'cmake-help "cmake-mode" "\
Queries for any of the four available help topics and prints out the
appropriate page." t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(register-definition-prefixes "cmake-mode" '("cmake-"))

;;; End of scraped data

(provide 'cmake-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; cmake-mode-autoloads.el ends here
