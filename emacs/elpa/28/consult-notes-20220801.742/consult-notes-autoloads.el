;;; consult-notes-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "consult-notes" "consult-notes.el" (0 0 0 0))
;;; Generated autoloads from consult-notes.el

(autoload 'consult-notes "consult-notes" "\
Find a file in a notes directory with consult-multi, or from SOURCES.

\(fn &optional SOURCES)" t nil)

(autoload 'consult-notes-search-in-all-notes "consult-notes" "\
Search in all notes using `grep' or `ripgrep'.
Which search function is used depends on the value of `consult-notes-use-rg'." t nil)

(register-definition-prefixes "consult-notes" '("consult-notes-"))

;;;***

;;;### (autoloads nil "consult-notes-org-roam" "consult-notes-org-roam.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from consult-notes-org-roam.el

(autoload 'consult-notes-org-roam-find-node-relation "consult-notes-org-roam" "\
Navigate org-roam notes by link relation.

With universal ARG tries to navigate the tags of the current
note. Optionally takes a selected NODE and filepaths CHOICES.

\(fn ARG &optional NODE CHOICES)" t nil)

(defvar consult-notes-org-roam-mode nil "\
Non-nil if Consult-Notes-Org-Roam mode is enabled.
See the `consult-notes-org-roam-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `consult-notes-org-roam-mode'.")

(custom-autoload 'consult-notes-org-roam-mode "consult-notes-org-roam" nil)

(autoload 'consult-notes-org-roam-mode "consult-notes-org-roam" "\
Toggle `consult-notes-org-roam-mode' to integrate consult with org-roam.

By enabling `consult-notes-org-roam-mode' the functions
`org-roam-node-read' and `org-roam-ref-read' are overriden by
consults-org-roam's equivalents. Optional argument ARG indicates
whether the mode should be enabled or disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "consult-notes-org-roam" '("consult-notes-org-roam-"))

;;;***

;;;### (autoloads nil nil ("consult-notes-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; consult-notes-autoloads.el ends here
