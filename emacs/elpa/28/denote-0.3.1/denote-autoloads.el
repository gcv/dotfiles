;;; denote-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "denote" "denote.el" (0 0 0 0))
;;; Generated autoloads from denote.el
 (put 'denote-directory 'safe-local-variable (lambda (val) (or (eq val 'local) (eq val 'default-directory))))

(autoload 'denote "denote" "\
Create a new note with the appropriate metadata and file name.

When called interactively, the metadata and file name are prompted
according to the value of `denote-prompts'.

When called from Lisp, all arguments are optional.

- TITLE is a string or a function returning a string.

- KEYWORDS is a list of strings.  The list can be empty or the
  value can be set to nil.

- FILE-TYPE is a symbol among those described in `denote-file-type'.

- SUBDIRECTORY is a string representing the path to either the value of
  the variable `denote-directory' or a subdirectory thereof.  The
  subdirectory must exist: Denote will not create it.

- DATE is a string representing a date like 2022-06-30 or a date
  and time like 2022-06-16 14:30.  A nil value or an empty string
  is interpreted as the `current-time'.

\(fn &optional TITLE KEYWORDS FILE-TYPE SUBDIRECTORY DATE)" t nil)

(autoload 'denote-type "denote" "\
Create note while prompting for a file type.

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\'(file-type title keywords)." t nil)

(function-put 'denote-type 'interactive-only 't)

(autoload 'denote-date "denote" "\
Create note while prompting for a date.

The date can be in YEAR-MONTH-DAY notation like 2022-06-30 or
that plus the time: 2022-06-16 14:30

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\'(date title keywords)." t nil)

(function-put 'denote-date 'interactive-only 't)

(autoload 'denote-subdirectory "denote" "\
Create note while prompting for a subdirectory.

Available candidates include the value of the variable
`denote-directory' and any subdirectory thereof.

This is equivalent to calling `denote' when `denote-prompts' is set to
\\'(subdirectory title keywords)." t nil)

(function-put 'denote-subdirectory 'interactive-only 't)

(register-definition-prefixes "denote" '("denote-"))

;;;***

;;;### (autoloads nil "denote-dired" "denote-dired.el" (0 0 0 0))
;;; Generated autoloads from denote-dired.el

(autoload 'denote-dired-rename-file "denote-dired" "\
Rename FILE to include TITLE and KEYWORDS.

If in Dired, consider FILE to be the one at point, else prompt
with completion.

If FILE has a Denote-compliant identifier, retain it while
updating the TITLE and KEYWORDS fields of the file name.  Else
create an identifier based on the file's attribute of last
modification time.  If such attribute cannot be found, the
identifier falls back to the current time.

As a final step, prompt for confirmation, showing the difference
between old and new file names.  If `denote-dired-rename-expert'
is non-nil, conduct the renaming operation outright---no
questions asked!

The file type extension (e.g. .pdf) is read from the underlying
file and is preserved through the renaming process.  Files that
have no extension are simply left without one.

Renaming only occurs relative to the current directory.  Files
are not moved between directories.  As a final step, call the
`denote-dired-post-rename-functions'.

This command is intended to (i) rename existing Denote
notes, (ii) complement note-taking, such as by renaming
attachments that the user adds to their notes.

\(fn FILE TITLE KEYWORDS)" t nil)

(autoload 'denote-dired-mode "denote-dired" "\
Fontify all Denote-style file names in Dired.

This is a minor mode.  If called interactively, toggle the
`Denote-Dired mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `denote-dired-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'denote-dired-mode-in-directories "denote-dired" "\
Enable `denote-dired-mode' in `denote-dired-directories'.
Add this function to `dired-mode-hook'." nil nil)

(register-definition-prefixes "denote-dired" '("denote-dired-"))

;;;***

;;;### (autoloads nil "denote-faces" "denote-faces.el" (0 0 0 0))
;;; Generated autoloads from denote-faces.el

(register-definition-prefixes "denote-faces" '("denote-faces-"))

;;;***

;;;### (autoloads nil "denote-link" "denote-link.el" (0 0 0 0))
;;; Generated autoloads from denote-link.el

(autoload 'denote-link "denote-link" "\
Create link to TARGET note in variable `denote-directory'.
With optional ID-ONLY, such as a universal prefix
argument (\\[universal-argument]), insert links with just the
identifier and no further description.  In this case, the link
format is always [[denote:IDENTIFIER]].

\(fn TARGET &optional ID-ONLY)" t nil)

(autoload 'denote-link-find-file "denote-link" "\
Use minibuffer completion to visit linked file." t nil)

(autoload 'denote-link-buttonize-buffer "denote-link" "\
Make denote: links actionable buttons in the current buffer.

Add this to `find-file-hook'.  It will only work with Denote
notes and will not do anything in `org-mode' buffers, as buttons
already work there.  If you do not use Markdown or plain text,
then you do not need this.

When called from Lisp, with optional BEG and END as buffer
positions, limit the process to the region in-between.

\(fn &optional BEG END)" nil nil)

(autoload 'denote-link-backlinks "denote-link" "\
Produce a buffer with files linking to current note.
Each file is a clickable/actionable button that visits the
referenced entry.  Files are fontified if the user option
`denote-link-fontify-backlinks' is non-nil.

The placement of the backlinks' buffer is controlled by the user
option `denote-link-backlinks-display-buffer-action'.  By
default, it will show up below the current window." t nil)

(autoload 'denote-link-add-links "denote-link" "\
Insert links to all notes matching REGEXP.
Use this command to reference multiple files at once.
Particularly useful for the creation of metanotes (read the
manual for more on the matter).

Optional ID-ONLY has the same meaning as in `denote-link': it
inserts links with just the identifier.

\(fn REGEXP &optional ID-ONLY)" t nil)

(register-definition-prefixes "denote-link" '("denote-link-"))

;;;***

;;;### (autoloads nil "denote-org-capture" "denote-org-capture.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from denote-org-capture.el

(autoload 'denote-org-capture "denote-org-capture" "\
Create new note through `org-capture-templates'.
Use this as a function that returns the path to the new file.
The file is populated with Denote's front matter.  It can then be
expanded with the usual specifiers or strings that
`org-capture-templates' supports.

Note that this function ignores the `denote-file-type': it always
sets the Org file extension for the created note to ensure that
the capture process works as intended, especially for the desired
output of the `denote-org-capture-specifiers'.

Consult the manual for template samples." nil nil)

(register-definition-prefixes "denote-org-capture" '("denote-org-capture-"))

;;;***

;;;### (autoloads nil "denote-retrieve" "denote-retrieve.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from denote-retrieve.el

(register-definition-prefixes "denote-retrieve" '("denote-retrieve--"))

;;;***

;;;### (autoloads nil nil ("denote-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; denote-autoloads.el ends here
