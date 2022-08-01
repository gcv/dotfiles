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

- SUBDIRECTORY is a string representing the path to either the
  value of the variable `denote-directory' or a subdirectory
  thereof.  The subdirectory must exist: Denote will not create
  it.  If SUBDIRECTORY does not resolve to a valid path, the
  variable `denote-directory' is used instead.

- DATE is a string representing a date like 2022-06-30 or a date
  and time like 2022-06-16 14:30.  A nil value or an empty string
  is interpreted as the `current-time'.

\(fn &optional TITLE KEYWORDS FILE-TYPE SUBDIRECTORY DATE)" t nil)

(autoload 'denote-type "denote" "\
Create note while prompting for a file type.

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(file-type title keywords)." t nil)

(function-put 'denote-type 'interactive-only 't)

(autoload 'denote-date "denote" "\
Create note while prompting for a date.

The date can be in YEAR-MONTH-DAY notation like 2022-06-30 or
that plus the time: 2022-06-16 14:30

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(date title keywords)." t nil)

(function-put 'denote-date 'interactive-only 't)

(autoload 'denote-subdirectory "denote" "\
Create note while prompting for a subdirectory.

Available candidates include the value of the variable
`denote-directory' and any subdirectory thereof.

This is equivalent to calling `denote' when `denote-prompts' is set to
\\='(subdirectory title keywords)." t nil)

(function-put 'denote-subdirectory 'interactive-only 't)

(register-definition-prefixes "denote" '("denote-"))

;;;***

;;;### (autoloads nil "denote-dired" "denote-dired.el" (0 0 0 0))
;;; Generated autoloads from denote-dired.el

(autoload 'denote-dired-rename-file "denote-dired" "\
Rename file and update existing front matter if appropriate.

If in Dired, consider FILE to be the one at point, else prompt
with minibuffer completion for one.

If FILE has a Denote-compliant identifier, retain it while
updating the TITLE and KEYWORDS fields of the file name.  Else
create an identifier based on the file's attribute of last
modification time.  If such attribute cannot be found, the
identifier falls back to the `current-time'.

The default TITLE is retrieved from a line starting with a title
field in the file's contents, depending on the given file type.
Else, the file name is used as a default value at the minibuffer
prompt.

As a final step after the FILE, TITLE, and KEYWORDS prompts, ask
for confirmation, showing the difference between old and new file
names.  If `denote-dired-rename-expert' is non-nil, conduct the
renaming operation outright---no question asked!

The file type extension (e.g. .pdf) is read from the underlying
file and is preserved through the renaming process.  Files that
have no extension are simply left without one.

Renaming only occurs relative to the current directory.  Files
are not moved between directories.

If the FILE has Denote-style front matter for the TITLE and
KEYWORDS, ask to rewrite their values in order to reflect the new
input (this step always requires confirmation and the underlying
buffer is not saved, so consider invoking `diff-buffer-with-file'
to double-check the effect).  The rewrite of the FILE and
KEYWORDS in the front matter should not affect the rest of the
block.

If the file doesn't have front matter, skip this step (see the
command `denote-dired-rename-file-and-add-front-matter').

This command is intended to (i) rename existing Denote notes
while updating their title and keywords in the front matter, (ii)
rename files that can benefit from Denote's file-naming scheme.
The latter is a convenience we provide, since we already have all
the requisite mechanisms in place (though Denote does not---and
will not---manage such files).

\(fn FILE TITLE KEYWORDS)" t nil)

(autoload 'denote-dired-rename-file-and-add-front-matter "denote-dired" "\
Rename FILE and unconditionally add front matter.

This command has the same modalities of interaction as
`denote-dired-rename-file' in terms of the FILE, TITLE, and
KEYWORDS prompts, except it always inserts front matter at the
start of the file.  It does not check if any front matter is
already present.

Front matter is added only when the file is one of the supported
file types (per `denote-file-type').  For per-file-type front
matter, refer to the variables:

- `denote-org-front-matter'
- `denote-text-front-matter'
- `denote-toml-front-matter'
- `denote-yaml-front-matter'

\(fn FILE TITLE KEYWORDS)" t nil)

(autoload 'denote-dired-rename-marked-files "denote-dired" "\
Rename marked files in Dired to Denote file name.

The operation does the following:

- the file's existing file name is retained and becomes the TITLE
  field, per Denote's file-naming scheme;

- the TITLE is sluggified and downcased, per our conventions;

- an identifier is prepended to the TITLE;

- the file's extension is retained;

- a prompt is asked once for the KEYWORDS field and the input is
  applied to all file names;

- if the file is recognized as a Denote note, rewrite its front
  matter to include the new keywords.  A confirmation to carry
  out this step is performed once at the outset.  Note that the
  affected buffers are not saved.  The user can thus check them
  to confirm that the new front matter does not cause any
  problems (e.g. with the command `diff-buffer-with-file').
  Multiple buffers can be saved with `save-some-buffers' (read
  its doc string)." '(dired-mode) nil)

(autoload 'denote-dired-rename-marked-files-and-add-front-matters "denote-dired" "\
Like `denote-dired-rename-marked-files' but add front matter.

The additon of front matter takes place only if the given file
has the appropriate file type extension (per the user option
`denote-file-type').

Buffers are not saved.  The user can thus check them to confirm
that the new front matter does not cause any problems (e.g. by
invoking the command `diff-buffer-with-file').

Multiple buffers can be saved with `save-some-buffers' (read its
doc string)." '(dired-mode) nil)

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

\(fn &optional BEG END)" t nil)

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

(autoload 'denote-link-dired-marked-notes "denote-link" "\
Insert Dired marked FILES as links in BUFFER.

FILES are Denote notes, meaning that they have our file-naming
scheme, are writable/regular files, and use the appropriate file
type extension (per `denote-file-type').  Furthermore, the marked
files need to be inside the variable `denote-directory' or one of
its subdirectories.  No other file is recognised (the list of
marked files ignores whatever does not count as a note for our
purposes).

The BUFFER is one which visits a Denote note file.  If there are
multiple buffers, prompt with completion for one among them.  If
there isn't one, throw an error.

With optional ID-ONLY as a prefix argument, insert links with
just the identifier (same principle as with `denote-link').

This command is meant to be used from a Dired buffer.

\(fn FILES BUFFER &optional ID-ONLY)" '(dired-mode) nil)

(eval-after-load 'org `(funcall ',(lambda nil (with-no-warnings (org-link-set-parameters "denote" :follow #'denote-link-ol-follow :complete #'denote-link-ol-complete :export #'denote-link-ol-export)))))

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
output of the `denote-org-capture-specifiers' (which can include
arbitrary text).

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
