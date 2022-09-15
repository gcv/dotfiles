;;; denote-autoloads.el --- automatically extracted autoloads
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

- TEMPLATE is a symbol which represents the key of a cons cell in
  the user option `denote-templates'.  The value of that key is
  inserted to the newly created buffer after the front matter.

\(fn &optional TITLE KEYWORDS FILE-TYPE SUBDIRECTORY DATE TEMPLATE)" t nil)

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

This is equivalent to calling `denote' when `denote-prompts' is
set to \\='(subdirectory title keywords)." t nil)

(function-put 'denote-subdirectory 'interactive-only 't)

(autoload 'denote-template "denote" "\
Create note while prompting for a template.

Available candidates include the keys in the `denote-templates'
alist.  The value of the selected key is inserted in the newly
created note after the front matter.

This is equivalent to calling `denote' when `denote-prompts' is
set to \\='(template title keywords)." t nil)

(function-put 'denote-template 'interactive-only 't)

(autoload 'denote-rename-file "denote" "\
Rename file and update existing front matter if appropriate.

If in Dired, consider FILE to be the one at point, else prompt
with minibuffer completion for one.

If FILE has a Denote-compliant identifier, retain it while
updating the TITLE and KEYWORDS fields of the file name.  Else
create an identifier based on the file's attribute of last
modification time.  If such attribute cannot be found, the
identifier falls back to the `current-time'.

The default TITLE is retrieved from a line starting with a title
field in the file's contents, depending on the given file
type (e.g. #+title for Org).  Else, the file name is used as a
default value at the minibuffer prompt.

As a final step after the FILE, TITLE, and KEYWORDS prompts, ask
for confirmation, showing the difference between old and new file
names.

The file type extension (like .txt) is read from the underlying
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

If the file doesn't have front matter but is among the supported
file types (per `denote-file-type'), add front matter at the top
of it and leave the buffer unsaved for further inspection.

For per-file-type front matter, refer to the variables:

- `denote-org-front-matter'
- `denote-text-front-matter'
- `denote-toml-front-matter'
- `denote-yaml-front-matter'

This command is intended to (i) rename existing Denote notes
while updating their title and keywords in the front matter, (ii)
convert existing supported file types to Denote notes, and (ii)
rename non-note files (e.g. PDF) that can benefit from Denote's
file-naming scheme.  The latter is a convenience we provide,
since we already have all the requisite mechanisms in
place (though Denote does not---and will not---manage such
files).

\(fn FILE TITLE KEYWORDS)" t nil)

(autoload 'denote-dired-rename-marked-files "denote" "\
Rename marked files in Dired to Denote file name.

The operation does the following:

- the file's existing file name is retained and becomes the TITLE
  field, per Denote's file-naming scheme;

- the TITLE is sluggified and downcased, per our conventions;

- an identifier is prepended to the TITLE;

- the file's extension is retained;

- a prompt is asked once for the KEYWORDS field and the input is
  applied to all file names;

- if the file is recognized as a Denote note, add a front matter
  or rewrite it to include the new keywords.  A confirmation to
  carry out this step is performed once at the outset.  Note that
  the affected buffers are not saved.  The user can thus check
  them to confirm that the new front matter does not cause any
  problems (e.g. with the command `diff-buffer-with-file').
  Multiple buffers can be saved with `save-some-buffers' (read
  its doc string).  The addition of front matter takes place only
  if the given file has the appropriate file type extension (per
  the user option `denote-file-type')." t nil)

(autoload 'denote-rename-file-using-front-matter "denote" "\
Rename FILE using its front matter as input.
When called interactively, FILE is the return value of the
function `buffer-file-name' which is subsequently inspected for
the requisite front matter.  It is thus implied that the FILE has
a file type that is supported by Denote, per `denote-file-type'.

Ask for confirmation, showing the difference between the old and
the new file names.  Refrain from performing the operation if the
buffer has unsaved changes.

Never modify the identifier of the FILE, if any, even if it is
edited in the front matter.  Denote considers the file name to be
the source of truth in this case to avoid potential breakage with
typos and the like.

\(fn FILE)" t nil)

(autoload 'denote-dired-rename-marked-files-using-front-matter "denote" "\
Rename marked files in Dired using their front matter as input.
Marked files must count as notes for the purposes of Denote,
which means that they at least have an identifier in their file
name and use a supported file type, per `denote-file-type'.
Files that do not meet this criterion are ignored.

The operation does the following:

- the title in the front matter becomes the TITLE component of
  the file name, with hyphenation per Denote's file-naming
  scheme;

- the keywords in the front matter are used for the KEYWORDS
  component of the file name and are processed accordingly, if
  needed;

- the identifier remains unchanged in the file name even if it is
  modified in the front matter (this is done to avoid breakage
  caused by typos and the like).

NOTE that files must be saved, because Denote reads from the
underlying file, not a modified buffer (this is done to avoid
potential mistakes).  The return value of a modified buffer is
the one prior to the modification, i.e. the one already written
on disk.

This command is useful for synchronizing multiple file names with
their respective front matter." t nil)

(autoload 'denote-add-front-matter "denote" "\
Insert front matter at the top of FILE.

When called interactively, FILE is the return value of the
function `buffer-file-name'.  FILE is checked to determine
whether it is a note for Denote's purposes.

TITLE is a string.  Interactively, it is the user input at the
minibuffer prompt.

KEYWORDS is a list of strings.  Interactively, it is the user
input at the minibuffer prompt.  This one supports completion for
multiple entries, each separated by the `crm-separator' (normally
a comma).

The purpose of this command is to help the user generate new
front matter for an existing note (perhaps because the user
deleted the previous one and could not undo the change).

This command does not rename the file (e.g. to update the
keywords).  To rename a file by reading its front matter as
input, use `denote-rename-file-using-front-matter'.

Note that this command is useful only for existing Denote notes.
If the user needs to convert a generic text file to a Denote
note, they can use one of the command which first rename the file
to make it comply with our file-naming scheme and then add the
relevant front matter.

\(fn FILE TITLE KEYWORDS)" t nil)

(autoload 'denote-dired-mode "denote" "\
Fontify all Denote-style file names.
Add this or `denote-dired-mode-in-directories' to
`dired-mode-hook'.

If called interactively, enable Denote-Dired mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'denote-dired-mode-in-directories "denote" "\
Enable `denote-dired-mode' in `denote-dired-directories'.
Add this function to `dired-mode-hook'." nil nil)

(autoload 'denote-link "denote" "\
Create link to TARGET note in variable `denote-directory'.
With optional ID-ONLY, such as a universal prefix
argument (\\[universal-argument]), insert links with just the
identifier and no further description.  In this case, the link
format is always [[denote:IDENTIFIER]].

\(fn TARGET &optional ID-ONLY)" t nil)

(autoload 'denote-link-find-file "denote" "\
Use minibuffer completion to visit linked file." t nil)

(autoload 'denote-link-buttonize-buffer "denote" "\
Make denote: links actionable buttons in the current buffer.

Add this to `find-file-hook'.  It will only work with Denote
notes and will not do anything in `org-mode' buffers, as buttons
already work there.  If you do not use Markdown or plain text,
then you do not need this.

When called from Lisp, with optional BEG and END as buffer
positions, limit the process to the region in-between.

\(fn &optional BEG END)" t nil)

(autoload 'denote-link-backlinks "denote" "\
Produce a buffer with files linking to current note.
Each file is a clickable/actionable button that visits the
referenced entry.  Files are fontified if the user option
`denote-link-fontify-backlinks' is non-nil.

The placement of the backlinks' buffer is controlled by the user
option `denote-link-backlinks-display-buffer-action'.  By
default, it will show up below the current window." t nil)

(autoload 'denote-link-add-links "denote" "\
Insert links to all notes matching REGEXP.
Use this command to reference multiple files at once.
Particularly useful for the creation of metanotes (read the
manual for more on the matter).

Optional ID-ONLY has the same meaning as in `denote-link': it
inserts links with just the identifier.

\(fn REGEXP &optional ID-ONLY)" t nil)

(autoload 'denote-link-dired-marked-notes "denote" "\
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

\(fn FILES BUFFER &optional ID-ONLY)" t nil)

(eval-after-load 'org `(funcall ',(lambda nil (with-no-warnings (org-link-set-parameters "denote" :follow #'denote-link-ol-follow :face #'denote-link-ol-face :complete #'denote-link-ol-complete :export #'denote-link-ol-export)))))

(autoload 'denote-org-capture "denote" "\
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

(autoload 'denote-migrate-old-org-filetags "denote" "\
Rewrite Org filetags' value as colon-separated.

Change the filetags from:

    #+filetags:   one  two

To the standard format of:

    #+filetags:  :one:two:

A single tags chnages from TAG to :TAG:.

Denote used to format filetags with two spaces between them, but
this is not fully supported by Org.  The colon-separated entries
are the rule.

The rewrite DOES NOT SAVE BUFFERS.  The user is expected to
review the changes, such as by using `diff-buffer-with-file'.
Multiple buffers can be saved with `save-some-buffers' (check its
doc string).

This command is provided for the convenience of the user.  It
shall be deprecated and eventually removed from future versions
of Denote.  Written on 2022-08-10 for version 0.5.0." t nil)

(autoload 'denote-migrate-old-markdown-yaml-tags "denote" "\
Rewrite Markdown YAML tags value as comma-separated strings.

Change the tags from:

    tags:   one  two

To the standard format of:

    tags:  [\"one\", \"two\"]

Denote used to format filetags with two spaces between them, but
this is not supported by YAML.

The rewrite DOES NOT SAVE BUFFERS.  The user is expected to
review the changes, such as by using `diff-buffer-with-file'.
Multiple buffers can be saved with `save-some-buffers' (check its
doc string).

This command is provided for the convenience of the user.  It
shall be deprecated and eventually removed from future versions
of Denote.  Written on 2022-08-10 for version 0.5.0." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "denote" '("denote-")))

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
