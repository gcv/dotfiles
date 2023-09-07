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

- TEMPLATE is a symbol which represents the key of a cons cell in
  the user option `denote-templates'.  The value of that key is
  inserted to the newly created buffer after the front matter.

- SIGNATURE is a string or a function returning a string.

\(fn &optional TITLE KEYWORDS FILE-TYPE SUBDIRECTORY DATE TEMPLATE SIGNATURE)" t nil)

(autoload 'denote-type "denote" "\
Create note while prompting for a file type.

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(file-type title keywords)." t nil)

(function-put 'denote-type 'interactive-only 't)

(autoload 'denote-date "denote" "\
Create note while prompting for a date.

The date can be in YEAR-MONTH-DAY notation like 2022-06-30 or
that plus the time: 2022-06-16 14:30.  When the user option
`denote-date-prompt-use-org-read-date' is non-nil, the date
prompt uses the more powerful Org+calendar system.

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

(autoload 'denote-signature "denote" "\
Create note while prompting for a file signature.

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(signature title keywords)." t nil)

(function-put 'denote-signature 'interactive-only 't)

(autoload 'denote-open-or-create "denote" "\
Visit TARGET file in variable `denote-directory'.
If file does not exist, invoke `denote' to create a file.

If TARGET file does not exist, add the user input that was used
to search for it to the minibuffer history of the
`denote-file-prompt'.  The user can then retrieve and possibly
further edit their last input, using it as the newly created
note's actual title.  At the `denote-file-prompt' type
\\<minibuffer-local-map>\\[previous-history-element].

\(fn TARGET)" t nil)

(autoload 'denote-keywords-add "denote" "\
Prompt for KEYWORDS to add to the current note's front matter.
When called from Lisp, KEYWORDS is a list of strings.

Rename the file without further prompt so that its name reflects
the new front matter, per `denote-rename-file-using-front-matter'.

\(fn KEYWORDS)" t nil)

(autoload 'denote-keywords-remove "denote" "\
Prompt for keywords in current note and remove them.
Keywords are retrieved from the file's front matter.

Rename the file without further prompt so that its name reflects
the new front matter, per `denote-rename-file-using-front-matter'." t nil)

(function-put 'denote-keywords-remove 'interactive-only 't)

(autoload 'denote-rename-file "denote" "\
Rename file and update existing front matter if appropriate.

If in Dired, consider FILE to be the one at point, else prompt
with minibuffer completion for one.

If FILE has a Denote-compliant identifier, retain it while
updating the TITLE and KEYWORDS fields of the file name.  Else
create an identifier based on the following conditions:

- If FILE does not have an identifier and optional DATE is
  non-nil (such as with a prefix argument), invoke the function
  `denote-prompt-for-date-return-id'.  It prompts for a date and
  uses it to derive the identifier.

- If FILE does not have an identifier and optional DATE is
  nil (this is the case without a prefix argument), use the file
  attributes to determine the last modified date and format it as
  an identifier.

- As a fallback, derive an identifier from the current time.

- If the resulting identifier is not unique among the files in
  the variable `denote-directory', increment it such that it
  becomes unique.

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

\(fn FILE TITLE KEYWORDS &optional DATE)" t nil)

(autoload 'denote-change-file-type "denote" "\
Change file type of FILE and add an appropriate front matter.

If in Dired, consider FILE to be the one at point, else prompt
with minibuffer completion for one.

Add a front matter in the format of the NEW-FILE-TYPE at the
beginning of the file.

Retrieve the title of FILE from a line starting with a title
field in its front matter, depending on the previous file
type (e.g.  #+title for Org).  The same process applies for
keywords.

As a final step, ask for confirmation, showing the difference
between old and new file names.

Important note: No attempt is made to modify any other elements
of the file.  This needs to be done manually.

\(fn FILE NEW-FILE-TYPE)" t nil)

(autoload 'denote-dired-rename-marked-files "denote" "\
Rename marked files in Dired to a Denote file name.

Specifically, do the following:

- retain the file's existing name and make it the TITLE field,
  per Denote's file-naming scheme;

- downcase and sluggify the TITLE, per our conventions;

- prepend an identifier to the TITLE;

- preserve the file's extension, if any;

- prompt once for KEYWORDS and apply the user's input to the
  corresponding field in the file name;

- add or rewrite existing front matter to the underlying file, if
  it is recognized as a Denote note (per `denote-file-type'),
  such that it includes the new keywords;

- prompt at the outset for a confirmation, unless optional
  SKIP-FRONT-MATTER-PROMPT is non-nil (such as with a universal
  prefix argument).

  [ Note that the affected buffers are not saved.  Users can thus
    check them to confirm that the new front matter does not
    cause any problems (e.g. with the `diff-buffer-with-file'
    command).  Multiple buffers can be saved in one go with
    `save-some-buffers' (read its doc string). ]

With the optional NO-UNIQUE-ID-CHECK as non-nil (such as as a
double prefix argument), do not process the file identifiers of
the marked files for potential duplicates.  The default is to
check for duplicates and increment them such that they become
unique.  The reason this optional argument exists is for those
who want to speed up the process, perhaps because they know ahead
of time all identifiers will be unique or do not care about them.

\[ When renaming files in Dired, it is possible to produce
  duplicate identifiers.  This can happen when multiple files
  share the same modification time, which can be casually done
  with the `touch' command, `git', and others. ]

\(fn &optional SKIP-FRONT-MATTER-PROMPT NO-UNIQUE-ID-CHECK)" '(dired-mode) nil)

(autoload 'denote-rename-file-using-front-matter "denote" "\
Rename FILE using its front matter as input.
When called interactively, FILE is the return value of the
function `buffer-file-name' which is subsequently inspected for
the requisite front matter.  It is thus implied that the FILE has
a file type that is supported by Denote, per `denote-file-type'.

Unless AUTO-CONFIRM is non-nil (such as with a prefix argument),
ask for confirmation, showing the difference between the old and
the new file names.

Never modify the identifier of the FILE, if any, even if it is
edited in the front matter.  Denote considers the file name to be
the source of truth in this case to avoid potential breakage with
typos and the like.

Refrain from performing the operation if the buffer has unsaved
changes.  Inform the user about the need to save their changes
first.  If AUTO-CONFIRM is non-nil, then save the buffer and
proceed with the renaming.

\(fn FILE &optional AUTO-CONFIRM)" t nil)

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
their respective front matter." '(dired-mode) nil)

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

(autoload 'denote-dired-mode-in-directories "denote" "\
Enable `denote-dired-mode' in `denote-dired-directories'.
Add this function to `dired-mode-hook'." nil nil)

(autoload 'denote-link "denote" "\
Create link to TARGET note in variable `denote-directory'.
With optional ID-ONLY, such as a universal prefix
argument (\\[universal-argument]), insert links with just the
identifier and no further description.  In this case, the link
format is always [[denote:IDENTIFIER]].

Use TARGET's title for the link's description.  The title comes
either from the front matter or the file name.

If region is active, use its text as the link's description
instead of TARGET's title.  If active region is empty (i.e
whitespace-only), insert an ID-ONLY link.

\(fn TARGET &optional ID-ONLY)" t nil)

(autoload 'denote-find-link "denote" "\
Use minibuffer completion to visit linked file." t nil)

(autoload 'denote-find-backlink "denote" "\
Use minibuffer completion to visit backlink to current file.

Like `denote-find-link', but select backlink to follow." t nil)

(autoload 'denote-link-after-creating "denote" "\
Create new note in the background and link to it directly.

Use `denote' interactively to produce the new note.  Its doc
string explains which prompts will be used and under what
conditions.

With optional ID-ONLY as a prefix argument create a link that
consists of just the identifier.  Else try to also include the
file's title.  This has the same meaning as in `denote-link'.

IMPORTANT NOTE: Normally, `denote' does not save the buffer it
produces for the new note.  This is a safety precaution to not
write to disk unless the user wants it (e.g. the user may choose
to kill the buffer, thus cancelling the creation of the note).
However, for this command the creation of the note happens in the
background and the user may miss the step of saving their buffer.
We thus have to save the buffer in order to (i) establish valid
links, and (ii) retrieve whatever front matter from the target
file.

\(fn &optional ID-ONLY)" t nil)

(autoload 'denote-link-or-create "denote" "\
Use `denote-link' on TARGET file, creating it if necessary.

If TARGET file does not exist, call `denote-link-after-creating'
which runs the `denote' command interactively to create the file.
The established link will then be targeting that new file.

If TARGET file does not exist, add the user input that was used
to search for it to the minibuffer history of the
`denote-file-prompt'.  The user can then retrieve and possibly
further edit their last input, using it as the newly created
note's actual title.  At the `denote-file-prompt' type
\\<minibuffer-local-map>\\[previous-history-element].

With optional ID-ONLY as a prefix argument create a link that
consists of just the identifier.  Else try to also include the
file's title.  This has the same meaning as in `denote-link'.

\(fn TARGET &optional ID-ONLY)" t nil)

(autoload 'denote-link-buttonize-buffer "denote" "\
Make denote: links actionable buttons in the current buffer.

Buttonization applies to the plain text and Markdown file types,
per the user option `denote-file-types'.  It will not do anything
in `org-mode' buffers, as buttons already work there.  If you do
not use Markdown or plain text, then you do not need this.

Links work when they point to a file inside the variable
`denote-directory'.

To buttonize links automatically add this function to the
`find-file-hook'.  Or call it interactively for on-demand
buttonization.

When called from Lisp, with optional BEG and END as buffer
positions, limit the process to the region in-between.

\(fn &optional BEG END)" t nil)

(autoload 'denote-backlinks "denote" "\
Produce a buffer with backlinks to the current note.

The backlinks' buffer shows the file name of the note linking to
the current note, as well as the context of each link.

File names are fontified by Denote if the user option
`denote-link-fontify-backlinks' is non-nil.  If this user option
is nil, the buffer is fontified by Xref.

The placement of the backlinks' buffer is controlled by the user
option `denote-link-backlinks-display-buffer-action'.  By
default, it will show up below the current window." t nil)

(autoload 'denote-add-links "denote" "\
Insert links to all notes matching REGEXP.
Use this command to reference multiple files at once.
Particularly useful for the creation of metanotes (read the
manual for more on the matter).

Optional ID-ONLY has the same meaning as in `denote-link': it
inserts links with just the identifier.

\(fn REGEXP &optional ID-ONLY)" t nil)

(autoload 'denote-add-missing-links "denote" "\
Insert missing links to all notes matching REGEXP.
Similar to `denote-add-links' but insert only links not yet
present in the current buffer.

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

\(fn FILES BUFFER &optional ID-ONLY)" '(dired-mode) nil)

(autoload 'denote-link-ol-follow "denote" "\
Find file of type `denote:' matching LINK.
LINK is the identifier of the note, optionally followed by a
search option akin to that of standard Org `file:' link types.
Read Info node `(org) Search Options'.

Uses the function `denote-directory' to establish the path to the
file.

\(fn LINK)" nil nil)

(autoload 'denote-link-ol-complete "denote" "\
Like `denote-link' but for Org integration.
This lets the user complete a link through the `org-insert-link'
interface by first selecting the `denote:' hyperlink type." nil nil)

(autoload 'denote-link-ol-store "denote" "\
Handler for `org-store-link' adding support for denote: links." nil nil)

(autoload 'denote-link-ol-export "denote" "\
Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend.

\(fn LINK DESCRIPTION FORMAT)" nil nil)

(eval-after-load 'org `(funcall ',(lambda nil (with-no-warnings (org-link-set-parameters "denote" :follow #'denote-link-ol-follow :face 'denote-faces-link :complete #'denote-link-ol-complete :store #'denote-link-ol-store :export #'denote-link-ol-export)))))

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

(autoload 'denote-org-capture-with-prompts "denote" "\
Like `denote-org-capture' but with optional prompt parameters.

When called without arguments, do not prompt for anything.  Just
return the front matter with title and keyword fields empty and
the date and identifier fields specified.  Also make the file
name consist of only the identifier plus the Org file name
extension.

Otherwise produce a minibuffer prompt for every non-nil value
that corresponds to the TITLE, KEYWORDS, SUBDIRECTORY, DATE, and
TEMPLATE arguments.  The prompts are those used by the standard
`denote' command and all of its utility commands.

When returning the contents that fill in the Org capture
template, the sequence is as follows: front matter, TEMPLATE, and
then the value of the user option `denote-org-capture-specifiers'.

Important note: in the case of SUBDIRECTORY actual subdirectories
must exist---Denote does not create them.  Same principle for
TEMPLATE as templates must exist and are specified in the user
option `denote-templates'.

\(fn &optional TITLE KEYWORDS SUBDIRECTORY DATE TEMPLATE)" nil nil)

(autoload 'denote-modules-mode "denote" "\
Enable Denote integration modules locally.
Set modules to be enabled in `denote-modules' and activate the
minor mode, either globally or locally.  The selected modules are
enabled only when the minor mode is active.

This is a minor mode.  If called interactively, toggle the
`Denote-Modules mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `denote-modules-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defvar denote-modules-global-mode nil "\
Non-nil if Denote-Modules-Global mode is enabled.
See the `denote-modules-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `denote-modules-global-mode'.")

(custom-autoload 'denote-modules-global-mode "denote" nil)

(autoload 'denote-modules-global-mode "denote" "\
Enable Denote integration modules globally.
Set modules to be enabled in `denote-modules' and activate the
minor mode, either globally or locally.  The selected modules are
enabled only when the minor mode is active.

This is a minor mode.  If called interactively, toggle the
`Denote-Modules-Global mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='denote-modules-global-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "denote" '("denote-"))

;;;***

;;;### (autoloads nil "denote-org-dblock" "denote-org-dblock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from denote-org-dblock.el

(autoload 'denote-org-dblock-insert-links "denote-org-dblock" "\
Create Org dynamic block to insert Denote links matching REGEXP.

\(fn REGEXP)" t nil)

(autoload 'denote-org-dblock-insert-backlinks "denote-org-dblock" "\
Insert new Org dynamic block to include backlinks." t nil)

(register-definition-prefixes "denote-org-dblock" '("org-dblock-write:denote-"))

;;;***

;;;### (autoloads nil "denote-rename-buffer" "denote-rename-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from denote-rename-buffer.el

(defvar denote-rename-buffer-mode nil "\
Non-nil if Denote-Rename-Buffer mode is enabled.
See the `denote-rename-buffer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `denote-rename-buffer-mode'.")

(custom-autoload 'denote-rename-buffer-mode "denote-rename-buffer" nil)

(autoload 'denote-rename-buffer-mode "denote-rename-buffer" "\
Automatically rename Denote buffers to be easier to read.
A buffer is renamed upon visiting the underlying file.  This
means that existing buffers are not renamed until they are
visited again in a new buffer (files are visited with the command
`find-file' or related).

This is a minor mode.  If called interactively, toggle the
`Denote-Rename-Buffer mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='denote-rename-buffer-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "denote-rename-buffer" '("denote-rename-buffer-"))

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