;;; code-cells-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from code-cells.el

(autoload 'code-cells-forward-cell "code-cells" "\
Move to the next cell boundary, or end of buffer.
With ARG, repeat this that many times.  If ARG is negative, move
backward.

(fn &optional ARG)" t)
(autoload 'code-cells-backward-cell "code-cells" "\
Move to the previous cell boundary, or beginning of buffer.
With ARG, repeat this that many times.  If ARG is negative, move
forward.

(fn &optional ARG)" t)
(autoload 'code-cells-command "code-cells" "\
Return an anonymous command calling FUN on the current cell.

FUN must be a function that takes two character positions as argument.
Most interactive commands that act on a region are of this form and
can be used here.

If USE-REGION is non-nil, the command will act on the region instead of
the current cell when the region is active.

If PULSE is non-nil, provide visual feedback via
`pulse-momentary-highlight-region'.

If NO-HEADER is non-nil, exclude the cell header from the acted region.

(fn FUN &key USE-REGION PULSE NO-HEADER)")
(autoload 'code-cells-speed-key "code-cells" "\
Return a speed key definition, suitable for passing to `define-key'.
The resulting keybinding will only have any effect when the point
is at the beginning of a cell heading, in which case it executes
COMMAND.

(fn COMMAND)")
(autoload 'code-cells-move-cell-down "code-cells" "\
Move current code cell vertically ARG cells.
Move up when ARG is negative and move down otherwise.

(fn ARG)" t)
(autoload 'code-cells-move-cell-up "code-cells" "\
Move current code cell vertically up ARG cells.

(fn &optional ARG)" t)
(autoload 'code-cells-mark-cell "code-cells" "\
Put point at the beginning of this cell, mark at end.
If ARG is non-nil, mark that many cells.

(fn &optional ARG)" t)
(autoload 'code-cells-comment-or-uncomment "code-cells" nil t)
(autoload 'code-cells-indent "code-cells" nil t)
(autoload 'code-cells-delete "code-cells" nil t)
(autoload 'code-cells-kill "code-cells" nil t)
(autoload 'code-cells-copy "code-cells" nil t)
(autoload 'code-cells-duplicate "code-cells" "\
Duplicate the current code cell.
With a prefix argument, act on that many cells.

(fn &optional ARG)" t)
(autoload 'code-cells-eval "code-cells" "\
Evaluate code according to current modes.
The first suitable function from `code-cells-eval-region-commands'
is used to do the job.

Interactively, evaluate the region, if active, otherwise the
current code cell.  With a numeric prefix, evaluate that many
code cells.

Called from Lisp, evaluate region between START and END.

(fn START END)" t)
(autoload 'code-cells-eval-and-step "code-cells" "\
Evaluate the current cell and move to the next one.
With a prefix argument ARG, act on that many cells.

(fn ARG)" t)
(autoload 'code-cells-eval-above "code-cells" "\
Evaluate all cells above the current one.
With a prefix argument, exclude that many extra cells.

From Lisp, just evaluate from beginning of buffer to POINT.

(fn POINT)" t)
(autoload 'code-cells-eval-below "code-cells" "\
Evaluate the current cell and all below.
With a prefix argument, include that many extra cells.

From Lisp, just evaluate from POINT to end of buffer.

(fn POINT)" t)
(autoload 'code-cells-eval-whole-buffer "code-cells" "\
Evaluate the entire buffer." t)
(autoload 'code-cells-mode "code-cells" "\
Minor mode for cell-oriented code.

This is a minor mode.  If called interactively, toggle the `Code-Cells
mode' mode.  If the prefix argument is positive, enable the mode, and if
it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `code-cells-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(autoload 'code-cells-mode-maybe "code-cells" "\
Turn on `code-cells-mode' if the buffer appears to contain cells.
This function is useful when added to a major mode hook.")
(autoload 'code-cells-convert-ipynb "code-cells" "\
Convert buffer from ipynb format to a regular script.")
(autoload 'code-cells-write-ipynb "code-cells" "\
Convert buffer to ipynb format and write to FILE.
Interactively, asks for the file name.  When called from Lisp,
FILE defaults to the current buffer file name.

(fn &optional FILE)" t)
(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . code-cells-convert-ipynb))
(register-definition-prefixes "code-cells" '("code-cells-"))

;;; End of scraped data

(provide 'code-cells-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; code-cells-autoloads.el ends here
