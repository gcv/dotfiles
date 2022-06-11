;;; code-cells-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "code-cells" "code-cells.el" (0 0 0 0))
;;; Generated autoloads from code-cells.el

(autoload 'code-cells-forward-cell "code-cells" "\
Move to the next cell boundary, or end of buffer.
With ARG, repeat this that many times.  If ARG is negative, move
backward.

\(fn &optional ARG)" t nil)

(autoload 'code-cells-backward-cell "code-cells" "\
Move to the previous cell boundary, or beginning of buffer.
With ARG, repeat this that many times.  If ARG is negative, move
forward.

\(fn &optional ARG)" t nil)

(autoload 'code-cells-do "code-cells" "\
Find current cell bounds and evaluate BODY.
Inside BODY, the variables `start' and `end' are bound to the
limits of the current cell.

If the first element of BODY is the keyword `:use-region' and the
region is active, use its bounds instead.  In this case,
`using-region' is non-nil in BODY.

\(fn &rest BODY)" nil t)

(autoload 'code-cells-move-cell-up "code-cells" "\
Move current code cell vertically up ARG cells.

\(fn &optional ARG)" t nil)

(autoload 'code-cells-mark-cell "code-cells" "\
Put point at the beginning of this cell, mark at end.

\(fn &optional ARG)" t nil)

(autoload 'code-cells-comment-or-uncomment "code-cells" "\
Comment or uncomment the current code cell.

ARG, if provided, is the number of comment characters to add or
remove.

\(fn &optional ARG)" t nil)

(autoload 'code-cells-command "code-cells" "\
Return an anonymous command that calls FUN on the current cell.

FUN is a function that takes two character positions as argument.
Most interactive commands that act on a region are of this form
and can be used here.

If OPTIONS contains the keyword :use-region, the command will act
on the region instead of the current cell when appropriate.

If OPTIONS contains the keyword :pulse, provide visual feedback
via `pulse-momentary-highlight-region'.

\(fn FUN &rest OPTIONS)" nil nil)

(autoload 'code-cells-speed-key "code-cells" "\
Return a speed key definition, suitable for passing to `define-key'.
The resulting keybinding will only have any effect when the point
is at the beginning of a cell heading, in which case it executes
COMMAND.

\(fn COMMAND)" nil nil)

(autoload 'code-cells-eval "code-cells" "\
Evaluate code according to current modes.
The first suitable function from `code-cells-eval-region-commands'
is used to do the job.

Interactively, evaluate the region, if active, otherwise the
current code cell.  With a numeric prefix, evaluate that many
code cells.

Called from Lisp, evaluate region between START and END.

\(fn START END)" t nil)

(autoload 'code-cells-eval-above "code-cells" "\
Evaluate this and all above cells.

\(fn ARG)" t nil)

(autoload 'code-cells-mode "code-cells" "\
Minor mode for cell-oriented code.

This is a minor mode.  If called interactively, toggle the
`Code-Cells mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `code-cells-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'code-cells-mode-maybe "code-cells" "\
Turn on `code-cells-mode' if the buffer appears to contain cells.
This function is useful when added to a major mode hook." nil nil)

(autoload 'code-cells-convert-ipynb "code-cells" "\
Convert buffer from ipynb format to a regular script." t nil)

(autoload 'code-cells-write-ipynb "code-cells" "\
Convert buffer to ipynb format and write to FILE.
Interactively, asks for the file name.  When called from Lisp,
FILE defaults to the current buffer file name.

\(fn &optional FILE)" t nil)

(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . code-cells-convert-ipynb))

(register-definition-prefixes "code-cells" '("code-cells-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; code-cells-autoloads.el ends here
