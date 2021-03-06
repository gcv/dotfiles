;;; multiple-cursors-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "mc-edit-lines" "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-edit-lines.el"
;;;;;;  "5d934922b9f9a3a5965a92fd7f58e807")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-edit-lines.el

(autoload 'mc/edit-lines "mc-edit-lines" "\
Add one cursor to each line of the active region.
Starts from mark and moves in straight down or up towards the
line point is on.

What is done with lines which are not long enough is governed by
`mc/edit-lines-empty-lines'.  The prefix argument ARG can be used
to override this.  If ARG is a symbol (when called from Lisp),
that symbol is used instead of `mc/edit-lines-empty-lines'.
Otherwise, if ARG negative, short lines will be ignored.  Any
other non-nil value will cause short lines to be padded.

\(fn &optional ARG)" t nil)

(autoload 'mc/edit-ends-of-lines "mc-edit-lines" "\
Add one cursor to the end of each line in the active region.

\(fn)" t nil)

(autoload 'mc/edit-beginnings-of-lines "mc-edit-lines" "\
Add one cursor to the beginning of each line in the active region.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "mc-hide-unmatched-lines-mode" "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-hide-unmatched-lines-mode.el"
;;;;;;  "95db90b7f4d131eb3a7ce3c722f6ddd9")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-hide-unmatched-lines-mode.el

(autoload 'mc-hide-unmatched-lines-mode "mc-hide-unmatched-lines-mode" "\
Minor mode when enabled hides all lines where no cursors (and
also hum/lines-to-expand below and above) To make use of this
mode press \"C-'\" while multiple-cursor-mode is active. You can
still edit lines while you are in mc-hide-unmatched-lines
mode. To leave this mode press <return> or \"C-g\"

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "mc-mark-more" "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-mark-more.el"
;;;;;;  "55dcf56112d4bb399f423693bc035935")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-mark-more.el

(autoload 'mc/mark-next-like-this "mc-mark-more" "\
Find and mark the next part of the buffer matching the currently active region
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

\(fn ARG)" t nil)

(autoload 'mc/mark-next-like-this-word "mc-mark-more" "\
Find and mark the next part of the buffer matching the currently active region
If no region is active, mark the word at the point and find the next match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

\(fn ARG)" t nil)

(autoload 'mc/mark-next-word-like-this "mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/mark-next-symbol-like-this "mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/mark-previous-like-this "mc-mark-more" "\
Find and mark the previous part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

\(fn ARG)" t nil)

(autoload 'mc/mark-previous-word-like-this "mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/mark-previous-symbol-like-this "mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/mark-next-lines "mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/mark-previous-lines "mc-mark-more" "\


\(fn ARG)" t nil)

(autoload 'mc/unmark-next-like-this "mc-mark-more" "\
Deselect next part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/unmark-previous-like-this "mc-mark-more" "\
Deselect prev part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/skip-to-next-like-this "mc-mark-more" "\
Skip the current one and select the next part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/skip-to-previous-like-this "mc-mark-more" "\
Skip the current one and select the prev part of the buffer matching the currently active region.

\(fn)" t nil)

(autoload 'mc/mark-all-like-this "mc-mark-more" "\
Find and mark all the parts of the buffer matching the currently active region

\(fn)" t nil)

(autoload 'mc/mark-all-words-like-this "mc-mark-more" "\


\(fn)" t nil)

(autoload 'mc/mark-all-symbols-like-this "mc-mark-more" "\


\(fn)" t nil)

(autoload 'mc/mark-all-in-region "mc-mark-more" "\
Find and mark all the parts in the region matching the given search

\(fn BEG END &optional SEARCH)" t nil)

(autoload 'mc/mark-all-in-region-regexp "mc-mark-more" "\
Find and mark all the parts in the region matching the given regexp.

\(fn BEG END)" t nil)

(autoload 'mc/mark-more-like-this-extended "mc-mark-more" "\
Like mark-more-like-this, but then lets you adjust with arrows key.
The adjustments work like this:

   <up>    Mark previous like this and set direction to 'up
   <down>  Mark next like this and set direction to 'down

If direction is 'up:

   <left>  Skip past the cursor furthest up
   <right> Remove the cursor furthest up

If direction is 'down:

   <left>  Remove the cursor furthest down
   <right> Skip past the cursor furthest down

The bindings for these commands can be changed. See `mc/mark-more-like-this-extended-keymap'.

\(fn)" t nil)

(autoload 'mc/mark-all-like-this-dwim "mc-mark-more" "\
Tries to guess what you want to mark all of.
Can be pressed multiple times to increase selection.

With prefix, it behaves the same as original `mc/mark-all-like-this'

\(fn ARG)" t nil)

(autoload 'mc/mark-all-dwim "mc-mark-more" "\
Tries even harder to guess what you want to mark all of.

If the region is active and spans multiple lines, it will behave
as if `mc/mark-all-in-region'. With the prefix ARG, it will call
`mc/edit-lines' instead.

If the region is inactive or on a single line, it will behave like
`mc/mark-all-like-this-dwim'.

\(fn ARG)" t nil)

(autoload 'mc/mark-all-like-this-in-defun "mc-mark-more" "\
Mark all like this in defun.

\(fn)" t nil)

(autoload 'mc/mark-all-words-like-this-in-defun "mc-mark-more" "\
Mark all words like this in defun.

\(fn)" t nil)

(autoload 'mc/mark-all-symbols-like-this-in-defun "mc-mark-more" "\
Mark all symbols like this in defun.

\(fn)" t nil)

(autoload 'mc/toggle-cursor-on-click "mc-mark-more" "\
Add a cursor where you click, or remove a fake cursor that is
already there.

\(fn EVENT)" t nil)

(defalias 'mc/add-cursor-on-click 'mc/toggle-cursor-on-click)

(autoload 'mc/mark-sgml-tag-pair "mc-mark-more" "\
Mark the tag we're in and its pair for renaming.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "mc-mark-pop" "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-mark-pop.el"
;;;;;;  "d6ca90158891ee1716fda33be322cacd")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-mark-pop.el

(autoload 'mc/mark-pop "mc-mark-pop" "\
Add a cursor at the current point, pop off mark ring and jump
to the popped mark.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "mc-separate-operations" "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-separate-operations.el"
;;;;;;  "90f2001c1083b7dba9ff23187e6a3a8e")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-separate-operations.el

(autoload 'mc/insert-numbers "mc-separate-operations" "\
Insert increasing numbers for each cursor, starting at 0 or ARG.

\(fn ARG)" t nil)

(autoload 'mc/insert-letters "mc-separate-operations" "\
Insert increasing letters for each cursor, starting at 0 or ARG.
     Where letter[0]=a letter[2]=c letter[26]=aa

\(fn ARG)" t nil)

(autoload 'mc/reverse-regions "mc-separate-operations" "\


\(fn)" t nil)

(autoload 'mc/sort-regions "mc-separate-operations" "\


\(fn)" t nil)

(autoload 'mc/vertical-align "mc-separate-operations" "\
Aligns all cursors vertically with a given CHARACTER to the one with the
highest colum number (the rightest).
Might not behave as intended if more than one cursors are on the same line.

\(fn CHARACTER)" t nil)

(autoload 'mc/vertical-align-with-space "mc-separate-operations" "\
Aligns all cursors with whitespace like `mc/vertical-align' does

\(fn)" t nil)

;;;***

;;;### (autoloads nil "multiple-cursors-core" "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/multiple-cursors-core.el"
;;;;;;  "ae73724c9a1c578a1504efe1a960dd9f")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/multiple-cursors-core.el

(autoload 'multiple-cursors-mode "multiple-cursors-core" "\
Mode while multiple cursors are active.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "rectangular-region-mode" "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/rectangular-region-mode.el"
;;;;;;  "282f7e241597c641ddb18f9086ee65c5")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/rectangular-region-mode.el

(autoload 'set-rectangular-region-anchor "rectangular-region-mode" "\
Anchors the rectangular region at point.

Think of this one as `set-mark' except you're marking a rectangular region. It is
an exceedingly quick way of adding multiple cursors to multiple lines.

\(fn)" t nil)

(autoload 'rectangular-region-mode "rectangular-region-mode" "\
A mode for creating a rectangular region to edit

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-cycle-cursors.el"
;;;;;;  "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-edit-lines.el"
;;;;;;  "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-hide-unmatched-lines-mode.el"
;;;;;;  "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-mark-more.el"
;;;;;;  "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-mark-pop.el"
;;;;;;  "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/mc-separate-operations.el"
;;;;;;  "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/multiple-cursors-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/multiple-cursors-core.el"
;;;;;;  "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/multiple-cursors-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/multiple-cursors.el"
;;;;;;  "../../../../../.emacs.d/elpa/multiple-cursors-1.4.0/rectangular-region-mode.el")
;;;;;;  (23508 57671 313586 587000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; multiple-cursors-autoloads.el ends here
