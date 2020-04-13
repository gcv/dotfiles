;;; ctrlf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ctrlf" "ctrlf.el" (0 0 0 0))
;;; Generated autoloads from ctrlf.el

(defvar ctrlf-mode-bindings '(([remap isearch-forward] . ctrlf-forward-literal) ([remap isearch-backward] . ctrlf-backward-literal) ([remap isearch-forward-regexp] . ctrlf-forward-regexp) ([remap isearch-backward-regexp] . ctrlf-backward-regexp)) "\
Keybindings enabled in `ctrlf-mode'. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
`ctrlf-mode' is (re-)enabled. The keys are strings or raw key
events and the values are command symbols.

These bindings are available globally in Emacs. See also
`ctrlf-minibuffer-bindings', which defines bindings that are
active in the minibuffer during a search.")

(custom-autoload 'ctrlf-mode-bindings "ctrlf" nil)

(autoload 'ctrlf-forward-literal "ctrlf" "\
Search forward for literal string.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-literal search,
change back to literal search if prefix ARG is provided.

\(fn &optional ARG)" t nil)

(autoload 'ctrlf-backward-literal "ctrlf" "\
Search backward for literal string.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-literal
search, change back to literal search if prefix ARG is provided.

\(fn &optional ARG)" t nil)

(autoload 'ctrlf-forward-regexp "ctrlf" "\
Search forward for regexp.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-regexp search,
change back to regexp search.

\(fn)" t nil)

(autoload 'ctrlf-backward-regexp "ctrlf" "\
Search backward for regexp.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-regexp
search, change back to regexp search.

\(fn)" t nil)

(defvar ctrlf--keymap (make-sparse-keymap) "\
Keymap for `ctrlf-mode'. Populated when mode is enabled.
See `ctrlf-mode-bindings'.")

(define-minor-mode ctrlf-mode "\
Minor mode to use CTRLF in place of Isearch.
See `ctrlf-mode-bindings' to customize." :global t :keymap ctrlf--keymap (when ctrlf-mode (ctrlf-mode -1) (setq ctrlf-mode t) (setcdr ctrlf--keymap nil) (map-apply (lambda (key cmd) (when (stringp key) (setq key (kbd key))) (define-key ctrlf--keymap key cmd)) ctrlf-mode-bindings)) (with-eval-after-load (quote ctrlf) (if ctrlf-mode (advice-add (function minibuffer-message) :around (function ctrlf--minibuffer-message-condense)) (advice-remove (function minibuffer-message) (function ctrlf--minibuffer-message-condense)))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ctrlf" '("ctrlf-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ctrlf-autoloads.el ends here
