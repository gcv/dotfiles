;;; selectrum-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "selectrum" "selectrum.el" (0 0 0 0))
;;; Generated autoloads from selectrum.el

(defmacro selectrum--when-compile (cond &rest body) "\
Like `when', but COND is evaluated at compile time.
If it's nil, BODY is not even compiled." (declare (indent 1)) (when (eval cond) (\` (progn (\,@ body)))))

(autoload 'selectrum-completing-read "selectrum" "\
Read choice using Selectrum. Can be used as `completing-read-function'.
For PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, and INHERIT-INPUT-METHOD, see `completing-read'.

\(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

(autoload 'selectrum-completing-read-multiple "selectrum" "\
Read one or more choices using Selectrum.
Replaces `completing-read-multiple'. For PROMPT, TABLE,
PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD, see `completing-read-multiple'.

\(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

(autoload 'selectrum-completion-in-region "selectrum" "\
Complete in-buffer text using a list of candidates.
Can be used as `completion-in-region-function'. For START, END,
COLLECTION, and PREDICATE, see `completion-in-region'.

\(fn START END COLLECTION PREDICATE)" nil nil)

(autoload 'selectrum-read-buffer "selectrum" "\
Read buffer using Selectrum. Can be used as `read-buffer-function'.
Actually, as long as `selectrum-completing-read' is installed in
`completing-read-function', `read-buffer' already uses Selectrum.
Installing this function in `read-buffer-function' makes sure the
buffers are sorted in the default order (most to least recently
used) rather than in whatever order is defined by
`selectrum-preprocess-candidates-function', which is likely to be
less appropriate. It also allows you to view hidden buffers,
which is otherwise impossible due to tricky behavior of Emacs'
completion machinery. For PROMPT, DEF, REQUIRE-MATCH, and
PREDICATE, see `read-buffer'.

\(fn PROMPT &optional DEF REQUIRE-MATCH PREDICATE)" nil nil)

(autoload 'selectrum-read-file-name "selectrum" "\
Read file name using Selectrum. Can be used as `read-file-name-function'.
For PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL, and
PREDICATE, see `read-file-name'.

\(fn PROMPT &optional DIR DEFAULT-FILENAME MUSTMATCH INITIAL PREDICATE)" nil nil)

(autoload 'selectrum-read-directory-name "selectrum" "\
Read directory name using Selectrum.
Same as `read-directory-name' except it handles default
candidates a bit better (in particular you can immediately press
\\[selectrum-select-current-candidate] to use the current
directory). For PROMPT, DIR, DEFAULT-DIRNAME, MUSTMATCH, and
INITIAL, see `read-directory-name'.

\(fn PROMPT &optional DIR DEFAULT-DIRNAME MUSTMATCH INITIAL)" nil nil)

(autoload 'selectrum--fix-dired-read-dir-and-switches "selectrum" "\
Make \\[dired] do the \"right thing\" with its default candidate.
By default \\[dired] uses `read-file-name' internally, which
causes Selectrum to provide you with the first file inside the
working directory as the default candidate. However, it would
arguably be more semantically appropriate to use
`read-directory-name', and this is especially important for
Selectrum since this causes it to provide you with the working
directory itself as the default candidate.

To test that this advice is working correctly, type \\[dired] and
accept the default candidate. You should have opened the working
directory in Dired, and not a filtered listing for the current
file.

This is an `:around' advice for `dired-read-dir-and-switches'.
FUNC and ARGS are standard as in any `:around' advice.

\(fn FUNC &rest ARGS)" nil nil)

(autoload 'selectrum-read-library-name "selectrum" "\
Read and return a library name.
Similar to `read-library-name' except it handles `load-path'
shadows correctly.

\(fn)" nil nil)

(autoload 'selectrum--fix-set-minibuffer-message "selectrum" "\
Move the minibuffer message overlay to the right place.
This advice fixes the overlay placed by `set-minibuffer-message',
which is different from the one placed by `minibuffer-message'.

By default the overlay is placed at the end, but in the case of
Selectrum this means after all the candidates. We want to move it
instead to just after the user input.

To test that this advice is working correctly, type \\[find-file]
and enter \"/sudo::\", then authenticate. The overlay indicating
that authentication was successful should appear right after the
user input area, not at the end of the candidate list.

This is an `:after' advice for `set-minibuffer-message'.

\(fn &rest _)" nil nil)

(autoload 'selectrum--fix-minibuffer-message "selectrum" "\
Move the minibuffer message overlay to the right place.
This advice fixes the overlay placed by `minibuffer-message',
which is different from the one placed by
`set-minibuffer-message'.

By default the overlay is placed at the end, but in the case of
Selectrum this means after all the candidates. We want to move it
instead to just after the user input.

To test that this advice is working correctly, type \\[find-file]
twice in a row. The overlay indicating that recursive minibuffers
are not allowed should appear right after the user input area,
not at the end of the candidate list.

This is an `:around' advice for `minibuffer-message'. FUNC and
ARGS are standard as in all `:around' advice.

\(fn FUNC &rest ARGS)" nil nil)

(define-minor-mode selectrum-mode "\
Minor mode to use Selectrum for `completing-read'." :global t (if selectrum-mode (progn (selectrum-mode -1) (setq selectrum-mode t) (setq selectrum--old-completing-read-function (default-value (quote completing-read-function))) (setq-default completing-read-function (function selectrum-completing-read)) (setq selectrum--old-read-buffer-function (default-value (quote read-buffer-function))) (setq-default read-buffer-function (function selectrum-read-buffer)) (setq selectrum--old-read-file-name-function (default-value (quote read-file-name-function))) (setq-default read-file-name-function (function selectrum-read-file-name)) (setq selectrum--old-completion-in-region-function (default-value (quote completion-in-region-function))) (setq-default completion-in-region-function (function selectrum-completion-in-region)) (advice-add (function completing-read-multiple) :override (function selectrum-completing-read-multiple)) (advice-add (function read-directory-name) :override (function selectrum-read-directory-name)) (advice-add (quote dired-read-dir-and-switches) :around (function selectrum--fix-dired-read-dir-and-switches)) (advice-add (quote read-library-name) :override (function selectrum-read-library-name)) (advice-add (function minibuffer-message) :around (function selectrum--fix-minibuffer-message)) (advice-add (quote set-minibuffer-message) :after (function selectrum--fix-set-minibuffer-message)) (define-key minibuffer-local-map [remap previous-matching-history-element] (quote selectrum-select-from-history))) (when (equal (default-value (quote completing-read-function)) (function selectrum-completing-read)) (setq-default completing-read-function selectrum--old-completing-read-function)) (when (equal (default-value (quote read-buffer-function)) (function selectrum-read-buffer)) (setq-default read-buffer-function selectrum--old-read-buffer-function)) (when (equal (default-value (quote read-file-name-function)) (function selectrum-read-file-name)) (setq-default read-file-name-function selectrum--old-read-file-name-function)) (when (equal (default-value (quote completion-in-region-function)) (function selectrum-completion-in-region)) (setq-default completion-in-region-function selectrum--old-completion-in-region-function)) (advice-remove (function completing-read-multiple) (function selectrum-completing-read-multiple)) (advice-remove (function read-directory-name) (function selectrum-read-directory-name)) (advice-remove (quote dired-read-dir-and-switches) (function selectrum--fix-dired-read-dir-and-switches)) (advice-remove (quote read-library-name) (function selectrum-read-library-name)) (advice-remove (function minibuffer-message) (function selectrum--fix-minibuffer-message)) (advice-remove (quote set-minibuffer-message) (function selectrum--fix-set-minibuffer-message)) (when (eq (lookup-key minibuffer-local-map [remap previous-matching-history-element]) (function selectrum-select-from-history)) (define-key minibuffer-local-map [remap previous-matching-history-element] nil))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "selectrum" '("selectrum-")))

;;;***

;;;### (autoloads nil "selectrum-helm" "selectrum-helm.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from selectrum-helm.el

(defvar selectrum-helm-mode nil "\
Non-nil if Selectrum-Helm mode is enabled.
See the `selectrum-helm-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `selectrum-helm-mode'.")

(custom-autoload 'selectrum-helm-mode "selectrum-helm" nil)

(autoload 'selectrum-helm-mode "selectrum-helm" "\
Minor mode to use Selectrum to implement Helm commands.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "selectrum-helm" '("selectrum-helm--adapter")))

;;;***

;;;### (autoloads nil nil ("selectrum-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; selectrum-autoloads.el ends here
