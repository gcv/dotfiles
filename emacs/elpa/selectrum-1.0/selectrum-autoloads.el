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
Minor mode to use Selectrum for `completing-read'." :global t (if selectrum-mode (progn (selectrum-mode -1) (setq selectrum-mode t) (setq selectrum--old-completing-read-function (default-value (quote completing-read-function))) (setq-default completing-read-function (function selectrum-completing-read)) (setq selectrum--old-read-buffer-function (default-value (quote read-buffer-function))) (setq-default read-buffer-function (function selectrum-read-buffer)) (setq selectrum--old-read-file-name-function (default-value (quote read-file-name-function))) (setq-default read-file-name-function (function selectrum-read-file-name)) (advice-add (function read-directory-name) :override (function selectrum-read-directory-name)) (with-eval-after-load (quote dired) (eval-and-compile (require (quote dired))) (advice-add (function dired-read-dir-and-switches) :around (function selectrum--fix-dired-read-dir-and-switches))) (selectrum--when-compile (fboundp (quote read-library-name)) (advice-add (function read-library-name) :override (function selectrum-read-library-name))) (advice-add (function minibuffer-message) :around (function selectrum--fix-minibuffer-message)) (selectrum--when-compile (fboundp (quote set-minibuffer-message)) (advice-add (function set-minibuffer-message) :after (function selectrum--fix-set-minibuffer-message)))) (when (equal (default-value (quote completing-read-function)) (function selectrum-completing-read)) (setq-default completing-read-function selectrum--old-completing-read-function)) (when (equal (default-value (quote read-buffer-function)) (function selectrum-read-buffer)) (setq-default read-buffer-function selectrum--old-read-buffer-function)) (when (equal (default-value (quote read-file-name-function)) (function selectrum-read-file-name)) (setq-default read-file-name-function selectrum--old-read-file-name-function)) (advice-remove (function read-directory-name) (function selectrum-read-directory-name)) (with-eval-after-load (quote dired) (eval-and-compile (require (quote dired))) (advice-remove (function dired-read-dir-and-switches) (function selectrum--fix-dired-read-dir-and-switches))) (selectrum--when-compile (fboundp (quote read-library-name)) (advice-remove (function read-library-name) (function selectrum-read-library-name))) (advice-remove (function minibuffer-message) (function selectrum--fix-minibuffer-message)) (selectrum--when-compile (fboundp (quote set-minibuffer-message)) (advice-remove (function set-minibuffer-message) (function selectrum--fix-set-minibuffer-message)))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "selectrum" '("selectrum-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; selectrum-autoloads.el ends here
