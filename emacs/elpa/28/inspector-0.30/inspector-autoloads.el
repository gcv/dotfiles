;;; inspector-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "inspector" "inspector.el" (0 0 0 0))
;;; Generated autoloads from inspector.el

(autoload 'inspector-inspect-expression "inspector" "\
Evaluate EXP and inspect its result.

\(fn EXP)" t nil)

(autoload 'inspector-inspect "inspector" "\
Top-level function for inspecting OBJECTs.
When PRESERVE-HISTORY is T, inspector history is not cleared.

\(fn OBJECT &optional PRESERVE-HISTORY)" nil nil)

(autoload 'inspector-inspect-last-sexp "inspector" "\
Evaluate sexp before point and inspect the result." t nil)

(autoload 'inspector-inspect-defun "inspector" "\
Evaluate the top s-exp - simmilar the effect
 of M-x or eval-defun and inspect the result" t nil)

(autoload 'inspector-inspect-region "inspector" "\
Evaluate the region from START TO END and inspect the result.

\(fn START END)" t nil)

(autoload 'inspector-pprint-inspected-object "inspector" "\
Pretty print the object being inspected." t nil)

(autoload 'inspector-inspect-debugger-locals "inspector" "\
Inspect local variables of the frame at point in debugger backtrace." t nil)

(autoload 'inspector-inspect-debugger-local "inspector" "\
Inspect local variable named VARNAME of frame at point in debugger backtrace.

\(fn VARNAME)" t nil)

(autoload 'inspector-inspect-debugger-return-value "inspector" "\
Inspect the current return value in the debugger." t nil)

(autoload 'inspector-inspect-stack-frame "inspector" "\
Inspect current frame and locals in debugger backtrace." t nil)

(autoload 'inspector-inspect-in-stack-frame "inspector" "\
Inspect an expression, in an environment like that outside the debugger.
The environment used is the one when entering the activation frame at point.

\(fn EXP)" t nil)

(autoload 'inspector-inspect-edebug-expression "inspector" "\
Evaluate EXPR in `edebug-mode', and inspect the result.

\(fn EXPR)" t nil)

(register-definition-prefixes "inspector" '("inspector-"))

;;;***

;;;### (autoloads nil "inspector-tests" "inspector-tests.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from inspector-tests.el

(register-definition-prefixes "inspector-tests" '("inspector-tests-"))

;;;***

;;;### (autoloads nil "tree-inspector" "tree-inspector.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from tree-inspector.el

(autoload 'tree-inspector-inspect-last-sexp "tree-inspector" "\
Evaluate sexp before point and inspect the result." t nil)

(autoload 'tree-inspector-inspect-defun "tree-inspector" "\
Inspect the top-level defun." t nil)

(autoload 'tree-inspector-inspect-region "tree-inspector" "\
Inspect the region.

\(fn START END)" t nil)

(autoload 'tree-inspector-inspect-expression "tree-inspector" "\
Evaluate EXP and inspect its result with a tree-inspector.

\(fn EXP)" t nil)

(register-definition-prefixes "tree-inspector" '("tree-inspector-"))

;;;***

;;;### (autoloads nil "tree-inspector-tests" "tree-inspector-tests.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tree-inspector-tests.el

(register-definition-prefixes "tree-inspector-tests" '("inspector-tests--person" "tree-inspector-tests-"))

;;;***

;;;### (autoloads nil nil ("inspector-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; inspector-autoloads.el ends here
