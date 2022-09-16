;; tree-inspector.el --- Inspector tool for Emacs Lisp object that uses a treeview  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/emacs-inspector
;; Keywords: debugging, tool, emacs-lisp, development
;; Version: 0.1
;; Package-Requires: ((emacs "25") (treeview "1.1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspector tool for Emacs Lisp object that uses a treeview.

;; Usage:
;; M-x `tree-inspector-inspect-last-sexp' to inspect the last sexp at point.

;;; Code:

(require 'eieio)
(require 'treeview)
(require 'mule-util)

;;---------- Settings --------------------------------------------------------

(defgroup tree-inspector nil
  "Customizations for tree-inspector."
  :group 'applications)

(defun tree-inspector--inspect-object-at-event (event)
  "Command to run as response for EVENT on tree-inspector object's label."
  (interactive "@e")
  (when (featurep 'inspector)
    (let ((node (treeview-get-node-at-event event)))
      (when-let ((object (treeview-get-node-prop node 'object)))
        (inspector-inspect object)))))

(defun tree-inspector--inspect-object-at-point ()
  "Command to run for inspecting the object at point in tree-inspector."
  (interactive)
  (when (featurep 'inspector)
    (let ((node (treeview-get-node-at-pos (point))))
      (when-let ((object (treeview-get-node-prop node 'object)))
        (inspector-inspect object)))))

(defcustom tree-inspector-control-keymap
  '(("<mouse-1>" . treeview-toggle-node-state-at-event)
    ("<mouse-2>" . treeview-toggle-node-state-at-event)
    ("RET" . treeview-toggle-node-state-at-point)
    ("SPC" . treeview-toggle-node-state-at-point))
  "Keymap of the control symbols.
A list of assignments of key sequences to commands.  Key sequences are strings
in a format understood by `kbd'.  Commands a names of Lisp functions."
  :group 'tree-inspector
  :type '(repeat (cons (string :tag "Key    ") (function :tag "Command"))))

(defcustom tree-inspector-label-keymap
  '(("<mouse-1>" . tree-inspector--inspect-object-at-event)
    ("<mouse-2>" . tree-inspector--inspect-object-at-event)
    ("RET" . tree-inspector--inspect-object-at-point)
    ("<C-down-mouse-1>" . ignore)
    ("<C-mouse-1>" . treeview-toggle-select-node-at-event)
    ("<S-down-mouse-1>" . ignore)
    ("<S-mouse-1>" . treeview-select-gap-above-node-at-event))
  "Keymap of the labels.
A list of assignments of key sequences to commands.  Key sequences are strings
in a format understood by `kbd'.  Commands a names of Lisp functions."
  :group 'tree-inspector
  :type '(repeat (cons (string :tag "Key    ") (function :tag "Command"))))

(defcustom tree-inspector-use-specialized-inspectors-for-lists t
  "Whether to use specialized inspectors for plists and alists."
  :type 'boolean
  :group 'inspector)

(defcustom tree-inspector-indent-unit " | "
  "Symbol to indent directories when the parent is not the last child."
  :group 'tree-inspector
  :type 'string)

(defcustom tree-inspector-indent-last-unit "   "
  "Symbol to indent directories when the parent is the last child of its parent."
  :group 'tree-inspector
  :type 'string)

(defcustom tree-inspector-folded-node-control "[+]"
  "Control symbol for folded directories."
  :group 'tree-inspector
  :type 'string)

(defcustom tree-inspector-expanded-node-control "[-]"
  "Control symbol for expanded directories."
  :group 'tree-inspector
  :type 'string)

(defcustom tree-inspector-print-object-truncated-max 30
  "Maximum length for objects printed representation in tree-inspector."
  :group 'tree-inspector
  :type 'number)

;;-------- Utils ----------------------------------------------------------

(defun tree-inspector--princ-to-string (object)
  "Print OBJECT to string using `princ'."
  (with-output-to-string
    (princ object)))

(defun tree-inspector--proper-list-p (val)
  "Is VAL a proper list?"
  (if (fboundp 'format-proper-list-p)
      ;; Emacs stable.
      (with-no-warnings (format-proper-list-p val))
    ;; Function was renamed in Emacs master:
    ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2fde6275b69fd113e78243790bf112bbdd2fe2bf
    (with-no-warnings (proper-list-p val))))

(defun tree-inspector--plistp (list)
  "Return T if LIST is a property list."
  (let ((expected t))
    (and (tree-inspector--proper-list-p list)
         (cl-evenp (length list))
         (cl-every (lambda (x)
                     (setq expected (if (eql expected t) 'symbol t))
                     (cl-typep x expected))
                   list))))

(defun tree-inspector--alistp (list)
  "Return T if LIST is an association list."
  (and (tree-inspector--proper-list-p list)
       (cl-every (lambda (x) (consp x)) list)))

(defun tree-inspector--alist-to-plist (alist)
  "Convert association list ALIST to a property list."
  (let ((plist))
    (dolist (cons (reverse alist))
      (push (cdr cons) plist)
      (push (car cons) plist))
    plist))

(defun tree-inspector--print-object (object)
  "Print OBJECT, truncated."
  (truncate-string-to-width
   (prin1-to-string object)
   tree-inspector-print-object-truncated-max
   nil nil "..."))

;;-------------- treeview functions --------------------------------------------

(defun tree-inspector--get-indent (node)
  "Return the indentation of NODE."
  (let ((indent ())
        (parent nil))
    (while (setq parent (treeview-get-node-parent node))
      (setq indent (cons tree-inspector-indent-unit indent)
            node parent))
    indent))

(defun tree-inspector--new-node (object)
  "Create a new tree-inspector node for OBJECT."
  (let ((node (treeview-new-node)))
    (treeview-set-node-prop node 'object object)
    node))

(defun tree-inspector--set-node-children (node children)
  "Set the CHILDREN of NODE.
Assigns NODE as parent to CHILDREN nodes."
  (mapc (lambda (child)
          (treeview-set-node-parent child node))
        children)
  (treeview-set-node-children node children))

(defun tree-inspector--update-node-children (node)
  "Update the children of NODE.
This calls `tree-inspector--set-node-children' generic function,
that can be specialized for different types of objects."
  (let ((object (treeview-get-node-prop node 'object)))
    (when object
      (let ((children (tree-inspector--node-children object)))
        (when children
          (tree-inspector--set-node-children node children))))))

(cl-defgeneric tree-inspector--make-node (object)
  "Create treeview node for Emacs Lisp OBJECT.
This is the main node creation function in tree-inspector.
Can be specialized for user's custom object types.")

(cl-defgeneric tree-inspector--node-children (object)
  "Return the OBJECT children treeview nodes.
This generic function should be specialized for different type of objects,
to specify their children in the tree-inspector.")

(cl-defmethod tree-inspector--node-children ((_object t))
  "Objects have no children by default."
  nil)

(cl-defmethod tree-inspector--make-node ((object t))
  "Create tree-inspector node for EIEIO instances, structures, records."
  (cond
   ((eieio-object-p object)
    (let ((node (tree-inspector--new-node object)))
      (treeview-set-node-name node (tree-inspector--print-object object))
      (tree-inspector--set-node-children
       node (mapcar (lambda (slot)
                      (let ((child-node (tree-inspector--make-node
                                         (slot-value object (cl--slot-descriptor-name slot)))))
                        (treeview-set-node-name
                         child-node (format "%s: %s" (cl--slot-descriptor-name slot) (treeview-get-node-name child-node)))
                        child-node))
                    (eieio-class-slots (eieio-object-class object))))
      node))
   ((cl-struct-p object)
    (let ((node (tree-inspector--new-node object)))
      (treeview-set-node-name node (tree-inspector--print-object object))
      (tree-inspector--set-node-children
       node (mapcar (lambda (slot)
                      (let ((child-node (tree-inspector--make-node
                                         (cl-struct-slot-value (type-of object) (car slot) object))))
                        (treeview-set-node-name
                         child-node (format "%s: %s" (car slot) (treeview-get-node-name child-node)))
                        child-node))
                    (cdr (cl-struct-slot-info (type-of object)))))
      node))
   ((recordp object)
    (let ((node (tree-inspector--new-node object)))
      (treeview-set-node-name node (tree-inspector--print-object object))
      (let (children)
        (cl-do ((i 1 (cl-incf i)))
            ((= i (length object)))
          (push (tree-inspector--make-node (aref object i)) children))
        (tree-inspector--set-node-children node children)
        node)))
   (t
    (error "Implement tree-inspector--make-node for %s" (type-of object)))))

(cl-defmethod tree-inspector--make-node ((object subr))
  "Create tree-inspector node for subr function OBJECT."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object (eql t)))
  "Create tree-inspector node for T."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object null))
  "Create tree-inspector node for nil object."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node "nil")
    node))

(cl-defmethod tree-inspector--make-node ((object number))
  "Create tree-inspector node for numbers."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object symbol))
  "Create tree-inspector node for symbols."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object string))
  "Create tree-inspector node for strings."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name
     node (tree-inspector--print-object object))
    node))

;;--------- cons -------------------------------------------

(cl-defmethod tree-inspector--make-node  ((object cons))
  "Create tree-inspector node for cons and lists."
  (cond
   ;; alists
   ((and tree-inspector-use-specialized-inspectors-for-lists
         (tree-inspector--alistp object))
    (let ((node (tree-inspector--new-node object)))
      (treeview-set-node-name node (tree-inspector--print-object object))
      node))
   ;; plists
   ((tree-inspector--plistp object)
    (let ((node (tree-inspector--new-node object)))
      (treeview-set-node-name
       node (tree-inspector--print-object object))
      node))
   ;; proper lists
   ((tree-inspector--proper-list-p object)
    (let ((node (tree-inspector--new-node object)))
      (treeview-set-node-name
       node (tree-inspector--print-object object))
      node))
   ;; a cons
   (t (let ((node (tree-inspector--new-node object)))
        (treeview-set-node-name
         node (format "(%s . %s)"
                      (tree-inspector--print-object (car object))
                      (tree-inspector--print-object (cdr object))))
        (treeview-set-node-children
         node (list (tree-inspector--make-node (car object))
                    (tree-inspector--make-node (cdr object))))
        node))))

(cl-defmethod tree-inspector--node-children ((object cons))
  "Child nodes of CONS objects."
  (cond
   ;; alists
   ((and tree-inspector-use-specialized-inspectors-for-lists
         (tree-inspector--alistp object))
    (mapcar (lambda (cons)
              (let ((child (tree-inspector--new-node cons)))
                (treeview-set-node-name
                 child (format "(%s . %s)"
                               (tree-inspector--print-object (car cons))
                               (tree-inspector--print-object (cdr cons))))
                (tree-inspector--set-node-children
                 child (list (tree-inspector--make-node (car cons))
                             (tree-inspector--make-node (cdr cons))))
                child))
            object))
   ;; plists
   ((and tree-inspector-use-specialized-inspectors-for-lists
         (tree-inspector--plistp object))
    (mapcar (lambda (cons)
              (let ((child (tree-inspector--new-node cons)))
                (treeview-set-node-name
                 child (format "%s %s"
                               (tree-inspector--print-object (car cons))
                               (tree-inspector--print-object (cdr cons))))
                (tree-inspector--set-node-children
                 child (list (tree-inspector--make-node (car cons))
                             (tree-inspector--make-node (cdr cons))))
                child))
            (cl--plist-to-alist object)))
   ;; proper lists
   ((tree-inspector--proper-list-p object)
    (mapcar #'tree-inspector--make-node object))
   ;; a cons
   (t (list (tree-inspector--make-node (car object))
            (tree-inspector--make-node (cdr object))))))

;;---- vector -----------------------------------------------

(cl-defmethod tree-inspector--make-node ((object bool-vector))
  "Create tree-inspector node for bool-vector."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name
     node (tree-inspector--print-object object))
    (treeview-set-node-children
     node
     (cl-map 'list
             (lambda (item)
               (let ((child (tree-inspector--make-node item)))
                 (treeview-set-node-parent child node)
                 child))
             object))
    node))

(cl-defmethod tree-inspector--make-node ((object vector))
  "Create tree-inspector node for vectors."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name
     node (tree-inspector--print-object object))
    node))

(cl-defmethod tree-inspector--node-children ((object vector))
  "Child nodes of vector objects."
  (cl-map 'list #'tree-inspector--make-node object))


;;---- hash-table ------------------------------------------

(cl-defmethod tree-inspector--make-node ((object hash-table))
  "Create tree-inspector node for hash-tables."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    (let (children)
      (dolist (key (hash-table-keys object))
        (let ((child (tree-inspector--new-node object))
              (value (gethash key object)))
          (treeview-set-node-name child (format "%s=%s" key value))
          (tree-inspector--set-node-children
           child (list (tree-inspector--make-node key)
                       (tree-inspector--make-node value)))
          (push child children)))
      (tree-inspector--set-node-children node children)
      node)))

;;----- buffers, windows, frames ----------------------------

(cl-defmethod tree-inspector--make-node ((object buffer))
  "Create tree-inspector for buffers."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--node-children ((object buffer))
  "Return tree-inspector child nodes of buffer objects."
  (list (tree-inspector--make-node (get-buffer-window object))
        (tree-inspector--make-node
         (format "cursor pos: %s" (with-current-buffer object
                                    (what-cursor-position))))))

(cl-defmethod tree-inspector--make-node ((object window))
  "Create tree-inspector node for window objects."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--node-children ((object window))
  "Return tree-inspector child nodes for window objects."
  (list (let ((parent (tree-inspector--make-node (window-parent object))))
          (treeview-set-node-name
           parent (format "parent: %s" (treeview-get-node-name parent)))
          parent)
        (tree-inspector--make-node (window-buffer object))
        (tree-inspector--make-node (window-frame object))
        (tree-inspector--make-node (window-parameters object))))

(cl-defmethod tree-inspector--make-node ((object marker))
  "Create tree-inspector node for markers."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--make-node ((object frame))
  "Create tree-inspector nodes for frames."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--node-children ((object frame))
  "Return tree-inspector child nodes for frame objects."
  (mapcar #'tree-inspector--make-node (frame-parameters object)))

(cl-defmethod tree-inspector--make-node ((object overlay))
  "Create tree-inspector node for overlays."
  (let ((node (tree-inspector--new-node object)))
    (treeview-set-node-name node (prin1-to-string object))
    node))

(cl-defmethod tree-inspector--node-children ((object overlay))
  "Return tree-inspector child nodes for overlay objects."
  (list (tree-inspector--make-node (overlay-buffer object))
        (tree-inspector--make-node (overlay-properties object))))

;;------ api ----------------------------------------------------

(defun tree-inspector-inspect (data)
  "Inspect DATA with a tree-inspector.

DATA can be any Emacs Lisp object."
  (let ((buffer (get-buffer-create
                 (format "*tree-inspector: %s*"
                         (tree-inspector--print-object data)))))
    (with-current-buffer buffer
      (setq-local treeview-get-indent-function (cl-constantly (list " ")))
      (setq-local treeview-get-label-function #'cl-first)
      (setq-local treeview-get-indent-function #'tree-inspector--get-indent)
      (setq-local treeview-get-control-function
                  (lambda (node)
                    (when (or (treeview-get-node-children node)
                              (when-let ((object (treeview-get-node-prop node 'object)))
                                (tree-inspector--node-children object)))
                      (if (treeview-node-folded-p node)
                          tree-inspector-folded-node-control
                        tree-inspector-expanded-node-control))))
      (setq-local treeview-update-node-children-function
                  #'tree-inspector--update-node-children)
      (setq-local treeview-after-node-expanded-function
                  (cl-constantly nil))
      (setq-local treeview-after-node-folded-function
                  (cl-constantly nil))
      (setq-local treeview-get-control-keymap-function
                  (cl-constantly
                   (treeview-make-keymap tree-inspector-control-keymap)))
      (setq-local treeview-get-label-keymap-function
                  (cl-constantly
                   (treeview-make-keymap tree-inspector-label-keymap)))
      (treeview-display-node (tree-inspector--make-node data))
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'kill-current-buffer)
      (switch-to-buffer buffer)
      buffer)))

;;;###autoload
(defun tree-inspector-inspect-last-sexp ()
  "Evaluate sexp before point and inspect the result."
  (interactive)
  (let ((result (eval (eval-sexp-add-defvars (elisp--preceding-sexp)) lexical-binding)))
    (tree-inspector-inspect result)))

(provide 'tree-inspector)

;;; tree-inspector.el ends here
