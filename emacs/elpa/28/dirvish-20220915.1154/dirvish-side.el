;;; dirvish-side.el --- Toggle Dirvish in side window like treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Toggle Dirvish in side window like treemacs.

;;; Code:

(require 'dirvish-subtree)

(defcustom dirvish-side-display-alist
  '((side . left) (slot . -1))
  "Display alist for `dirvish-side' window."
  :group 'dirvish :type 'alist)

(defcustom dirvish-side-width 35
  "Width of the `dirvish-side' buffer."
  :type 'integer :group 'dirvish)

(defcustom dirvish-side-window-parameters '((no-delete-other-windows . t))
  "Window parameters for `dirvish-side' window."
  :group 'dirvish :type 'alist)

(defcustom dirvish-side-open-file-window-function
  (lambda () (get-mru-window nil nil t))
  "A function that returns a window for the `find-file' buffer.
This function is called before opening files in a `dirvish-side'
session.  For example, if you have `ace-window' installed, you
can set it to `ace-select-window', which prompts you for a target
window to place the file buffer.  Note that if this value is
`selected-window', the session closes after opening a file."
  :group 'dirvish :type 'function)

(define-obsolete-variable-alias 'dirvish-side-follow-buffer-file 'dirvish-side-auto-expand "Sep 15, 2022")
(defcustom dirvish-side-auto-expand t
  "Whether to auto expand parent directories of current file.
If non-nil, expand all the parent directories of current buffer's
filename until the project root when opening a side session."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-side-follow-project-switch t
  "Whether visible side session update index on project switch.
If this variable is non-nil, the visible `dirvish-side' session
will visit the latest `project-root' after executing
`project-switch-project' or `projectile-switch-project'."
  :group 'dirvish :type 'boolean
  :set
  (lambda (key enabled)
    (set key enabled)
    (if enabled
        (progn
          (and (fboundp 'project-switch-project)
               (advice-add 'project-switch-project :after #'dirvish-side--auto-jump))
          (add-hook 'projectile-after-switch-project-hook #'dirvish-side--auto-jump))
      (and (fboundp 'project-switch-project)
           (advice-remove 'project-switch-project #'dirvish-side--auto-jump))
      (remove-hook 'projectile-after-switch-project-hook #'dirvish-side--auto-jump))))

(defconst dirvish-side-header (dirvish--mode-line-fmt-setter '(project) nil t))

(defun dirvish-side-on-file-open (dv)
  "Called before opening a file in Dirvish-side session DV."
  (unless (dv-layout dv)
    (select-window (funcall dirvish-side-open-file-window-function))))

(defun dirvish-side-root-window-fn ()
  "Create root window according to `dirvish-side-display-alist'."
  (let ((win (display-buffer-in-side-window
              (dirvish--util-buffer "temp") dirvish-side-display-alist)))
    (cl-loop for (key . value) in dirvish-side-window-parameters
             do (set-window-parameter win key value))
    (with-selected-window win
      (let ((w (max dirvish-side-width window-min-width)) window-size-fixed)
        (cond ((> (window-width) w)
               (shrink-window-horizontally  (- (window-width) w)))
              ((< (window-width) w)
               (enlarge-window-horizontally (- w (window-width)))))))
    (select-window win)))

(defun dirvish-side--session-visible-p ()
  "Return the root window of visible side session."
  (cl-loop
   for w in (window-list)
   for b = (window-buffer w)
   for dv = (with-current-buffer b (dirvish-curr))
   thereis (and dv (eq 'side (dv-type dv)) w)))

(defun dirvish-side--auto-jump (&optional dir)
  "Visit DIR in current visible `dirvish-side' session."
  (setq dir (dirvish--get-project-root dir))
  (when-let ((win (dirvish-side--session-visible-p))
             (file buffer-file-name))
    (with-selected-window win
      (when dir (dirvish-find-entry-a dir))
      (dirvish-prop :cus-header 'dirvish-side-header)
      (if dirvish-side-auto-expand (dirvish-subtree-expand-to file)
        (dired-goto-file file))
      (dirvish--setup-mode-line (dv-layout (dirvish-curr)))
      (dirvish-update-body-h))))

(defun dirvish-side--new (path)
  "Open a side session in PATH."
  (let* ((bname buffer-file-name)
         (dv (or (car (dirvish--find-reusable 'side))
                 (dirvish-new
                  :type 'side :on-file-open #'dirvish-side-on-file-open
                  :root-window-fn #'dirvish-side-root-window-fn)))
         (r-win (dv-root-window dv)))
    (unless (window-live-p r-win) (setq r-win (dirvish--create-root-window dv)))
    (with-selected-window r-win
      (dirvish-save-dedication (switch-to-buffer (cdr (dv-index dv))))
      (setq dirvish--this dv)
      (dirvish-find-entry-a (or path (dirvish-prop :root)))
      (cond ((not bname) nil)
            (dirvish-side-auto-expand
             (dirvish-subtree-expand-to bname))
            (t (dired-goto-file bname)))
      (dirvish-prop :cus-header 'dirvish-side-header)
      (dirvish-update-body-h))))

(dirvish-define-mode-line project
  "Return a string showing current project."
  (let ((project (dirvish--get-project-root))
        (face (if (dirvish--window-selected-p dv) 'dired-header 'shadow)))
    (if project
        (setq project (file-name-base (directory-file-name project)))
      (setq project "-"))
    (format " %s %s"
            (propertize "Project:" 'face face)
            (propertize project 'face 'font-lock-string-face))))

;;;###autoload
(defun dirvish-side (&optional path)
  "Toggle a Dirvish session at the side window.

- If the current window is a side session window, hide it.
- If a side session is visible, select it.
- If a side session exists but is not visible, show it.
- If there is no side session exists,create a new one with PATH.

If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `project-current'."
  (interactive (list (and current-prefix-arg
                          (read-directory-name "Open sidetree: "))))
  (let ((fullframep (when-let ((dv (dirvish-curr))) (dv-layout dv)))
        (visible (dirvish-side--session-visible-p))
        (path (or path (dirvish--get-project-root) default-directory)))
    (cond (fullframep (user-error "Can not create side session here"))
          ((eq visible (selected-window)) (dirvish-quit))
          (visible (select-window visible))
          (t (dirvish-side--new path)))))

(provide 'dirvish-side)
;;; dirvish-side.el ends here