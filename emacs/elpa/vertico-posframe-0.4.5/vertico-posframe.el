;;; vertico-posframe.el --- Using posframe to show Vertico  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/vertico-posframe
;; Version: 0.4.5
;; Keywords: abbrev, convenience, matching, vertico
;; Package-Requires: ((emacs "26.0") (posframe "1.0.0") (vertico "0.13.0"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; * vertico-posframe README                                :README:

;; ** What is vertico-posframe

;; vertico-posframe is an vertico extension, which lets vertico use posframe to show
;; its candidate menu.

;; NOTE: vertico-posframe requires Emacs 26 and do not support mouse
;; click.

;; ** How to enable vertico-posframe
;; #+BEGIN_EXAMPLE
;; (require 'vertico-posframe)
;; (vertico-posframe-mode 1)
;; #+END_EXAMPLE

;; ** Tips

;; *** How to show fringe to vertico-posframe
;; #+BEGIN_EXAMPLE
;; (setq vertico-posframe-parameters
;;       '((left-fringe . 8)
;;         (right-fringe . 8)))
;; #+END_EXAMPLE

;; By the way, User can set *any* parameters of vertico-posframe with
;; the help of `vertico-posframe-parameters'.

;;; Code:
;; * vertico-posframe's code
(require 'posframe)
(require 'vertico)

(defgroup vertico-posframe nil
  "Using posframe to show vertico."
  :group 'vertico-posframe)

(defcustom vertico-posframe-font nil
  "The font used by vertico-posframe.
When nil, Using current frame's font as fallback."
  :type 'string)

(defcustom vertico-posframe-width nil
  "The width of vertico-posframe."
  :type 'number)

(defcustom vertico-posframe-height nil
  "The height of vertico-posframe."
  :type 'number)

(defcustom vertico-posframe-min-width nil
  "The min width of vertico-posframe."
  :type 'number)

(defcustom vertico-posframe-min-height nil
  "The min height of vertico-posframe."
  :type 'number)

(defcustom vertico-posframe-poshandler #'posframe-poshandler-frame-center
  "The posframe poshandler used by vertico-posframe."
  :type 'function)

(defcustom vertico-posframe-refposhandler #'vertico-posframe-refposhandler-default
  "The refposhandler used by vertico-posframe.

NOTE: This variable is very useful to EXWM users."
  :type 'function)

(defcustom vertico-posframe-size-function #'vertico-posframe-get-size
  "The function which is used to deal with posframe's size."
  :type 'function)

(defcustom vertico-posframe-border-width 2
  "The border width used by vertico-posframe.
When 0, no border is showed."
  :type 'number)

(defcustom vertico-posframe-parameters nil
  "The frame parameters used by vertico-posframe."
  :type 'string)

(defcustom vertico-posframe-show-minibuffer-rules
  (list "^eval-*")
  "A list of rule showed minibuffer.

a rule can be a regexp or a function.

1. when rule is a regexp and it match `this-command'.
2. when rule is a function and it return t.
3. when rule is a symbol, its value is t.

minibuffer will not be hided by minibuffer-cover."
  :type '(repeat (choice string function)))

(defface vertico-posframe
  '((t (:inherit default)))
  "Face used by the vertico-posframe."
  :group 'vertico-posframe)

(defface vertico-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face used by the vertico-posframe's border."
  :group 'vertico-posframe)

(defface vertico-posframe-cursor
  '((t (:inherit cursor)))
  "Face used by the vertico-posframe's fake cursor."
  :group 'vertico-posframe)

(defvar vertico-posframe--buffer " *vertico-posframe--buffer*")
(defvar vertico-posframe--minibuffer-cover " *vertico-posframe--minibuffer-cover*")

;; Fix warn
(defvar exwm--connection)
(defvar exwm-workspace--workareas)
(defvar exwm-workspace-current-index)

(defun vertico-posframe-refposhandler-default (&optional frame)
  "The default posframe refposhandler used by vertico-posframe.
Optional argument FRAME ."
  (cond
   ;; EXWM environment
   ((bound-and-true-p exwm--connection)
    (or (ignore-errors
          (let ((info (elt exwm-workspace--workareas
                           exwm-workspace-current-index)))
            (cons (elt info 0)
                  (elt info 1))))
        ;; Need user install xwininfo.
        (ignore-errors
          (posframe-refposhandler-xwininfo frame))
        ;; Fallback, this value will incorrect sometime, for example: user
        ;; have panel.
        (cons 0 0)))
   (t nil)))

(defun vertico-posframe-hidehandler (_)
  "Hidehandler used by vertico-posframe."
  (not (minibufferp)))

(defun vertico-posframe-get-size ()
  "The default functon used by `vertico-posframe-size-function'."
  (list
   :height vertico-posframe-height
   :width vertico-posframe-width
   :min-height (or vertico-posframe-min-height
                   (let ((height (+ vertico-count 1)))
                     (min height (or vertico-posframe-height height))))
   :min-width (or vertico-posframe-min-width
                  (let ((width (round (* (frame-width) 0.62))))
                    (min width (or vertico-posframe-width width))))))

(defun vertico-posframe--display (lines)
  "Display LINES in posframe."
  (let* ((show-minibuffer-p (vertico-posframe--show-minibuffer-p))
         (count (vertico-posframe--format-count))
         (prompt (propertize (minibuffer-prompt) 'face 'minibuffer-prompt))
         ;; NOTE: Vertico count in minibuffer is before-string of an
         ;; overlay, so minibuffer contents will not include it.
         (contents (minibuffer-contents))
         (n (+ (length count)
               (max (point) (+ (length prompt) 1))))
         ;; FIXME: make sure background and foreground do
         ;; not have similar color. ivy-posframe have not
         ;; this problem, I can not find the reason.
         (cursor-face
          (list :foreground (face-attribute 'default :background)
                :inherit 'vertico-posframe-cursor)))
    (with-current-buffer (get-buffer-create vertico-posframe--buffer)
      (setq-local inhibit-read-only nil
                  inhibit-modification-hooks t
                  cursor-in-non-selected-windows 'box)
      (erase-buffer)
      (insert count prompt contents "\n" (string-join lines))
      (add-text-properties n (+ n 1) `(face ,cursor-face)))
    (with-selected-window (vertico-posframe-last-window)
      ;; Create a posframe to cover minibuffer.
      (if show-minibuffer-p
          (vertico-posframe--hide-minibuffer-cover)
        (vertico-posframe--create-minibuffer-cover))
      (vertico-posframe--show))))

(defun vertico-posframe--format-count ()
  "Format vertico count."
  (propertize (or (vertico--format-count) "") 'face 'minibuffer-prompt))

(defun vertico-posframe--show (&optional string)
  "`posframe-show' of vertico-posframe.
Show STRING when it is a string."
  (apply #'posframe-show
         vertico-posframe--buffer
         :string string
         :font vertico-posframe-font
         :poshandler vertico-posframe-poshandler
         :background-color (face-attribute 'vertico-posframe :background nil t)
         :foreground-color (face-attribute 'vertico-posframe :foreground nil t)
         :border-width vertico-posframe-border-width
         :border-color (face-attribute 'vertico-posframe-border :background nil t)
         :override-parameters vertico-posframe-parameters
         :refposhandler vertico-posframe-refposhandler
         :hidehandler #'vertico-posframe-hidehandler
         :lines-truncate t
         (funcall vertico-posframe-size-function)))

(defun vertico-posframe--create-minibuffer-cover (&optional string)
  "Create minibuffer cover."
  (let ((color (face-background 'default nil))
        (win (active-minibuffer-window)))
    (posframe-show vertico-posframe--minibuffer-cover
                   :string (or string (make-string (frame-width) ?\ ))
                   :position (cons 0 (- (frame-pixel-height) (window-pixel-height win)))
                   :height (+ (window-height win) 1)
                   :background-color color
                   :foreground-color color
                   :lines-truncate t
                   :timeout 3)))

(defun vertico-posframe--hide-minibuffer-cover ()
  "Hide minibuffer cover."
  ;; FIXME: delay 0.1 second to remove minibuffer cover, which can
  ;; limit minibuffer flicker.
  (run-with-timer
   0.1 nil
   (lambda ()
     (posframe-hide vertico-posframe--minibuffer-cover))))

(defun vertico-posframe--show-minibuffer-p ()
  "Test show minibuffer or not."
  (or current-input-method
      (cl-some
       (lambda (rule)
         (cond ((functionp rule)
                (funcall rule))
               ((and rule (stringp rule))
                (string-match-p rule (symbol-name this-command)))
               ((symbolp rule)
                (symbol-value rule))
               (t nil)))
       vertico-posframe-show-minibuffer-rules)))

(defun vertico-posframe-last-window ()
  "Get the last actived window before active minibuffer."
  (let ((window (minibuffer-selected-window)))
    (or (if (window-live-p window)
            window
          (next-window))
        (selected-window))))

(defun vertico-posframe--hide ()
  "Hide vertico buffer."
  (when (posframe-workable-p)
    (posframe-hide vertico-posframe--buffer)
    (vertico-posframe--hide-minibuffer-cover)))

(defun vertico-posframe--setup ()
  "Setup minibuffer overlay, which pushes the minibuffer content down."
  (add-hook 'minibuffer-exit-hook 'vertico-posframe--hide nil 'local)
  (setq-local cursor-type '(bar . 0)))

(defun vertico-posframe--minibuffer-message (message &rest _args)
  "Advice function of `minibuffer-message'.
Argument MESSAGE ."
  (let* ((count (vertico-posframe--format-count))
         (contents (buffer-string)))
    (vertico-posframe--show (concat count contents message))))

;;;###autoload
(defun vertico-posframe-cleanup ()
  "Remove frames and buffers used for vertico-posframe."
  (interactive)
  (posframe-delete vertico-posframe--buffer)
  (posframe-delete vertico-posframe--minibuffer-cover))

;;;###autoload
(define-minor-mode vertico-posframe-mode
  "Display Vertico in posframe instead of the minibuffer."
  :global t
  (cond
   (vertico-posframe-mode
    (advice-add #'minibuffer-message :before #'vertico-posframe--minibuffer-message)
    (advice-add #'vertico--display-candidates :override #'vertico-posframe--display)
    (advice-add #'vertico--setup :after #'vertico-posframe--setup))
   (t
    (advice-remove #'minibuffer-message #'vertico-posframe--minibuffer-message)
    (advice-remove #'vertico--display-candidates #'vertico-posframe--display)
    (advice-remove #'vertico--setup #'vertico-posframe--setup))))

(provide 'vertico-posframe)
;;; vertico-posframe.el ends here
