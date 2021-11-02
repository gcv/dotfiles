;;; -*- lexical-binding: t; -*-

(put 'add-hook 'lisp-indent-function 1)
(put 'setq-default 'lisp-indent-function 1)
(put 'use-package 'lisp-indent-function 1)
(put 'cl-flet 'lisp-indent-function 1)
(put 'cl-labels 'lisp-indent-function 1)
(put 'customize-set-variable 'lisp-indent-function 1)


(diminish 'eldoc-mode)


(defun /emacs-lisp-mode-hook ()
  (setq show-trailing-whitespace t)
  (paredit-mode 1)
  (eldoc-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
  (define-key emacs-lisp-mode-map (kbd "C-m") 'newline-and-indent))

(add-hook 'emacs-lisp-mode-hook #'/emacs-lisp-mode-hook)


(defun /lisp-interaction-mode-hook ()
  (paredit-mode 1)
  (define-key lisp-interaction-mode-map (kbd "C-x C-j") 'eval-print-last-sexp)
  (eldoc-mode))

(add-hook 'lisp-interaction-mode-hook #'/lisp-interaction-mode-hook)


(defun /ielm-mode-hook ()
  ;; deal with comint-input-ring being buffer-local and therefore not subject to
  ;; savehist-mode helpfulness
  (add-hook 'kill-buffer-hook
    (lambda () (setq ielm-comint-input-ring comint-input-ring))
    nil t)
  (when ielm-comint-input-ring
    (setq comint-input-ring ielm-comint-input-ring))
  ;; normal setup
  (paredit-mode 1)
  (eldoc-mode)
  (local-set-key (kbd "C-S-d") (lambda ()
                                 (interactive)
                                 (comint-send-eof)
                                 (kill-buffer))))

(defvar ielm-comint-input-ring nil) ; global copy of the buffer-local variable
(add-to-list 'savehist-additional-variables 'ielm-comint-input-ring)
(add-hook 'ielm-mode-hook #'/ielm-mode-hook)
