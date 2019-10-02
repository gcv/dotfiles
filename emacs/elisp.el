(put 'add-hook 'lisp-indent-function 1)
(put 'setq-default 'lisp-indent-function 1)
(put 'use-package 'lisp-indent-function 1)
(put 'cl-flet 'lisp-indent-function 1)
(put 'cl-labels 'lisp-indent-function 1)


(diminish 'eldoc-mode)


(defun /emacs-lisp-mode-hook ()
  (setq show-trailing-whitespace t)
  (paredit-mode 1)
  (eldoc-mode)
  (elisp-slime-nav-mode t)
  (define-key emacs-lisp-mode-map (kbd "C-m") 'newline-and-indent))

(add-hook 'emacs-lisp-mode-hook #'/emacs-lisp-mode-hook)


(defun /lisp-interaction-mode-hook ()
  (paredit-mode 1)
  (elisp-slime-nav-mode t)
  (define-key lisp-interaction-mode-map (kbd "C-x C-j") 'eval-print-last-sexp)
  (eldoc-mode))

(add-hook 'lisp-interaction-mode-hook #'/lisp-interaction-mode-hook)


(defun /ielm-mode-hook ()
  (paredit-mode 1)
  (eldoc-mode)
  (elisp-slime-nav-mode t)
  (local-set-key (kbd "C-S-d") (lambda ()
                                 (interactive)
                                 (comint-send-eof)
                                 (kill-buffer))))

(add-hook 'ielm-mode-hook #'/ielm-mode-hook)
