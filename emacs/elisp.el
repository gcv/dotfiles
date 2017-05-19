(put 'add-hook 'lisp-indent-function 1)

(diminish 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq show-trailing-whitespace t)
    (paredit-mode 1)
    (eldoc-mode)
    (elisp-slime-nav-mode t)
    (define-key emacs-lisp-mode-map (kbd "C-m") 'newline-and-indent)))

(add-hook 'lisp-interaction-mode-hook
  (lambda ()
    (paredit-mode 1)
    (define-key lisp-interaction-mode-map (kbd "C-x C-j") 'eval-print-last-sexp)
    (eldoc-mode)))

(add-hook 'ielm-mode-hook
  (lambda ()
    (paredit-mode 1)
    (eldoc-mode)
    (elisp-slime-nav-mode t)
    (local-set-key (kbd "C-S-d") (lambda ()
                                   (interactive)
                                   (comint-send-eof)
                                   (kill-buffer)))))
