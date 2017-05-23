;;; ----------------------------------------------------------------------------
;;; various library dependencies
;;;
;;; NB: Because transitive dependencies are not pinned to the dependent's
;;; repository, order in this section matters!
;;; ----------------------------------------------------------------------------

(use-package async    :pin melpa-stable)
(use-package dash     :pin melpa-stable) ; a modern list library
(use-package s        :pin melpa-stable) ; string handling
(use-package f        :pin melpa-stable) ; file handling
(use-package queue    :pin gnu)
(use-package epl      :pin melpa-stable) ; package.el wrapper
(use-package pkg-info :pin melpa-stable) ; Emacs package



;;; ----------------------------------------------------------------------------
;;; real packages
;;;
;;; NB: Specify transitive dependency pins above the dependent's use-package
;;; form.
;;; ----------------------------------------------------------------------------

(add-to-list 'package-pinned-packages '(avy . "melpa-stable"))

(use-package ace-window
  :pin melpa-stable
  :config (progn
            (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m))
            (global-set-key (kbd "C-M-o") 'ace-window)
            ))


(add-to-list 'package-pinned-packages '(markup-faces . "melpa-stable"))

(use-package adoc-mode
  :pin melpa-stable
  :config (progn

            (add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))

            (add-hook 'adoc-mode-hook
              (lambda ()
                (setq mode-name "AD")
                (setq show-trailing-whitespace t)
                (visual-line-mode)))

            ))


(use-package ag
  :pin melpa-stable)


(use-package avy
  :pin melpa-stable
  :config (progn
            (global-set-key (kbd "M-j") 'avy-goto-char)
            (global-set-key (kbd "C-M-j") 'avy-goto-line)
            ))


(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable"))

(use-package cider
  :pin melpa-stable
  :config (progn

            (setq cider-show-error-buffer t
                  cider-prompt-for-symbol nil
                  cider-repl-tab-command 'indent-for-tab-command
                  cider-repl-display-help-banner nil
                  cider-repl-history-file (concat user-emacs-directory "nrepl-history")
                  cider-mode-line '(:eval (format " cider[%s]" (cider-current-ns))))

            (add-hook 'cider-mode-hook
              (lambda ()
                (paredit-mode 1)
                (define-key cider-mode-map (kbd "C-c C-,") #'cider-test-run-tests)
                (define-key cider-mode-map (kbd "C-c C-t") nil)
                (define-key cider-mode-map (kbd "C-c M-p") nil)))

            (add-hook 'cider-test-report-mode-hook
              (lambda ()
                (paredit-mode 1)
                (define-key cider-test-report-mode-map (kbd "C-c C-,") #'cider-test-run-tests)))

            (add-hook 'cider-repl-mode-hook
              (lambda ()
                (paredit-mode 1)
                (subword-mode)))

            (defun cv--display-buffer-reuse-window-nil (orig-fn &rest args)
              (let ((display-buffer-overriding-action '(display-buffer-reuse-window . nil)))
                (apply orig-fn args)))

            (advice-add 'cider-switch-to-repl-buffer :around #'cv--display-buffer-reuse-window-nil)
            (advice-add 'cider-switch-to-last-clojure-buffer :around #'cv--display-buffer-reuse-window-nil)

            ))


(use-package clojure-mode
  :pin melpa-stable
  :config (progn

            (add-hook 'clojure-mode-hook
              (lambda ()
                (paredit-mode 1)
                (subword-mode)
                (setq show-trailing-whitespace t)
                (define-key clojure-mode-map (kbd "C-m") 'newline-and-indent)
                (define-key clojure-mode-map (kbd "C-c C-t") 'projectile-toggle-between-implementation-and-test)
                ;; indentation fixes
                (put-clojure-indent 'if 4)
                (put-clojure-indent 'if-not 4)
                (put-clojure-indent 'if-let 4)
                (put-clojure-indent 'let-kw 2)
                (put-clojure-indent 'handle 1)))

            ))


(use-package cmake-mode
  :pin melpa-stable
  :config (progn
            (setq cmake-tab-width 4)
            ))


(use-package company                    ; "comp"lete "any"thing
  :pin melpa-stable
  :diminish ""
  :config (progn

            (global-company-mode)
            (setq company-idle-delay nil)

            (global-set-key (kbd "C-.") 'company-complete)

            (let ((company-backends (list 'company-slime
                                          'company-lua
                                          'company-web-html)))
              (dolist (backend company-backends)
                (when (functionp (lambda backend))
                  (add-to-list 'company-backends backend))))

            ))


(add-to-list 'package-pinned-packages '(lua-mode . "melpa"))

(use-package company-lua
  :pin melpa)


(add-to-list 'package-pinned-packages '(web-mode . "melpa-stable"))
(add-to-list 'package-pinned-packages '(web-completion-data . "melpa-stable"))

(use-package company-web
  :pin melpa-stable)


(use-package counsel                    ; Ivy / Swiper / Counsel
  :pin melpa
  :defer t)


(use-package deft
  :pin melpa-stable
  :config (progn
            (setq deft-extension "org"
                  deft-directory "~/Notes/NV/"
                  deft-text-mode 'org-mode)
            ))


(use-package elisp-slime-nav
  :pin melpa-stable
  :diminish "")


(use-package flycheck
  :pin melpa
  :defer t)


(use-package flycheck-rust
  :pin melpa
  :defer t)


(use-package fountain-mode              ; screenwriting
  :pin melpa-stable
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))
            ))


(use-package fringe-helper
  :pin melpa
  :config (progn

            (when (eq window-system 'ns) (set-fringe-mode '(0 . 8)))

            (define-fringe-bitmap 'cv--fringe-backslash (fringe-helper-convert
                                                         "........"
                                                         ".X......"
                                                         ".XX....."
                                                         "..XX...."
                                                         "...XX..."
                                                         "....XX.."
                                                         ".....X.."
                                                         "........"))

            (setcdr (assq 'continuation fringe-indicator-alist) '(nil cv--fringe-backslash))
            ;;(setq visual-line-fringe-indicators '(nil cv--fringe-backslash))

            ))


(use-package golden-ratio
  :pin melpa
  :diminish " φ"
  :config (progn
            (add-to-list 'golden-ratio-extra-commands 'mouse-set-point)
            (add-to-list 'golden-ratio-extra-commands 'flip-windows)
            (add-to-list 'golden-ratio-extra-commands 'switch-to-last-terminal-buffer)
            (add-to-list 'golden-ratio-extra-commands 'ace-window)
            (add-to-list 'golden-ratio-extra-commands 'avy-goto-char)
            (add-to-list 'golden-ratio-extra-commands 'avy-goto-line)
            (add-to-list 'golden-ratio-exclude-modes 'magit-key-mode)
            (add-to-list 'golden-ratio-exclude-modes 'which-key-mode)
            (add-to-list 'golden-ratio-exclude-buffer-names "*buffer-selection*")
            ))


;; http://www.emacswiki.org/emacs/GotoChg
(use-package goto-chg
  :pin marmalade
  :config (progn
            (when window-system (global-set-key (kbd "C-M-[") 'goto-last-change))
            (global-set-key (kbd "C-M-]") 'goto-last-change-reverse)
            ))


(use-package haskell-mode
  :pin melpa-stable
  :config (progn

            (add-hook 'haskell-mode-hook
              (lambda ()
                (turn-on-haskell-unicode-input-method)
                (turn-on-haskell-indentation)))

            ))


(add-to-list 'package-pinned-packages '(helm-core . "melpa-stable"))
(add-to-list 'package-pinned-packages '(popup . "melpa-stable"))

(use-package helm
  :pin melpa-stable
  :diminish ""
  :config (progn

            (require 'helm-config)

            (global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
            (global-set-key (kbd "C-M-y") 'helm-show-kill-ring)

            (defun helm-find-file-ace-window (file)
              "Use ace-window to select a window to display file."
              (ace-select-window)
              (find-file file))

            (defun helm-switch-buffer-ace-window (buffer)
              "Use ace-window to select a window to display buffer."
              (ace-select-window)
              (switch-to-buffer buffer))

            ;; XXX: Something about helm-projectile is weird in how it loads
            ;; actions. This is a workaround to enable these ace-window actions.
            (helm-mode 1)

            (add-to-list 'helm-find-files-actions
                         '("Find file with ace-window" . helm-find-file-ace-window)
                         :append)
            (add-to-list 'helm-type-buffer-actions
                         '("Switch to buffer with ace-window" . helm-switch-buffer-ace-window)
                         :append)

            (define-key helm-find-files-map (kbd "S-<return>")
              #'(lambda ()
                  (interactive)
                  (with-helm-alive-p
                    (helm-exit-and-execute-action 'helm-find-file-ace-window))))

            (define-key helm-buffer-map (kbd "S-<return>")
              #'(lambda ()
                  (interactive)
                  (with-helm-alive-p
                    (helm-exit-and-execute-action 'helm-switch-buffer-ace-window))))

            (helm-mode -1)
            ;;; XXX: Weirdness ends here.

            (setq helm-echo-input-in-header-line t)

            (dolist (boring-buffer-regex boring-buffers)
              (add-to-list 'helm-boring-buffer-regexp-list boring-buffer-regex))

            ;; the default C-x c is too close to C-x C-c
            (global-set-key (kbd "C-c h") 'helm-command-prefix)
            (global-unset-key (kbd "C-x c"))

            ;; tab fix?
            ;;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

            (custom-set-variables '(helm-minibuffer-history-key nil))

            (setq helm-split-window-in-side-p t
                  helm-ff-file-name-history-use-recentf t)

            ;; the buffer list mode string is not useful and too long
            (setq helm-buffer-max-len-mode 0)

            ))


(use-package helm-ag
  :pin melpa-stable)


(use-package helm-gtags
  :pin melpa-stable
  :config (progn

            (setq helm-gtags-path-style 'root
                  helm-gtags-ignore-case t
                  helm-gtags-use-input-at-cursor t
                  helm-gtags-display-style 'detail
                  ;;helm-gtags-auto-update t
                  helm-gtags-pulse-at-cursor t
                  helm-gtags-direct-helm-completing t
                  helm-gtags-fuzzy-match t)

            (add-hook 'helm-gtags-mode-hook
              (lambda ()
                (diminish-minor-mode 'helm-gtags-mode)
                (local-set-key (kbd "C-c C-t r") 'helm-gtags-find-rtag)
                (local-set-key (kbd "C-c C-t f") 'helm-gtags-parse-file)
                (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
                (local-set-key (kbd "M-,") 'helm-gtags-pop-stack)
                (local-set-key (kbd "M-*") 'helm-gtags-pop-stack)))

            ))


(add-to-list 'package-pinned-packages '(projectile . "melpa-stable"))

(use-package helm-projectile
  :pin melpa-stable
  :config (progn
            (helm-projectile-on)
            ))


(use-package highlight
  :pin marmalade
  :config (progn

            (defface cv--hlt-highlight
              '((((class color) (background light))
                 :background "darkseagreen2")
                (((class color) (background dark))
                 :background "#004400")
                (t :inverse-video t))
              "Custom highlighting background for hlt-highlight.")

            (setq hlt-last-face 'cv--hlt-highlight)

            (global-set-key (kbd "C-c H h") 'hlt-highlight)
            (global-set-key (kbd "C-c H u") 'hlt-unhighlight-region)
            (global-set-key (kbd "C-c H U") #'(lambda ()
                                                (interactive)
                                                (save-excursion
                                                  (mark-whole-buffer)
                                                  (hlt-unhighlight-region-for-face hlt-last-face))))

            ))


(use-package hydra
  :pin melpa-stable
  :config (progn
            ))


(use-package ivy                        ; Ivy / Swiper / Counsel
  :pin melpa
  :config (progn
            ;;(ivy-mode 1)
            ;;(setq projectile-completion-system 'ivy)
            ;;(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done) ; enter navigates into a directory
            ))


(use-package js2-mode
  :pin melpa
  :config (progn

            (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
            (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))

            (setq js2-basic-offset 2
                  js2-mirror-mode nil
                  js2-bounce-indent-p nil)

            (add-hook 'js2-mode-hook
              (lambda ()
                (setq mode-name "JS")
                (local-set-key (kbd "C-m") 'newline-and-indent)
                (local-set-key (kbd "M-,") 'pop-tag-mark)
                (local-unset-key (kbd "M-j"))
                (subword-mode)
                (setq show-trailing-whitespace t)))

            ))


(use-package ledger-mode
  :pin melpa-stable
  :config (progn

            (add-to-list 'auto-mode-alist '("\\.dat$" . ledger-mode))

            (add-hook 'ledger-mode-hook
              (lambda ()
                (local-set-key (kbd "C-c C-f") 'flush-to-fill-column)))

            ))


(use-package leuven-theme
  :pin melpa
  :defer t)


(use-package lua-mode
  :pin melpa
  :config (progn

            (add-hook 'lua-mode-hook
              (lambda ()
                (subword-mode)
                (setq show-trailing-whitespace t)))

            ))


(add-to-list 'package-pinned-packages '(git-commit . "melpa-stable"))
(add-to-list 'package-pinned-packages '(magit-popup . "melpa-stable"))
(add-to-list 'package-pinned-packages '(with-editor . "melpa-stable"))

(use-package magit
  :pin melpa-stable
  :config (progn

            (diminish 'magit-auto-revert-mode)

            (setq magit-commit-show-diff nil)
            (setq magit-diff-refine-hunk t) ; "all" shows word differences in all hunks
            ;;(setq auto-revert-check-vc-info t)

            (magit-define-popup-switch 'magit-log-popup ?f "Follow renames" "--follow")

            (add-hook 'magit-status-mode-hook
              (lambda ()
                (define-key magit-status-mode-map (kbd "W")
                  (lambda ()
                    (interactive)
                    (if (member "-w" magit-diff-options)
                        (setq magit-diff-options (remove "-w" magit-diff-options))
                      (add-to-list 'magit-diff-options "-w"))
                    (magit-refresh)))))

            (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
            (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

            ))


(use-package markdown-mode
  :pin melpa-stable
  :config (progn

            (add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
            (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

            (add-hook 'markdown-mode-hook
              (lambda ()
                (local-unset-key (kbd "M-<up>"))
                (local-unset-key (kbd "M-<down>"))
                (setq show-trailing-whitespace t)
                (visual-line-mode)))

            ))


(use-package multiple-cursors
  :pin melpa-stable
  :config (progn
            (global-set-key (kbd "C-?") 'mc/edit-lines)
            (global-set-key (kbd "C->") 'mc/mark-next-like-this)
            (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
            (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
            ))


(use-package multi-term
  :pin melpa
  :config (progn
            (setq multi-term-program (executable-find "zsh"))
            (setq multi-term-switch-after-close nil)
            ;; rebind C-r to the terminal's native one
            (setq term-bind-key-alist (remove* '"C-r" term-bind-key-alist :test 'equal :key 'car))
            (add-to-list 'term-bind-key-alist '("C-r" . term-send-reverse-search-history))
            (add-to-list 'term-bind-key-alist '("M-DEL" . term-send-backward-kill-word))
            (when window-system (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev)))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
            (add-to-list 'term-bind-key-alist '("C-o" . flip-windows))
            (add-to-list 'term-bind-key-alist '("C-c C-z" . flip-windows))
            ))


(use-package olivetti                   ; focused writing mode
  :pin melpa-stable
  :config (progn

            (add-hook 'olivetti-mode-hook
              (lambda ()
                (setq right-fringe-width 0
                      left-fringe-width 0)
                (set-window-buffer (frame-selected-window) (current-buffer))
                (olivetti-set-width 90)))

            ))


(use-package paredit
  :pin melpa-stable
  :diminish " π"
  :config (progn

            (define-key paredit-mode-map (kbd "C-<left>") 'backward-word)
            (define-key paredit-mode-map (kbd "C-<right>") 'forward-word)
            (define-key paredit-mode-map (kbd "M-<up>") 'scroll-up-line)
            (define-key paredit-mode-map (kbd "M-<down>") 'scroll-down-line)
            (define-key paredit-mode-map (kbd "M-S-<up>") 'paredit-splice-sexp-killing-backward)
            (define-key paredit-mode-map (kbd "M-S-<down>") 'paredit-splice-sexp-killing-forward)
            (if window-system
                (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
              (define-key paredit-mode-map (kbd "M-[") nil))
            (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)

            ))


(use-package perspective
  :pin melpa
  :config (progn

            (persp-mode)
            (persp-turn-off-modestring)

            (setq persp-interactive-completion-function 'ido-completing-read)

            ;; persp-mode does not use a prefix for defining keybindings, strange
            (define-key persp-mode-map (kbd "C-c C-p s") 'persp-switch)
            (define-key persp-mode-map (kbd "C-c M-p") 'persp-switch)
            (define-key persp-mode-map (kbd "C-c C-p k") 'persp-remove-buffer)
            (define-key persp-mode-map (kbd "C-c C-p c") 'persp-kill)
            (define-key persp-mode-map (kbd "C-c C-p r") 'persp-rename)
            (define-key persp-mode-map (kbd "C-c C-p a") 'persp-add-buffer)
            (define-key persp-mode-map (kbd "C-c C-p A") 'persp-set-buffer)
            (define-key persp-mode-map (kbd "C-c C-p i") 'persp-import)
            (define-key persp-mode-map (kbd "C-c C-p n") 'persp-next)
            (define-key persp-mode-map (kbd "C-c C-p <right>") 'persp-next)
            (define-key persp-mode-map (kbd "C-c C-p p") 'persp-prev)
            (define-key persp-mode-map (kbd "C-c C-p <left>") 'persp-prev)

            (defun persp-mode-cleanup-killed-buffers ()
              (interactive)
              (let ((live-buffers (-reject #'(lambda (b) (null (buffer-name b))) (persp-buffers persp-curr))))
                (setf (persp-buffers persp-curr) live-buffers)))

            (defun cv--persp-set-ido-buffers ()
              (when (boundp 'ido-temp-list)
                (setq ido-temp-list
                      (remove-if (lambda (name)
                                   (cl-some
                                    (lambda (ignore)
                                      (cond ((stringp ignore) (string-match-p ignore name))
                                            ((functionp ignore) (funcall ignore name))
                                            (t (error "unknown ido-ignore-buffers type"))))
                                    ido-ignore-buffers))
                                 ido-temp-list))))

            (advice-add 'persp-set-ido-buffers :after #'cv--persp-set-ido-buffers)

            ))


(use-package projectile
  :pin melpa-stable
  :config (progn
            (projectile-global-mode)
            (setq projectile-enable-caching nil)
            (setq projectile-tags-command "ctags -Re -f \"%s\" %s")
            (setq-default projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
            ))


(use-package rust-mode
  :pin melpa
  :config (progn

            (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

            (add-hook 'rust-mode-hook
              (lambda ()
                (flycheck-mode)))

            ))


(add-to-list 'package-pinned-packages '(macrostep . "melpa-stable"))

(use-package slime
  :pin melpa-stable
  :config (progn

            (setq slime-net-coding-system 'utf-8-unix)
            (setq inferior-lisp-program "sbcl")

            (setq common-lisp-hyperspec-root "~/Files/Common Lisp/CL HyperSpec 7.0/HyperSpec")
            (setq common-lisp-hyperspec-symbol-table
                  (concat common-lisp-hyperspec-root "/Data/Map_Sym.txt"))

            ;;(add-hook 'slime-mode-hook
            ;;  (lambda ()
            ;;    (setq slime-truncate-lines nil)))

            ;;(add-hook 'slime-repl-mode-hook
            ;;  (lambda ()
            ;;    (paredit-mode 1)
            ;;    (define-key slime-repl-mode-map "[" 'paredit-open-square)
            ;;    (define-key slime-repl-mode-map "]" 'paredit-close-square)
            ;;    (define-key slime-repl-mode-map "{" 'paredit-open-curly)
            ;;    (define-key slime-repl-mode-map "}" 'paredit-close-curly)))

            ))


(use-package slime-company
  :pin melpa-stable
  :config (progn
            ;; XXX: This is here instead of the slime :config section to make
            ;; sure it runs after the slime-company is available.
            (slime-setup '(slime-fancy slime-company))
            ))


(use-package smex                       ; smart M-x completion
  :pin melpa-stable
  :config (progn
            (setq smex-save-file (concat user-emacs-directory ".smex-items"))
            (global-set-key (kbd "M-x") (lambda ()
                                          (interactive)
                                          (or (boundp 'smex-cache)
                                              (smex-initialize))
                                          (smex)))
            ))


(use-package solarized-theme
  :pin melpa-stable
  :defer t)


(use-package swift-mode
  :pin melpa
  :config (progn

            (add-hook 'swift-mode-hook
              (lambda ()
                (local-unset-key (kbd "C-c C-z"))))

            ))


(use-package swiper                     ; Ivy / Swiper / Counsel
  :pin melpa
  :config (progn
            (global-set-key (kbd "M-S-C-s") 'swiper)
            ))


(use-package web-mode
  :pin melpa-stable
  :config (progn

            (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

            (setq web-mode-enable-current-element-highlight t
                  web-mode-markup-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-css-indent-offset 2)

            (add-hook 'web-mode-hook
              (lambda ()
                (local-set-key (kbd "C-m") 'newline-and-indent)))

            ))


(use-package wgrep
  :pin melpa-stable)


(use-package which-key
  :pin melpa-stable
  :diminish ""
  :config (progn
            (which-key-mode)
            ;;(which-key-setup-side-window-bottom)
            ;;(which-key-setup-side-window-right)
            ;;(which-key-setup-side-window-right-bottom)
            (which-key-setup-minibuffer)
            ))


(use-package zenburn-theme
  :pin melpa-stable
  :defer t)
