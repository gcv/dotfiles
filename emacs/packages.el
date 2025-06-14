;;; -*- lexical-binding: t -*-

;;; ----------------------------------------------------------------------------
;;; various library dependencies
;;;
;;; NB: Because transitive dependencies are not pinned to the dependent's
;;; repository, order in this section matters!
;;; ----------------------------------------------------------------------------

(use-package a)                          ; Clojure-style associative data
(use-package async)
(use-package dash)                       ; a modern list library
(use-package f)                          ; file handling
(use-package ht)                         ; hash tables
(use-package pfuture)
(use-package queue :pin gnu)
(use-package s)                          ; string handling


;;; ----------------------------------------------------------------------------
;;; real packages
;;; ----------------------------------------------------------------------------

(use-package ace-window
  :bind
  (("C-M-o" . ace-window))

  :custom
  (aw-display-mode-overlay nil)
  (aw-make-frame-char ?!)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m))
  )


(use-package adoc-mode
  :mode "\\.\\(adoc\\|asciidoc\\)$"

  :config
  (defun /adoc-mode-hook ()
    (setq mode-name "AD")
    (setq show-trailing-whitespace t)
    (visual-line-mode))

  (add-hook 'adoc-mode-hook #'/adoc-mode-hook)
  )


(use-package ag)


(use-package avy
  :bind
  (("M-j" . avy-goto-char)
   ("C-M-j" . avy-goto-line))
  )


(use-package breadcrumb
  :pin "gnu"
  :if (>= emacs-major-version 29)
  ;; NB: :vc checkouts should be added to elpa-level .gitignore.
  ;;:vc (:fetcher github :repo "joaotavora/breadcrumb")
  )


;;; configuration bisect utility
(use-package bug-hunter
  :pin "gnu")


(use-package casual-avy
  :bind ("M-g a" . casual-avy-tmenu))


(use-package casual
  :bind ((:map dired-mode-map
               ("C-c c" . casual-dired-tmenu))
         (:map Info-mode-map
               ("C-c c" . casual-info-tmenu))
         (:map isearch-mode-map
               ("C-c c" . casual-isearch-tmenu))
         (:map calc-mode-map
               ("C-c c" . casual-calc-tmenu))))


(use-package cider
  ;; :mode (rx (or ".clj" ".cljs" ".cljc" ".cljx") eol)

  :custom
  (cider-show-error-buffer 'except-in-repl)
  (cider-auto-select-error-buffer nil)
  (cider-prompt-for-symbol nil)
  (cider-repl-tab-command 'indent-for-tab-command)
  (cider-repl-display-help-banner nil)
  (cider-repl-history-file (concat user-emacs-directory "nrepl-history"))
  (cider-mode-line '(:eval (format " cider[%s]" (cider-current-ns))))

  :config
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*cider-repl .*"
  ;;                (display-buffer-reuse-window display-buffer-in-side-window)
  ;;                (reusable-frames . visible)
  ;;                (side . bottom)
  ;;                (window-height . 0.2)))

  (defun /cider-mode-hook ()
    (paredit-mode 1)
    (define-key cider-mode-map (kbd "C-c C-,") #'cider-test-run-tests)
    (define-key cider-mode-map (kbd "C-c C-t") nil)
    (define-key cider-repl-mode-map (kbd "C-c M-p") nil)
    (define-key cider-mode-map (kbd "C-c M-p") nil))

  (add-hook 'cider-mode-hook #'/cider-mode-hook)

  (defun /cider-test-report-mode-hook ()
    (paredit-mode 1)
    (define-key cider-test-report-mode-map (kbd "C-c C-,") #'cider-test-run-tests))

  (add-hook 'cider-test-report-mode-hook #'/cider-test-report-mode-hook)

  (defun /cider-repl-mode-hook ()
    (paredit-mode 1)
    (subword-mode))

  (add-hook 'cider-repl-mode-hook #'/cider-repl-mode-hook)

  (defun /display-buffer-reuse-window-nil (orig-fn &rest args)
    (let ((display-buffer-overriding-action '(display-buffer-reuse-window . nil)))
      (apply orig-fn args)))

  (advice-add 'cider-switch-to-repl-buffer :around #'/display-buffer-reuse-window-nil)
  (advice-add 'cider-switch-to-last-clojure-buffer :around #'/display-buffer-reuse-window-nil)
  )


(use-package clojure-mode
  :config
  (defun /clojure-mode-hook ()
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
    (put-clojure-indent 'handle 1))

  (add-hook 'clojure-mode-hook #'/clojure-mode-hook)
  )


(use-package cmake-mode
  :config
  (setq cmake-tab-width 4)
  )


;;; neat package for writing "notebook" style literate programming files
(use-package code-cells
  :pin melpa

  :hook (julia-mode . code-cells-mode)

  :config
  (let ((map code-cells-mode-map))
    (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
    (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
    (define-key map "e" (code-cells-speed-key 'code-cells-eval))
    (define-key map (kbd "TAB") (code-cells-speed-key (lambda ()
                                                        "Show/hide current cell"
                                                        (interactive)
                                                        (outline-minor-mode)
                                                        (if (outline-invisible-p (line-end-position))
                                                            (outline-show-subtree)
                                                          (outline-hide-subtree))))))

  (add-to-list 'code-cells-eval-region-commands '(julia-snail-mode . julia-snail-send-code-cell))
  )


(use-package colorful-mode
  :pin gnu

  :custom
  (colorful-use-prefix nil) ;; cute if t

  :config
  (global-colorful-mode 1)
  )


(use-package crux
  :pin melpa

  :bind
  (("C-M-z" . crux-indent-defun))
  )


(use-package csv-mode
  :pin gnu)


;; color API
;; e.g.:
;; (ct-distance "blanchedalmond" "papayawhip") => 1.8387780600973087
;; (ct-pastel "#4fa5e8") => "#77a3c5"
(use-package ct
  :pin melpa)


(use-package deadgrep
  :pin melpa
  :custom
  (deadgrep-display-buffer-function 'switch-to-buffer))


(use-package devdocs
  :pin gnu)


;;; XXX: This package is supposed to support changing lighters for major modes
;;; in a more sophisticated way than (setq mode-name "..."), and has hooks into
;;; use-package, but does not seem to work. For example, (delight 'markdown-mode
;;; " MD" :major) does not work, though the documentation says it should.
;;; Varying the last argument does nothing. Leaving this comment here in case I
;;; forget and go back looking for it again.
;; (use-package delight)


(use-package diminish)


(use-package dired-sidebar
  :pin melpa

  :bind
  (("C-x C-n" . (lambda ()
                  (interactive)
                  (if current-prefix-arg
                      (call-interactively #'dired-sidebar-toggle-sidebar)
                    (dired-sidebar-follow-file)
                    (call-interactively #'dired-sidebar-jump-to-sidebar)))))

  :custom
  (dired-sidebar-one-instance-p t)

  :commands (dired-sidebar-toggle-sidebar dired-sidebar-follow-file)

  :init
  (add-hook 'dired-sidebar-mode-hook
    (lambda ()
      (unless (file-remote-p default-directory)
        (auto-revert-mode))))

  (add-hook 'dired-sidebar-mode-hook #'hl-line-mode)

  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-theme 'ascii)
  )


(use-package dired-subtree
  :pin melpa

  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle))

  :init
  (require 'dired-subtree)
  )


(use-package dirvish
  :pin melpa

  :bind
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map
   ("RET" . dired-find-file)
   ("^"   . dired-up-directory)
   ;;("h" . dired-up-directory)
   ;;("j" . dired-next-line)
   ;;("k" . dired-previous-line)
   ;;("l" . dired-find-file)
   ("i"     . wdired-change-to-wdired-mode)
   ;;("." . dired-omit-mode)
   ("b"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("TAB" . dirvish-subtree-toggle)
   ("C-M-]" . dirvish-history-go-forward)
   ("C-M-[" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))

  :custom
  (dirvish-media-auto-cache-threshold '(0)) ; disable auto cache
  (dirvish-preview-dispatchers '(archive no-media)) ; disable media previews
  (dirvish-quick-access-entries
   '(("h" "~/" "Home") ;; e.g.: `qh' to go back home
     ("f" "~/Files" "Files")
     ))
  (dirvish-header-line-format
   '(:left (path)))
  (dirvish-mode-line-format
   '(:left (sort " " file-time " " file-size symlink)
     :right (omit yank index)))
  (dirvish-attributes '(file-time file-size))
  ;;(dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  ;;(dirvish-all-the-icons-height 0.8)  ; fix icon sizes?
  ;;(dirvish-hide-details nil)

  :init
  (dirvish-override-dired-mode)

  :config
  ;;(dirvish-peek-mode 1)
  (setq dired-dwim-target t)
  ;;(setq delete-by-moving-to-trash t)
  ;;(setq dired-mouse-drag-files t)                   ; added in Emacs 29
  ;;(setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  )


(use-package disk-usage
  :pin gnu

  :config
  (setq disk-usage--du-command "du")
  )


;;; obsolete: replaced by envrc
;; (use-package direnv
;;   :config
;;   ;; It's faster to keep this minor mode disabled, and use
;;   ;; direnv-update-environment manually as needed.
;;   ;;(direnv-mode 1)
;;   )


(use-package discover-my-major
  :pin melpa

  :bind
  (("C-h C-m" . discover-my-major))
  )


;;; dtrt: Do The Right Thing indentation mode (tries to cleanly deal with tab and space-formatted files)
(use-package dtrt-indent
  )


(use-package dumb-jump
  :bind
  (("H-." . dumb-jump-go)
   ("H-," . dumb-jump-back))

  :config
  (setq dumb-jump-selector 'popup)
  )


;;; eat: Emulate A Terminal - vterm alternative
(use-package eat
  :pin nongnu
  :custom
  (eat-kill-buffer-on-exit t)

  :config
  (dolist (key '([?\C-u] [?\C-g]
                 [C-left] [C-right] [C-up] [C-down]
                 [M-left] [M-right]))
    (delete key eat-semi-char-non-bound-keys))
  (eat-update-semi-char-mode-map)
  ;; XXX: Workaround awkward need to call eat-reload after changing its keymaps,
  ;; but reloading from :config section causes infinite recursion.
  (defvar eat--prevent-use-package-config-recursion nil)
  (unless eat--prevent-use-package-config-recursion
    (setq eat--prevent-use-package-config-recursion t)
    (eat-reload))
  (makunbound 'eat--prevent-use-package-config-recursion)
  )


;;; eglot: LSP support mode (for Emacs <29, only version 1.9 or earlier work)
(use-package eglot
  :if (< emacs-major-version 29))


;;; obsolete: set-fontset-font invocations with appropriate fonts should work
;; (use-package emojify
;;   :pin melpa
;;   :defer t
;;   :config (progn
;;             ;; 'unicode possibly broken on NextStep port?
;;             (setq emojify-display-style 'image)
;;             (setq emojify-emoji-style (list 'unicode))
;;             ;; NB: This mode does not seem to be needed for the Mac port.
;;             ;;(global-emojify-mode)
;;             ))


;;; handle direnv configuration on a per-buffer basis, instead of globally (as
;;; the direnv package does)
(use-package envrc
  :pin melpa

  :custom
  ;;(envrc-lighter nil)
  (envrc-on-lighter '(" ε"))

  :hook
  (after-init . envrc-global-mode)
  )


;;; gopher and gemini client
(use-package elpher)


;;; elvish shell script mode
(use-package elvish-mode
  :pin melpa)


(use-package eshell-autojump)


(use-package expand-region
  :bind
  (("C-S-SPC" . er/expand-region)
   ("C-c SPC" . er/expand-region)       ; terminal
   )
  )


(use-package fish-mode
  :pin melpa)


(use-package fountain-mode              ; screenwriting
  :mode "\\.fountain$")


(use-package fringe-helper
  :pin melpa
  :if window-system
  :demand t

  ;;:custom
  ;;(visual-line-fringe-indicators '(/fringe-fwdslash /fringe-backslash))

  :config
  (set-fringe-mode '(0 . 8))
  ;;(set-fringe-mode '(8 . 8))

  (define-fringe-bitmap '/fringe-fwdslash (fringe-helper-convert
                                           "........"
                                           "......X."
                                           ".....XX."
                                           "....XX.."
                                           "...XX..."
                                           "..XX...."
                                           "..X....."
                                           "........"))

  (define-fringe-bitmap '/fringe-bckslash (fringe-helper-convert
                                           "........"
                                           ".X......"
                                           ".XX....."
                                           "..XX...."
                                           "...XX..."
                                           "....XX.."
                                           ".....X.."
                                           "........"))

  (setcdr (assq 'continuation fringe-indicator-alist) '(/fringe-fwdslash /fringe-bckslash))
  )


(use-package git-auto-commit-mode
  :pin melpa

  :config
  (setq gac-automatically-push-p nil)
  (setq-default gac-debounce-interval 300)
  )


(use-package go-mode
  :pin melpa-stable

  :config
  (defun /go-mode-hook ()
    (setq-local tab-width 4))

  (add-hook 'go-mode-hook #'/go-mode-hook)
  (add-hook 'go-dot-mod-mode-hook #'/go-mode-hook)
  )


;;; obsolete: replaced by zoom
;; (use-package golden-ratio
;;   :pin melpa
;;   :diminish " φ"
;;
;;   :config
;;   ;;(setq golden-ratio-auto-scale t)
;;
;;   (add-to-list 'golden-ratio-extra-commands 'mouse-set-point)
;;   (add-to-list 'golden-ratio-extra-commands 'flip-windows)
;;   (add-to-list 'golden-ratio-extra-commands 'switch-to-last-terminal-buffer)
;;   (add-to-list 'golden-ratio-extra-commands 'ace-window)
;;   (add-to-list 'golden-ratio-extra-commands 'avy-goto-char)
;;   (add-to-list 'golden-ratio-extra-commands 'avy-goto-line)
;;   (add-to-list 'golden-ratio-exclude-modes 'magit-key-mode)
;;   (add-to-list 'golden-ratio-exclude-modes 'which-key-mode)
;;   (add-to-list 'golden-ratio-exclude-buffer-names "*buffer-selection*")
;;
;;   (defun toggle-golden-ratio ()
;;     (interactive)
;;     (golden-ratio-mode (if golden-ratio-mode 0 1))
;;     (when golden-ratio-mode (golden-ratio)))
;;   )


;;; http://www.emacswiki.org/emacs/GotoChg
(use-package goto-chg
  :bind
  (("C-M-[" . goto-last-change)
   ("C-M-]" . goto-last-change-reverse))
  )


;;; LLM interaction package: https://github.com/karthink/gptel
(use-package gptel
  :pin melpa
  :custom
  (gptel-display-buffer-action nil)
  :config
  (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (gptel-make-anthropic "Claude"
    :stream t
    :key (getenv "ANTHROPIC_API_KEY"))
  (gptel-make-gemini "Gemini"
    :stream t
    :key (getenv "GEMINI_API_KEY"))
  )


(use-package haskell-mode
  :config
  ;; consider installing Intero if Haskell becomes relevant
  (defun /haskell-mode-hook ()
    (turn-on-haskell-unicode-input-method)
    (turn-on-haskell-indentation))

  (add-hook 'haskell-mode-hook #'/haskell-mode-hook)
  )


;;; Hashicorp Configuration Language: dependency for terraform-mode
(use-package hcl-mode)


;;; replacement for built-in Help system
(use-package helpful
  :pin melpa

  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h o" . helpful-symbol)
   ("C-h k" . helpful-key)
   ("C-c C-d h" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command))

  :config
  (when (featurep 'counsel)
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable))
  )


(use-package highlight
  :pin melpa

  :bind
  (("C-c H h" . hlt-highlight)
   ("C-c H u" . hlt-unhighlight-region)
   ("C-c H U" . /hlt-remove-all-highlights-in-buffer))

  :config
  (defface /hlt-highlight
    '((((class color) (background light))
       :background "darkseagreen2")
      (((class color) (background dark))
       :background "#004400")
      (t :inverse-video t))
    "Custom highlighting background for hlt-highlight.")

  (setq hlt-last-face '/hlt-highlight)

  (defun /hlt-remove-all-highlights-in-buffer ()
    (interactive)
    (save-excursion
      (mark-whole-buffer)
      (hlt-unhighlight-region-for-face hlt-last-face)))
  )


(use-package highlight-indent-guides
  :pin melpa

  :custom
  (highlight-indent-guides-method 'character)
  )


(use-package iedit
  :init
  (setq iedit-toggle-key-default nil)

  ;; manually set keys, because iedit overrides some important globals:
  (let ((iedit-toggle-keybinding (kbd "C-;")))
    (define-key global-map iedit-toggle-keybinding 'iedit-mode)
    (define-key global-map (kbd "C-c ;") 'iedit-mode)
    (define-key isearch-mode-map iedit-toggle-keybinding 'iedit-mode-from-isearch)
    (define-key help-map iedit-toggle-keybinding 'iedit-mode-toggle-on-function))
  )


(use-package inspector
  :pin gnu)


(use-package imenu-list
  :pin melpa
  :custom
  (imenu-list-position 'left)
  (imenu-list-size 30))


(use-package js2-mode
  :pin melpa

  :mode
  (("\\.js$" . js2-mode)
   ("\\.jsx$" . js2-jsx-mode))

  :config
  (setq js2-basic-offset 2
        js-switch-indent-offset 2
        js2-mirror-mode nil
        js2-bounce-indent-p nil)

  (defun /js2-mode-hook ()
    (setq mode-name "JS")
    (local-set-key (kbd "C-m") 'newline-and-indent)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (local-unset-key (kbd "M-j"))
    (subword-mode)
    (setq show-trailing-whitespace t))

  (add-hook 'js2-mode-hook #'/js2-mode-hook)
  )


(use-package julia-mode
  :pin melpa

  :config
  (setq julia-indent-offset 3)

  (setq julia-prompt-regexp "julia> ")

  (defun /julia-mode-hook ()
    (subword-mode)
    (setq show-trailing-whitespace t))

  (add-hook 'julia-mode-hook #'/julia-mode-hook)
  )


(use-package julia-snail
  ;;:load-path "~/Files/Emacs/julia-snail"
  :pin melpa

  :custom
  (julia-snail-terminal-type :eat)
  (julia-snail-repl-display-eval-results t)

  :hook
  (julia-mode . julia-snail-mode)
  )


;; color adjustment package
;; kurecolor-decrease-brightness-by-step is especially useful for theme work
(use-package kurecolor)


(use-package ledger-mode
  :pin melpa

  :custom
  (ledger-post-account-alignment-column 2)
  (ledger-post-amount-alignment-column (- fill-column 4))

  :bind
  (("C-c C-f" . flush-to-fill-column))  ; historical
  )


(use-package lua-mode
  :pin melpa

  :mode "\\.lua$"

  :config
  (defun /lua-mode-hook ()
    (subword-mode)
    (setq show-trailing-whitespace t))

  (add-hook 'lua-mode-hook #'/lua-mode-hook)
  )


(use-package magit
  :bind
  (("C-c p v" . magit-status))

  :config
  (diminish 'magit-auto-revert-mode)

  (setq magit-commit-show-diff nil)
  (setq magit-diff-refine-hunk t) ; "all" shows word differences in all hunks
  ;;(setq auto-revert-check-vc-info t)

  (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
  (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

  (defun /magit-window-config ()
    (set-window-fringes nil 8 8))

  (defun /magit-mode-hook ()
    (add-hook 'window-configuration-change-hook #'/magit-window-config nil :local))

  (add-hook 'magit-mode-hook #'/magit-mode-hook)

  (defun /magit-status-mode-hook ()
    ;; Restore window configuration and clean up all Magit buffers.
    ;; The default binding for "q" is #'magit-restore-window-configuration.
    (local-set-key "q" (lambda ()
                         (interactive)
                         (let ((buffers (magit-mode-get-buffers)))
                           (magit-restore-window-configuration)
                           (mapc #'kill-buffer buffers)))))

  (add-hook 'magit-status-mode-hook #'/magit-status-mode-hook)
  )


(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

  (defun /markdown-mode-hook ()
    (setq mode-name "MD")
    (local-unset-key (kbd "M-<up>"))
    (local-unset-key (kbd "M-<down>"))
    (setq show-trailing-whitespace t)
    (visual-line-mode))

  (add-hook 'markdown-mode-hook #'/markdown-mode-hook)
  )


(use-package markdown-toc
  :custom
  (markdown-toc-header-toc-title "")
  )


(use-package minibuffer-line
  :pin gnu

  :custom
  (minibuffer-line-refresh-interval 5)

  (minibuffer-line-format
   '("" (:eval
         (let ((time-str (format-time-string "%m/%d  %R")))
           (concat
            (propertize
             " "
             'display
             `(space :align-to (- right-fringe ,(string-width time-str) 1)))
            time-str)))))

  :init
  (minibuffer-line-mode 1)

  (set-face-attribute 'minibuffer-line nil :inherit 'unspecified)
  (set-face-attribute 'minibuffer-line nil :foreground "dark gray")
  )


(use-package multiple-cursors
  :bind
  (("C-?" . mc/edit-lines)
   ("C-c ?" . mc/edit-lines)
   ("C-M-_" . mc/edit-lines)             ; C-M-/ evals to this in terminal?
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this))
  )


(use-package nix-mode
  :pin melpa
  :mode "\\.nix$")


;;; mail client
(use-package notmuch)


;;; turn on when built-in Org dependency is up-to-date
;;(use-package ob-async)


(use-package ob-restclient
  :pin melpa)


;;; Focused writing mode.
;;; Another writing-assistance package to consider
;;; [[https://github.com/rnkn/binder][Binder]], by the same author.
;;; Binder has a few problems, specifically that it has global
;;; single-project state. And it's also interesting that the author
;;; has GitHub Issues (tickets) turned off for both Binder and
;;; Olivetti.
(use-package olivetti
  :pin melpa
  :custom
  (olivetti-body-width 0.9)
  )


(use-package org-contrib
  :if (>= emacs-major-version 28)
  :pin nongnu)


(use-package origami
  :pin melpa
  :custom
  (origami-show-fold-header t)
  :bind (:map origami-mode-map
              ("C-c f" . origami-toggle-node)
              ("C-c F" . origami-toggle-all-nodes)))


(use-package package-lint
  :pin melpa)


(use-package pandoc-mode)


(use-package paredit
  :diminish " π"

  :config
  (define-key paredit-mode-map (kbd "<RET>") nil) ; paredit-RET broke eval-expression and ielm in paredit-26
  (define-key paredit-mode-map (kbd "C-<left>") 'backward-word)
  (define-key paredit-mode-map (kbd "C-<right>") 'forward-word)
  (define-key paredit-mode-map (kbd "M-<up>") 'scroll-up-line)
  (define-key paredit-mode-map (kbd "M-<down>") 'scroll-down-line)
  (define-key paredit-mode-map (kbd "M-S-<up>") 'paredit-splice-sexp-killing-backward)
  (define-key paredit-mode-map (kbd "M-S-<down>") 'paredit-splice-sexp-killing-forward)
  (define-key paredit-mode-map (kbd "C-c <up>") 'paredit-splice-sexp-killing-backward)
  (define-key paredit-mode-map (kbd "C-c <down>") 'paredit-splice-sexp-killing-forward)
  (define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-slurp-sexp)
  (if window-system
      (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
    (define-key paredit-mode-map (kbd "M-[") nil))
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)

  (defun /paredit-minibuffer-setup-hook ()
    (when (memq this-command '(eval-expression pp-eval-expression))
      (paredit-mode 1)))

  (add-hook 'minibuffer-setup-hook #'/paredit-minibuffer-setup-hook)
  )


(use-package perspective
  ;;:load-path "~/Files/Emacs/perspective-el"
  :pin melpa

  :bind
  (("H-p" . perspective-map)
   ("C-c M-p" . persp-switch))

  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  (persp-interactive-completion-function 'completing-read)
  (persp-sort 'access)

  :init
  (persp-mode)
  (persp-turn-off-modestring)

  (setq persp-state-default-file (concat user-emacs-directory "persp-state.el"))

  ;; Remove posframes before saving (seems to reduce
  ;; likelihood of extra frame state, even though
  ;; Perspective tries to lock it out):
  (defun /persp-state-before-save-hook ()
    (when (and (fboundp 'mini-frame--delete-frame)
               (boundp 'mini-frame-frame))
      (mini-frame--delete-frame mini-frame-frame))
    (when (boundp 'mini-frame-frame)
      (delete-frame mini-frame-frame))
    (when (fboundp 'posframe-delete-all)
      (posframe-delete-all)))

  (add-hook 'persp-state-before-save-hook #'/persp-state-before-save-hook)

  ;; Prompt to save perspectives on exit.
  (defun /persp-kill-emacs-hook ()
    (interactive)
    (when (and (boundp 'persp-state-default-file)
	       (y-or-n-p (format "Save perspectives to %s? " persp-state-default-file)))
      (persp-state-save)))

  (add-hook 'kill-emacs-hook #'/persp-kill-emacs-hook)

  ;; By default, ido-temp-list filters out ido-ignore-buffers from the
  ;; list displayed by ido-switch-buffer, but typing a name from the
  ;; ignored list will still auto-complete it. The following advice
  ;; turns off this behavior. It may make sense to include it in
  ;; Perspective, but would be a breaking change to Perspective's
  ;; current ido-mode integration.
  (defun /persp-set-ido-buffers ()
    (when (boundp 'ido-temp-list)
      (setq ido-temp-list
            (cl-remove-if (lambda (name)
                            (cl-some
                             (lambda (ignore)
                               (cond ((stringp ignore) (string-match-p ignore name))
                                     ((functionp ignore) (funcall ignore name))
                                     (t (error "unknown ido-ignore-buffers type"))))
                             ido-ignore-buffers))
                          ido-temp-list))))

  (advice-add 'persp-set-ido-buffers :after #'/persp-set-ido-buffers)
  )


(use-package poporg
  :pin melpa)


(use-package posframe
  :pin gnu
  ;;:quelpa (posframe :fetcher github :repo "tumashu/posframe")
  )


(use-package projectile
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))

  :custom
  (projectile-mode-line-prefix "")
  (projectile-completion-system 'default)
  (projectile-enable-caching nil)
  (projectile-tags-command "ctags -Re -f \"%s\" %s")

  :config
  ;;(projectile-mode 1)

  (setq /projectile-project-cache (make-hash-table))

  (defun /projectile-project-name ()
    (if-let ((name (gethash (current-buffer) /projectile-project-cache)))
        name
      (puthash (current-buffer) (projectile-project-name) /projectile-project-cache)))

  (defun /projectile-mode-line ()
    (if (file-remote-p default-directory)
        (format " [%s]" (/projectile-project-name))
      (format " [%s]" (projectile-project-name))))

  (setq projectile-mode-line-function '/projectile-mode-line)
  )


(use-package racket-mode
  :pin nongnu

  :config
  (defun /racket-mode-hook ()
    (paredit-mode 1)
    (racket-xp-mode 1))

  (add-hook 'racket-mode-hook #'/racket-mode-hook)

  (defun /racket-repl-mode-hook ()
    (paredit-mode 1))

  (add-hook 'racket-repl-mode-hook #'/racket-repl-mode-hook)
  )


;; Maybe deprecated in favor of colorful-mode?
(use-package rainbow-mode
  :pin gnu
  :diminish ""

  :config
  ;;(rainbow-mode 1)
  (setq rainbow-ansi-colors t)
  (setq rainbow-html-colors t)
  (setq rainbow-latex-colors t)
  )


(use-package restclient
  :pin melpa

  :config
  (setq restclient-log-request nil)
  )


;;; Translate input sequences to Latin characters even when another input method
;;; is enabled system-wide.
(use-package reverse-im
  :pin melpa

  :config
  (setq reverse-im-modifiers '(control meta super hyper))
  (setq reverse-im-input-methods '("russian-computer"))
  ;;(reverse-im-mode 1)
  )


(use-package rustic
  ;; stable version 3.4 does not use inheritenv correctly
  :pin melpa

  :custom
  (rustic-lsp-client 'eglot)

  :init
  ;; not sure why envrc/inheritenv is not automatically loaded with rustic, but
  ;; this manual load is necessary:
  (require 'inheritenv)
  )


(use-package show-font
  :pin gnu)


;;; An alternative to .dir-locals.el for configuring projects.
;; (use-package sidecar-locals
;;   :pin melpa
;;
;;   :config
;;   (add-to-list 'sidecar-locals-paths-allow "/path/to/trusted/directory")
;;   (sidecar-locals-mode 1)
;;   )


;;; Deprecated in favor of Sly. Sigh. It's been many years. Keeping here for
;;; nostalgic reasons.
;; (use-package slime
;;   :config
;;   (setq slime-net-coding-system 'utf-8-unix)
;;   (setq inferior-lisp-program "sbcl")
;;   ;; Alternatively:
;;   ;;(setq slime-lisp-implementations
;;   ;;      '((cmucl ("cmucl" "-quiet"))
;;   ;;        (sbcl ("/opt/sbcl/bin/sbcl") :coding-system utf-8-unix)))
;;
;;   (setq common-lisp-hyperspec-root "~/Files/Common Lisp/CL HyperSpec 7.0/HyperSpec")
;;   (setq common-lisp-hyperspec-symbol-table
;;         (concat common-lisp-hyperspec-root "/Data/Map_Sym.txt"))
;;
;;   (add-to-list 'slime-contribs 'slime-fancy)
;;   (add-to-list 'slime-contribs 'slime-asdf)
;;   (add-to-list 'slime-contribs 'slime-quicklisp)
;;
;;   ;;(defun /slime-mode-hook ()
;;   ;;  (setq slime-truncate-lines nil))
;;   ;;
;;   ;;(add-hook 'slime-mode-hook #'/slime-mode-hook)
;;
;;   (defun /slime-repl-mode-hook ()
;;     (paredit-mode 1)
;;     (define-key slime-repl-mode-map (kbd "<backspace>") 'paredit-backward-delete)
;;     (define-key slime-repl-mode-map (kbd "C-c C-z") 'flip-windows)
;;     (define-key slime-repl-mode-map "[" 'paredit-open-square)
;;     (define-key slime-repl-mode-map "]" 'paredit-close-square)
;;     (define-key slime-repl-mode-map "{" 'paredit-open-curly)
;;     (define-key slime-repl-mode-map "}" 'paredit-close-curly))
;;
;;   (add-hook 'slime-repl-mode-hook #'/slime-repl-mode-hook)
;;   )


(use-package sly
  :pin melpa

  :hook (sly-mrepl-mode . paredit-mode)

  :config
  ;; XXX: I want C-c C-z to flip between the REPL and the most recently edited
  ;; Lisp file, but binding a proper "go back to the last Lisp file" function to
  ;; sly-mrepl-mode-map does not work. There's something weird about the way
  ;; keymaps are applied in sly-mrepl-mode, and the sly-mode-map binding of C-c
  ;; C-z to sly-mrepl always executes. Workaround: advice that does something
  ;; different in sly-mrepl-mode.
  (defun /sly-mrepl (orig-fn &rest args)
    (if (eql 'sly-mrepl-mode major-mode)
        (let ((current-buffer (current-buffer))
              (current-persp-buffers (persp-current-buffers)))
          (cl-loop for buffer in (buffer-list)
                   when (and (not (eq buffer current-buffer))
                             (member buffer current-persp-buffers)
                             (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (derived-mode-p 'lisp-mode)))
                   do (progn
                        (pop-to-buffer buffer)
                        (cl-return))))
      (apply orig-fn args)))

  (advice-add 'sly-mrepl :around #'/sly-mrepl)
  )


;;; Deprecated in favor of newer completion systems, but keep here for reference.
;; (use-package smex                       ; smart M-x completion
;;   :config (progn
;;             (setq smex-save-file (concat user-emacs-directory ".smex-items"))
;;             ;;(global-set-key (kbd "M-x") (lambda ()
;;             ;;                              (interactive)
;;             ;;                              (or (boundp 'smex-cache)
;;             ;;                                  (smex-initialize))
;;             ;;                              (smex)))
;;             ))


;;; Package showing how to render SVG with updates, in any buffer.
;;; Example: (svg-clock-insert 50).
(use-package svg-clock
  :pin gnu)


(use-package swift-mode
  :pin melpa

  :config
  (setq swift-mode:switch-case-offset 2)

  (defun /swift-mode-hook ()
    (subword-mode)
    (local-unset-key (kbd "C-c C-z")))

  (add-hook 'swift-mode-hook #'/swift-mode-hook)
  )


(use-package terraform-mode)


(use-package typst-ts-mode
  ;; NB: :vc checkouts should be added to elpa-level .gitignore.
  :vc (typst-ts-mode :url "https://codeberg.org/meow_king/typst-ts-mode" :branch "main")
  :custom
  (typst-ts-mode-indent-offset 2)
  :config
  (add-to-list 'consult-imenu-config
               '(typst-ts-mode :toplevel "Headings" :types
                               ((?h "Headings" typst-ts-markup-header-face)
                                (?f "Functions" font-lock-function-name-face))))
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))


;;; Deprecated in favor of Eat.
;; (use-package vterm
;;   :pin melpa
;;
;;   :init
;;   ;; when the module has to be compiled, set some environment variables to help
;;   ;; find the system libvterm:
;;   (when (and (not (or (locate-library (concat "vterm-module" module-file-suffix))
;;                       (locate-library "vterm-module.so")))
;;              (file-exists-p "~/.nix-profile"))
;;     (setenv "LIB" (format "%s/.nix-profile/lib" (getenv "HOME")))
;;     (setenv "INCLUDE" (format "%s/.nix-profile/include" (getenv "HOME"))))
;;
;;   :config
;;   ;; after the module is built, make it self-contained:
;;   (let ((modfile (locate-library (concat "vterm-module" module-file-suffix))))
;;     (when (and (eq 'darwin system-type)
;;                modfile
;;                (file-exists-p modfile))
;;       (let* ((libfile "libvterm.0.dylib")
;;              (moddir (file-name-directory modfile))
;;              (libfiledir (concat moddir libfile))
;;              (default-directory moddir))
;;         (when (not (file-exists-p libfile))
;;           (let ((underlying (s-trim (shell-command-to-string (format "otool -L %s | grep libvterm | awk '{print $1}'" modfile)))))
;;             (copy-file underlying moddir t nil nil)
;;             (set-file-modes libfiledir #o755)
;;             (shell-command (format "install_name_tool -id \"%s\" \"%s\""
;;                                    libfiledir
;;                                    libfiledir))
;;             (shell-command (format "install_name_tool -change \"%s\" \"%s\" \"%s\""
;;                                    underlying
;;                                    libfiledir
;;                                    modfile)))))))
;;
;;   (setq vterm-max-scrollback 10000)
;;   (setq vterm-min-window-width 20)
;;
;;   (setq vterm-keymap-exceptions
;;         '("C-c" "C-x" "C-h" "M-x" "M-o" "C-o" "C-M-o" "C-v" "M-v" "C-y" "M-y"))
;;
;;   (defun /vterm-mode-hook ()
;;     (local-set-key (kbd "C-c C-z") 'flip-windows)
;;     (local-set-key (kbd "C-u") 'vterm--self-insert)
;;     (local-set-key (kbd "C-g") 'vterm--self-insert)
;;     (local-set-key (kbd "<prior>") 'scroll-down-command)  ; page up
;;     (local-set-key (kbd "<next>") 'scroll-up-command)     ; page down
;;     (local-set-key (kbd "C-<left>") (kbd "M-b"))
;;     (local-set-key (kbd "C-<right>") (kbd "M-f"))
;;     (local-set-key (kbd "C-<backspace>") 'vterm-send-meta-backspace)
;;     (local-set-key (kbd "C-S-d") (lambda ()
;;                                    (interactive)
;;                                    (vterm--self-insert)
;;                                    (let ((kill-buffer-query-functions nil))
;;                                      (kill-buffer)
;;                                      (delete-window))))
;;     (setq truncate-lines t))
;;
;;   (add-hook 'vterm-mode-hook #'/vterm-mode-hook)
;;   )


(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.erb\\'" . web-mode))

  :config
  (setq web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-quoting nil)

  (setq-default web-mode-comment-formats
    '(("css" . "/*")
      ("java" . "//")
      ("javascript" . "//")
      ("jsx" . "//")
      ("typescript" . "//")
      ("tsx" . "//")))

  (defun /web-mode-hook ()
    (subword-mode 1)
    (local-set-key (kbd "C-m") 'newline-and-indent))

  (add-hook 'web-mode-hook #'/web-mode-hook)
  )


(use-package wgrep)


(use-package which-key
  :diminish ""

  :init
  (which-key-mode)
  ;;(which-key-setup-side-window-bottom)
  ;;(which-key-setup-side-window-right)
  ;;(which-key-setup-side-window-right-bottom)
  (which-key-setup-minibuffer)
  )


(use-package xclip
  :pin gnu
  :if (not window-system)

  :init
  (condition-case _
      (xclip-mode 1)
    ;; handle the problem of xclip-program not being found on GUI-less systems
    (file-error nil))
  )


(use-package yaml-mode
  :hook
  (yaml-mode . /yaml-mode-hook)

  :config
  (defun /yaml-outline-level ()
    "Return the outline level based on the indentation, hardcoded at 2 spaces."
    (s-count-matches "[ ]\\{2\\}" (match-string 0)))

  (defun /yaml-mode-hook ()
    (highlight-indent-guides-mode)
    (outline-minor-mode)
    (setq outline-regexp "^\\([ ]\\{2\\}\\)*\\([-] \\)?\\([\"][^\"]*[\"]\\|[a-zA-Z0-9_-]*\\)")
    (setq outline-level '/yaml-outline-level))
  )


;;; This supersedes golden-ratio-mode.
(use-package zoom
  :pin melpa
  :diminish " φ²"

  :custom
  ;;(zoom-size '(0.618 . 0.618))
  (zoom-size #'/zoom-size)
  (zoom-ignored-major-modes '(magit-key-mode which-key-mode))
  (zoom-ignored-buffer-names '("*buffer-selection*"))

  :config
  (defun /zoom-size ()
    (cond ((> (frame-pixel-width) 1120) '(90 . 0.75))
          (t '(0.618 . 0.618))))
  )
