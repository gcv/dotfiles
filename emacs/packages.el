;;; ----------------------------------------------------------------------------
;;; various library dependencies
;;;
;;; NB: Because transitive dependencies are not pinned to the dependent's
;;; repository, order in this section matters!
;;; ----------------------------------------------------------------------------

(use-package a)                          ; Clojure-style associative data
(use-package async)
(use-package dash)                       ; a modern list library
(use-package dash-functional)
(use-package f)                          ; file handling
(use-package ht)                         ; hash tables
(use-package parsec :pin melpa)          ; parser generator
(use-package pfuture)
(use-package queue :pin gnu)
(use-package s)                          ; string handling


;;; ----------------------------------------------------------------------------
;;; real packages
;;; ----------------------------------------------------------------------------

(use-package ace-window
  :bind ("C-M-o" . ace-window)
  :config (progn
            (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m))
            ))


(use-package adoc-mode
  :mode "\\.adoc$"
  :config (progn

            (defun /adoc-mode-hook ()
              (setq mode-name "AD")
              (setq show-trailing-whitespace t)
              (visual-line-mode))

            (add-hook 'adoc-mode-hook #'/adoc-mode-hook)

            ))


(use-package ag
  :defer t)


(use-package avy
  :bind (("M-j" . avy-goto-char)
         ("C-M-j" . avy-goto-line)))


;; configuration bisect utility
(use-package bug-hunter
  :pin "gnu")


(use-package cider
  :defer t
  :config (progn

            (add-to-list 'display-buffer-alist
                         '("\\*cider-repl .*"
                           (display-buffer-reuse-window display-buffer-in-side-window)
                           (reusable-frames . visible)
                           (side . bottom)
                           (window-height . 0.2)))

            (setq cider-show-error-buffer t
                  cider-prompt-for-symbol nil
                  cider-repl-tab-command 'indent-for-tab-command
                  cider-repl-display-help-banner nil
                  cider-repl-history-file (concat user-emacs-directory "nrepl-history")
                  cider-mode-line '(:eval (format " cider[%s]" (cider-current-ns))))

            (defun /cider-mode-hook ()
              (paredit-mode 1)
              (define-key cider-mode-map (kbd "C-c C-,") #'cider-test-run-tests)
              (define-key cider-mode-map (kbd "C-c C-t") nil)
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

            ))


(use-package clojure-mode
  :defer t
  :config (progn

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

            ))


(use-package cmake-mode
  :defer t
  :config (progn
            (setq cmake-tab-width 4)
            ))


(use-package company                    ; "comp"lete "any"thing
  :bind ("C-." . company-complete)
  :diminish ""
  :config (progn

            (global-company-mode)
            (setq company-idle-delay nil)

            (let ((company-backends (list 'company-slime
                                          'company-lua
                                          'company-web-html)))
              (dolist (backend company-backends)
                (when (functionp (lambda backend))
                  (add-to-list 'company-backends backend))))

            ))


(use-package company-lua
  :pin melpa
  :defer t)


(use-package company-terraform
  :defer t)


(use-package company-web
  :defer t)


(use-package deft
  :bind ("C-c d" . deft)
  :config (progn

            (setq deft-extensions '( "org" "note" "txt" "tex")
                  deft-directory "~/Notes/"
                  deft-recursive t
                  deft-text-mode 'org-mode)

            (defun /deft-current-window-width (orig-fn &rest args)
              (let ((res (apply orig-fn args)))
                (- res 1)))

            (advice-add 'deft-current-window-width :around #'/deft-current-window-width)

            (defun /deft-complete ()
              (kill-buffer deft-buffer))

            (advice-add 'deft-complete :after #'/deft-complete)

            ))


(use-package disk-usage
  :pin gnu
  :defer t
  :config (progn
            (setq disk-usage--du-command "du")
            ))


(use-package direnv
  :defer t
  :config (progn
            ;; It's faster to keep this minor mode disabled, and use
            ;; direnv-update-environment manually as needed.
            ;;(direnv-mode 1)
            ))


(use-package discover-my-major
  :pin melpa
  :bind ("C-h C-m" . discover-my-major))


(use-package doom-themes
  :pin melpa
  :defer t)


(use-package dumb-jump
  :bind (("H-." . dumb-jump-go)
         ("H-," . dumb-jump-back)
         ("H->" . dumb-jump-quick-look))
  :config (progn
            (setq dumb-jump-selector 'popup)
            ))


(use-package elisp-slime-nav
  :defer t
  :diminish "")


(use-package emojify
  :pin melpa
  :defer t
  :config (progn
            ;; 'unicode possibly broken on NextStep port?
            (setq emojify-display-style 'image)
            (setq emojify-emoji-style (list 'unicode))
            ;; NB: This mode does not seem to be needed for the Mac port.
            ;;(global-emojify-mode)
            ))


(use-package eshell-autojump
  :defer t)


(use-package expand-region
  :bind ("C-S-SPC" . er/expand-region))


(use-package flycheck
  :pin melpa
  :defer t)


(use-package flycheck-rust
  :pin melpa
  :defer t)


(use-package fountain-mode              ; screenwriting
  :mode "\\.fountain$")


(use-package fringe-helper
  :pin melpa
  :config (progn

            (when (or (eq window-system 'ns) (eq window-system 'mac))
              (set-fringe-mode '(0 . 8)))

            (define-fringe-bitmap '/fringe-backslash (fringe-helper-convert
                                                      "........"
                                                      ".X......"
                                                      ".XX....."
                                                      "..XX...."
                                                      "...XX..."
                                                      "....XX.."
                                                      ".....X.."
                                                      "........"))

            (setcdr (assq 'continuation fringe-indicator-alist) '(nil /fringe-backslash))
            ;;(setq visual-line-fringe-indicators '(nil /fringe-backslash))

            ))


(use-package gif-screencast
  :pin melpa
  :defer t
  ;;:bind ("H-s" . /gif-screencast)
  :config (progn

            ;; required on Mac
            (setq gif-screencast-args '("-x")
                  gif-screencast-cropping-program "mogrify"
                  gif-screencast-capture-format "ppm")

            ;; sanity
            (setq gif-screencast-want-optimized t
                  gif-screencast-autoremove-screenshots t)

            (defun /gif-screencast ()
              (interactive)
              (if gif-screencast-mode
                  (gif-screencast-stop)
                (if (not (y-or-n-p "Start recording a screencast? "))
                    (message "Canceled")
                  (gif-screencast))))

            ;; use this redefinition on a high-DPI screen:
            (defun gif-screencast--cropping-region ()
              (let ((x (* 2 (car (frame-position))))
                    (y (* 2 (cdr (frame-position))))
                    (width (* 2 (car (alist-get 'outer-size (frame-geometry)))))
                    (height (* 2 (cdr (alist-get 'outer-size (frame-geometry))))))
                (format "%dx%d+%d+%d" width height x y)))

            ))


(use-package git-auto-commit-mode
  :pin melpa
  :defer t
  :config (progn
            (setq gac-automatically-push-p nil)
            (setq-default gac-debounce-interval 300)
            ))


(use-package golden-ratio
  :pin melpa
  :defer t
  :diminish " φ"
  :config (progn

            ;;(setq golden-ratio-auto-scale t)

            (add-to-list 'golden-ratio-extra-commands 'mouse-set-point)
            (add-to-list 'golden-ratio-extra-commands 'flip-windows)
            (add-to-list 'golden-ratio-extra-commands 'switch-to-last-terminal-buffer)
            (add-to-list 'golden-ratio-extra-commands 'ace-window)
            (add-to-list 'golden-ratio-extra-commands 'avy-goto-char)
            (add-to-list 'golden-ratio-extra-commands 'avy-goto-line)
            (add-to-list 'golden-ratio-exclude-modes 'magit-key-mode)
            (add-to-list 'golden-ratio-exclude-modes 'which-key-mode)
            (add-to-list 'golden-ratio-exclude-buffer-names "*buffer-selection*")

            (defun toggle-golden-ratio ()
              (interactive)
              (golden-ratio-mode (if golden-ratio-mode 0 1))
              (when golden-ratio-mode (golden-ratio)))

            ))


;; http://www.emacswiki.org/emacs/GotoChg
(use-package goto-chg
  :bind (("C-M-[" . goto-last-change)
         ("C-M-]" . goto-last-change-reverse)))


(use-package green-phosphor-theme
  :pin melpa
  :defer t)


(use-package haskell-mode
  :defer t
  :config (progn

            ;; consider installing Intero if Haskell becomes relevant

            (defun /haskell-mode-hook ()
              (turn-on-haskell-unicode-input-method)
              (turn-on-haskell-indentation))

            (add-hook 'haskell-mode-hook #'/haskell-mode-hook)

            ))


;;; Hashicorp Configuration Language: dependency for terraform-mode
(use-package hcl-mode
  :defer t)


;; Replacement for built-in Help system.
(use-package helpful
  :pin melpa
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h o" . helpful-symbol)
         ("C-h k" . helpful-key)
         ("C-c C-d h" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command))
  :config (progn
            (when (featurep 'counsel)
              (setq counsel-describe-function-function #'helpful-callable)
              (setq counsel-describe-variable-function #'helpful-variable))))


(use-package highlight
  :pin melpa
  :bind (("C-c H h" . hlt-highlight)
         ("C-c H u" . hlt-unhighlight-region)
         ("C-c H U" . /hlt-remove-all-highlights-in-buffer))
  :config (progn

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

            ))


(use-package hydra
  :defer t)


(use-package iedit
  :defer nil
  :init (progn
          (setq iedit-toggle-key-default nil)
          )
  :config (progn
            ;; manually set keys, because iedit overrides some important globals:
            (let ((iedit-toggle-keybinding (kbd "C-;")))
              (define-key global-map iedit-toggle-keybinding 'iedit-mode)
              (define-key isearch-mode-map iedit-toggle-keybinding 'iedit-mode-from-isearch)
              (define-key help-map iedit-toggle-keybinding 'iedit-mode-toggle-on-function))
            ))


(use-package js2-mode
  :pin melpa
  :mode (("\\.js$" . js2-mode)
         ("\\.jsx$" . js2-jsx-mode))
  :config (progn

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

            ))


(use-package julia-mode
  :pin melpa
  :defer t
  :config (progn

            (setq julia-indent-offset 3)

            (setq julia-prompt-regexp "julia> ")

            (defun /julia-mode-hook ()
              (subword-mode)
              (setq show-trailing-whitespace t))

            (add-hook 'julia-mode-hook #'/julia-mode-hook)

            ))


(use-package julia-snail
  ;;:load-path "~/Code/julia-snail"
  :pin melpa
  :hook (julia-mode . julia-snail-mode))


(use-package ledger-mode
  :mode "\\.dat$"
  :config (progn

            (defun /ledger-mode-hook ()
              (local-set-key (kbd "C-c C-f") 'flush-to-fill-column))

            (add-hook 'ledger-mode-hook #'/ledger-mode-hook)

            ))


(use-package leuven-theme
  :pin melpa
  :defer t)


(use-package lua-mode
  :pin melpa
  :mode "\\.lua$"
  :config (progn

            (defun /lua-mode-hook ()
              (subword-mode)
              (setq show-trailing-whitespace t))

            (add-hook 'lua-mode-hook #'/lua-mode-hook)

            ))


(use-package magit
  :defer t
  :config (progn

            (diminish 'magit-auto-revert-mode)

            (setq magit-commit-show-diff nil)
            (setq magit-diff-refine-hunk t) ; "all" shows word differences in all hunks
            ;;(setq auto-revert-check-vc-info t)

            (magit-define-popup-switch 'magit-log-popup ?f "Follow renames" "--follow")

            (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
            (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

            (defun /magit-status-mode-hook ()
              ;; Restore window configuration and clean up all Magit buffers.
              ;; The default binding for "q" is #'magit-restore-window-configuration.
              (local-set-key "q" (lambda ()
                                   (interactive)
                                   (let ((buffers (magit-mode-get-buffers)))
                                     (magit-restore-window-configuration)
                                     (mapc #'kill-buffer buffers)))))

            (add-hook 'magit-status-mode-hook #'/magit-status-mode-hook)

            ))


(use-package markdown-mode
  :defer t
  :config (progn

            (add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
            (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

            (defun /markdown-mode-hook ()
              (setq mode-name "MD")
              (local-unset-key (kbd "M-<up>"))
              (local-unset-key (kbd "M-<down>"))
              (setq show-trailing-whitespace t)
              (visual-line-mode))

            (add-hook 'markdown-mode-hook #'/markdown-mode-hook)

            ))


(use-package material-theme
  :defer t)


(use-package modus-operandi-theme
  :defer t)


(use-package modus-vivendi-theme
  :defer t)


(use-package multiple-cursors
  :bind (("C-?" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))


(use-package nix-mode
  :mode "\\.nix$")


;; turn on when built-in Org dependency is up-to-date
;;(use-package ob-async)


(use-package ob-restclient
  :pin melpa
  :defer t)


(use-package olivetti                   ; focused writing mode
  :defer t
  :config (progn

            (setq-default olivetti-body-width 90)

            (defun olivetti-reset-width ()
              (interactive)
              (when (and (-contains? minor-mode-list 'olivetti-mode)
                         (symbol-value 'olivetti-mode))
                (let ((orig-width olivetti-body-width))
                  (olivetti-set-width 1.0)
                  (olivetti-set-width orig-width))
                (message "olivetti width reset")))

            (defun /olivetti-mode-hook ()
              (setq right-fringe-width 0
                    left-fringe-width 0)
              (set-window-buffer (frame-selected-window) (current-buffer)))

            (add-hook 'olivetti-mode-hook #'/olivetti-mode-hook)

            (defun /olivetti-mode-reset (&optional arg)
              (if (symbol-value 'olivetti-mode)
                  (olivetti-set-width (default-value 'olivetti-body-width))
                (olivetti-set-width 1.0)))

            (advice-add 'olivetti-mode :after #'/olivetti-mode-reset)

            (defhydra /hydra-olivetti (:color red)
              "
  _r_eset    _s_et width    _<left>_ shrink    _<right>_ expand   _<return>_ exit
  "
              ("r" olivetti-reset-width)
              ("s" olivetti-set-width)
              ("<left>" olivetti-shrink)
              ("<right>" olivetti-expand)
              ("<return>" keyboard-quit :color blue)
              )

            (define-key olivetti-mode-map (kbd "H-o") '/hydra-olivetti/body)

            ))


(use-package origami
  :pin melpa
  :after (hydra)
  :bind (:map origami-mode-map
         ("H-o" . /hydra-origami/body))
  :config (progn

            (setq origami-show-fold-header t)

            (defhydra /hydra-origami (:color red)
              "
  _o_pen node   _O_pen node rec    toggle _f_orward  _u_ndo
  _c_lose node  _C_lose node rec   toggle _a_ll      _r_edo
  _n_ext fold   _p_revious fold    _<tab>_ smart     _R_eset
  "
              ("<up>" previous-line)
              ("<down>" next-line)
              ("<left>" left-char)
              ("<right>" right-char)
              ("<tab>" origami-recursively-toggle-node)
              ("S-<tab>" origami-show-only-node)
              ("o" origami-open-node)
              ("O" origami-open-node-recursively)
              ("c" origami-close-node)
              ("C" origami-close-node-recursively)
              ("n" origami-next-fold)
              ("p" origami-previous-fold)
              ("f" origami-forward-toggle-node)
              ("a" origami-toggle-all-nodes)
              ("u" origami-undo)
              ("r" origami-redo)
              ("R" origami-reset)
              )

            ))


(use-package package-lint
  :pin melpa
  :defer t)


(use-package paredit
  :defer t
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

            (defun /paredit-minibuffer-setup-hook ()
              (when (memq this-command '(eval-expression pp-eval-expression))
                (paredit-mode 1)))

            (add-hook 'minibuffer-setup-hook #'/paredit-minibuffer-setup-hook)

            ))


(use-package perspective
  ;;:load-path "~/Code/perspective-el"
  :pin melpa
  :defer nil
  :bind (("C-c C-p" . perspective-map)
         ("H-p" . perspective-map)
         ("C-c M-p" . persp-switch)
         ("C-x C-M-b" . persp-ivy-switch-buffer))
  :config (progn

            (persp-mode)
            (persp-turn-off-modestring)

            (setq persp-interactive-completion-function 'ido-completing-read
                  persp-sort 'access)

            (setq persp-state-default-file (concat user-emacs-directory "persp-state.el"))
            ;; useful, but I prefer a prompt on exit
            ;;(add-hook 'kill-emacs-hook #'persp-state-save)

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

            ))


;;; Allows multiple major modes using indirect buffers. Needs a plugin which
;;; defines. As of this writing: poly-org and poly-noweb have problems.
;;; poly-markdown is pretty good.
(use-package polymode
  :defer t)
(use-package poly-markdown
  :defer t)


(use-package poporg
  :pin melpa
  :defer t)


(use-package prescient
  :defer t
  :config (progn
            ;;(setq prescient-aggressive-file-save t)
            (setq prescient-filter-method '(literal regexp initialism fuzzy))
            (prescient-persist-mode 1)))


(use-package projectile
  :defer nil                            ; must load eagerly
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :init (progn
          (setq projectile-mode-line-prefix "")
          )
  :config (progn

            (projectile-mode 1)

            (setq projectile-completion-system 'default)

            (setq projectile-enable-caching nil)
            (setq projectile-tags-command "ctags -Re -f \"%s\" %s")

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

            ))


(use-package rainbow-mode
  :pin gnu
  :defer t
  :diminish ""
  :config (progn
            (rainbow-mode 1)
            (setq rainbow-ansi-colors t)
            (setq rainbow-html-colors t)
            (setq rainbow-latex-colors t)
            ))


(use-package restclient
  :pin melpa
  :defer t
  :config (progn
            (setq restclient-log-request nil)
            ))


;;; Translate input sequences to Latin characters even when another input method
;;; is enabled system-wide.
(use-package reverse-im
  :pin melpa
  :defer t
  :config (progn
            (setq reverse-im-modifiers '(control meta super hyper))
            (setq reverse-im-input-methods '("russian-computer"))
            ;;(reverse-im-mode 1)
            ))


(use-package rust-mode
  :pin melpa
  :mode "\\.rs\\'"
  :config (progn

            (defun /rust-mode-hook ()
              (flycheck-mode))

            (add-hook 'rust-mode-hook #'/rust-mode-hook)

            ))


(use-package slime
  :defer t
  :config (progn

            (setq slime-net-coding-system 'utf-8-unix)
            (setq inferior-lisp-program "sbcl")

            (setq common-lisp-hyperspec-root "~/Files/Common Lisp/CL HyperSpec 7.0/HyperSpec")
            (setq common-lisp-hyperspec-symbol-table
                  (concat common-lisp-hyperspec-root "/Data/Map_Sym.txt"))

            ;;(defun /slime-mode-hook ()
            ;;  (setq slime-truncate-lines nil))
            ;;
            ;;(add-hook 'slime-mode-hook #'/slime-mode-hook)

            ;;(defun /slime-repl-mode-hook ()
            ;;  (paredit-mode 1)
            ;;  (define-key slime-repl-mode-map "[" 'paredit-open-square)
            ;;  (define-key slime-repl-mode-map "]" 'paredit-close-square)
            ;;  (define-key slime-repl-mode-map "{" 'paredit-open-curly)
            ;;  (define-key slime-repl-mode-map "}" 'paredit-close-curly))
            ;;
            ;;(add-hook 'slime-repl-mode-hook #'/slime-repl-mode-hook)

            ))


(use-package slime-company
  :defer t
  :config (progn
            ;; XXX: This is here instead of the slime :config section to make
            ;; sure it runs after the slime-company is available.
            (slime-setup '(slime-fancy slime-company))
            ))


;;; Deprecated in favor of Helm and Ivy, but keep here for reference.
;; (use-package smex                       ; smart M-x completion
;;   :config (progn
;;             (setq smex-save-file (concat user-emacs-directory ".smex-items"))
;;             ;;(global-set-key (kbd "M-x") (lambda ()
;;             ;;                              (interactive)
;;             ;;                              (or (boundp 'smex-cache)
;;             ;;                                  (smex-initialize))
;;             ;;                              (smex)))
;;             ))


(use-package solarized-theme
  :defer t)


(use-package swift-mode
  :pin melpa
  :defer t
  :config (progn

            (setq swift-mode:switch-case-offset 2)

            (defun /swift-mode-hook ()
              (subword-mode)
              (local-unset-key (kbd "C-c C-z")))

            (add-hook 'swift-mode-hook #'/swift-mode-hook)

            ))


(use-package terraform-mode
  :defer t)


(use-package treemacs
  :defer t
  :config (progn

            (setq treemacs-persist-file (expand-file-name "treemacs/treemacs-persist" user-emacs-directory))
            (setq treemacs-no-png-images t)

            (defun /treemacs-mode-hook ()
              (define-key treemacs-mode-map (kbd "M-x")
                (lambda ()
                  (interactive)
                  (let ((ivy-posframe-display-functions-alist '((counsel-M-x . ivy-posframe-display-at-frame-center))))
                    (counsel-M-x))))
              (define-key treemacs-mode-map (kbd "<S-return>") #'treemacs-visit-node-ace))

            (add-hook 'treemacs-mode-hook #'/treemacs-mode-hook)

            ))


(use-package tide                       ; TypeScript IDE
  :defer t
  :diminish " Tide"
  :config (progn
            (setq tide-tsserver-start-method 'manual)
            ))


;;; Installed purely because it's a tide dependency. web-mode provides superior
;;; TS indentation.
(use-package typescript-mode
  :defer t
  :diminish " TS"
  :config (progn
            (setq auto-mode-alist (delete '("\\.ts$" . typescript-mode) auto-mode-alist))
            ))


(use-package undo-fu
  :pin melpa
  :bind (("C-z" . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))


(use-package vterm
  :pin melpa
  :defer t
  :config (progn

            (setq vterm-max-scrollback 10000)

            (setq vterm-keymap-exceptions
                  '("C-c" "C-x" "C-h" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y"))

            (defun /vterm-mode-hook ()
              (local-set-key (kbd "C-c C-z") 'flip-windows)
              (local-set-key (kbd "C-u") 'vterm--self-insert)
              (local-set-key (kbd "C-g") 'vterm--self-insert)
              (local-set-key (kbd "<prior>") 'scroll-down-command)  ; page up
              (local-set-key (kbd "<next>") 'scroll-up-command)     ; page down
              (local-set-key (kbd "C-<left>") (kbd "M-b"))
              (local-set-key (kbd "C-<right>") (kbd "M-f"))
              (local-set-key (kbd "C-<backspace>") 'vterm-send-meta-backspace)
              (local-set-key (kbd "C-S-d") (lambda ()
                                             (interactive)
                                             (vterm--self-insert)
                                             (let ((kill-buffer-query-functions nil))
                                               (kill-buffer)
                                               (delete-window))))
              (setq truncate-lines t))

            (add-hook 'vterm-mode-hook #'/vterm-mode-hook)

            ))


(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.ts?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :config (progn

            (setq web-mode-enable-current-element-highlight t
                  web-mode-markup-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-enable-auto-quoting nil)

            (setq-default web-mode-comment-formats
              '(("css" . "/*")
                ("java" . "//")
                ("javascript" . "//")))

            (defun /web-mode-hook ()
              (when (-contains? '("ts" "tsx") (file-name-extension buffer-file-name))
                (tide-setup)
                (flycheck-mode))
              (local-set-key (kbd "C-m") 'newline-and-indent))

            (add-hook 'web-mode-hook #'/web-mode-hook)

            ))


(use-package wgrep
  :defer t)


(use-package which-key
  :defer nil
  :diminish ""
  :config (progn
            (which-key-mode)
            ;;(which-key-setup-side-window-bottom)
            ;;(which-key-setup-side-window-right)
            ;;(which-key-setup-side-window-right-bottom)
            (which-key-setup-minibuffer)
            ))


(use-package yaml-mode
  :defer t)


(use-package zenburn-theme
  :defer t)
