;;; ----------------------------------------------------------------------------
;;; various library dependencies
;;;
;;; NB: Because transitive dependencies are not pinned to the dependent's
;;; repository, order in this section matters!
;;; ----------------------------------------------------------------------------

(use-package async    :pin melpa-stable)
(use-package dash     :pin melpa-stable) ; a modern list library
(use-package ht       :pin melpa-stable) ; hash tables
(use-package s        :pin melpa-stable) ; string handling
(use-package f        :pin melpa-stable) ; file handling
(use-package queue    :pin gnu)
(use-package epl      :pin melpa-stable) ; package.el wrapper
(use-package pfuture  :pin melpa-stable)
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

            (defun /adoc-mode-hook ()
              (setq mode-name "AD")
              (setq show-trailing-whitespace t)
              (visual-line-mode))

            (add-hook 'adoc-mode-hook #'/adoc-mode-hook)

            ))


(use-package ag
  :pin melpa-stable)


(use-package avy
  :pin melpa-stable
  :config (progn
            (global-set-key (kbd "M-j") 'avy-goto-char)
            (global-set-key (kbd "C-M-j") 'avy-goto-line)
            ))


(use-package cider
  :pin melpa-stable
  :after (clojure-mode)
  :config (progn

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

            (defun cv--display-buffer-reuse-window-nil (orig-fn &rest args)
              (let ((display-buffer-overriding-action '(display-buffer-reuse-window . nil)))
                (apply orig-fn args)))

            (advice-add 'cider-switch-to-repl-buffer :around #'cv--display-buffer-reuse-window-nil)
            (advice-add 'cider-switch-to-last-clojure-buffer :around #'cv--display-buffer-reuse-window-nil)

            ))


(use-package clojure-mode
  :pin melpa-stable
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


(use-package company-lua
  :pin melpa
  :after (:all company lua-mode))


(use-package company-web
  :pin melpa-stable
  :after (:all company web-mode web-completion-data))


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


(use-package emojify
  :pin melpa-stable
  :config (progn
            ;; 'unicode currently broken on macOS
            (setq emojify-display-style 'image)
            (setq emojify-emoji-style (list 'unicode))
            (global-emojify-mode)
            ))


(use-package eshell-autojump
  :pin melpa-stable)


(use-package flycheck
  :pin melpa
  :defer t)


(use-package flycheck-rust
  :pin melpa
  :after (flycheck)
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


(use-package git-auto-commit-mode
  :pin melpa-stable
  :config (progn
            (setq gac-automatically-push-p nil)
            (setq-default gac-debounce-interval 300)
            ))


(use-package golden-ratio
  :pin melpa
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
  :pin melpa-stable
  :config (progn
            (when window-system (global-set-key (kbd "C-M-[") 'goto-last-change))
            (global-set-key (kbd "C-M-]") 'goto-last-change-reverse)
            ))


(use-package green-phosphor-theme
  :pin melpa
  :defer t)


(use-package haskell-mode
  :pin melpa-stable
  :config (progn

            (defun /haskell-mode-hook ()
              (turn-on-haskell-unicode-input-method)
              (turn-on-haskell-indentation))

            (add-hook 'haskell-mode-hook #'/haskell-mode-hook)

            ))


(add-to-list 'package-pinned-packages '(helm-core . "melpa-stable"))
(add-to-list 'package-pinned-packages '(popup . "melpa-stable"))

(use-package helm
  :pin melpa-stable
  :after (:all helm-core popup)
  :diminish ""
  :config (progn

            (require 'helm-config)

            (setq helm-buffers-fuzzy-matching t
                  helm-M-x-fuzzy-match t
                  helm-imenu-fuzzy-match t)

            (global-set-key (kbd "M-x") 'helm-M-x)
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
            (setq helm-buffer-max-length 35)

            ;; helm-mini is pretty useful
            (global-set-key (kbd "C-x M-b") 'helm-mini)
            (setq helm-mini-default-sources '(helm-source-buffers-list
                                              helm-source-projectile-recentf-list
                                              helm-source-buffer-not-found))

            ))


(use-package helm-ag
  :pin melpa-stable
  :after (helm-core))


(use-package helm-gtags
  :pin melpa-stable
  :after (helm-core)
  :config (progn

            (setq helm-gtags-path-style 'root
                  helm-gtags-ignore-case t
                  helm-gtags-use-input-at-cursor t
                  helm-gtags-display-style 'detail
                  ;;helm-gtags-auto-update t
                  helm-gtags-pulse-at-cursor t
                  helm-gtags-direct-helm-completing t
                  helm-gtags-fuzzy-match t)

            (defun /helm-gtags-mode-hook ()
              (diminish-minor-mode 'helm-gtags-mode)
              (local-set-key (kbd "C-c C-t r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "C-c C-t f") 'helm-gtags-parse-file)
              (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-,") 'helm-gtags-pop-stack)
              (local-set-key (kbd "M-*") 'helm-gtags-pop-stack))

            (add-hook 'helm-gtags-mode-hook #'/helm-gtags-mode-hook)

            ))


(use-package helm-projectile
  :pin melpa-stable
  :after (:all helm-core projectile)
  :config (progn
            (helm-projectile-on)
            ))


(use-package helm-swoop
  :pin melpa-stable
  :after (helm-core)
  :config (progn
            (global-set-key (kbd "C-M-S-i") 'helm-swoop)
            (define-key isearch-mode-map (kbd "C-M-S-i") 'helm-swoop-from-isearch)))


(use-package highlight
  :pin melpa
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


(use-package intero
  :pin melpa-stable
  :after (haskell-mode)
  :config (progn
            (intero-global-mode 1)
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
  :pin melpa-stable
  :config (progn

            (add-to-list 'display-buffer-alist '("\\*Julia\\*" (display-buffer-reuse-window display-buffer-same-window)))
            (add-to-list 'display-buffer-alist '("\\*julia\\*" (display-buffer-reuse-window display-buffer-same-window)))

            (setq julia-prompt-regexp "julia> ")

            (define-minor-mode cv-inferior-julia-mode
              "An inferior Julia mode running in an ansi-term buffer instead of comint."
              :init-value nil
              :lighter " Julia"
              :keymap (make-sparse-keymap))

            (define-key cv-inferior-julia-mode-map (kbd "<up>") 'term-send-prior)
            (define-key cv-inferior-julia-mode-map (kbd "<down>") 'term-send-next)
            (define-key cv-inferior-julia-mode-map (kbd "M-x") 'helm-M-x)
            (define-key cv-inferior-julia-mode-map (kbd "C-c C-z") 'flip-windows)
            (define-key cv-inferior-julia-mode-map (kbd "C-S-d")
              (lambda ()
                (interactive)
                (term-interrupt-subjob)
                (term-send-eof)
                (sleep-for 0 500)
                (kill-buffer)
                (delete-window)))

            (defun cv-term-julia ()
              (interactive)
              (let ((julia-buffer (get-buffer "*julia*")))
                (if julia-buffer
                    (pop-to-buffer-same-window julia-buffer)
                  (ansi-term "julia" "julia")
                  (cv-inferior-julia-mode))))

            (defun cv--term-julia-buffer ()
              (or (get-buffer "*julia*") (get-buffer "*Julia*")))

            (defun cv--term-julia-buffer-mode (buf)
              (save-excursion
                (with-current-buffer buf
                  major-mode)))

            (defun cv--term-julia-send-via-tmp-file (buf text-raw)
              (let ((text (s-trim text-raw))
                    (mode (cv--term-julia-buffer-mode buf))
                    (tmpfile (make-temp-file
                              (expand-file-name "julia-tmp"
                                                (or small-temporary-file-directory
                                                    temporary-file-directory)))))
                (unwind-protect
                    (progn
                      (with-temp-file tmpfile
                        (insert text))
                      (save-excursion
                        (with-current-buffer buf
                          (unless (equal 'term-mode mode) (end-of-buffer))
                          (insert "include(\"" tmpfile "\");")
                          (if (equal 'term-mode mode)
                              (term-send-input)
                            (comint-send-input))
                          ;; wait for the inclusion to succeed (i.e., the prompt prints)
                          (let ((sleep-total 0))
                            (while (and (< sleep-total 5000)
                                        (not (string-equal "julia>" (current-word))))
                              (sleep-for 0 20)
                              (setf sleep-total (+ sleep-total 20)))))))
                  ;; cleanup
                  (delete-file tmpfile))))

            (defun cv-julia-send-region ()
              (interactive)
              (let* ((inf-julia-buffer (cv--term-julia-buffer)))
                (when (and inf-julia-buffer (use-region-p))
                  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
                    (cv--term-julia-send-via-tmp-file inf-julia-buffer text)))))

            (defun cv-julia-send-top-level-form ()
              (interactive)
              (let* ((inf-julia-buffer (cv--term-julia-buffer))
)
                (when inf-julia-buffer
                  (let ((starting-point (point)))
                    (save-excursion
                      (beginning-of-line)
                      (unless (= 1 starting-point) (left-char))
                      (let* ((ending-raw (re-search-forward "\nend\n\n" nil t))
                             ;; subtract two trailing newlines
                             (ending (- (or ending-raw 0) 2))
                             (beginning-raw (re-search-backward
                                             (concat "\\(\nfunction\\)\\|"
                                                     "\\(\ntype\\)")))
                             ;; add one leading newline
                             (beginning (+ beginning-raw 1)))
                        (if (or (not (and ending-raw beginning-raw))
                                (< starting-point beginning)
                                (> starting-point ending))
                            (message "no suitable top-level form found")
                          (let ((text (buffer-substring-no-properties beginning ending)))
                            (cv--term-julia-send-via-tmp-file inf-julia-buffer text)))))))))

            (defun cv-julia-send-buffer ()
              (interactive)
              (let* ((filename buffer-file-name)
                     (inf-julia-buffer (cv--term-julia-buffer))
                     (mode (cv--term-julia-buffer-mode inf-julia-buffer)))
                (when inf-julia-buffer
                  (save-excursion
                    (with-current-buffer inf-julia-buffer
                      (unless (equal 'term-mode mode) (end-of-buffer))
                      (insert "include(\"" filename "\");")
                      (if (equal 'term-mode mode)
                          (term-send-input)
                        (comint-send-input)))))))

            (defun /julia-mode-hook ()
              (subword-mode)
              (local-set-key (kbd "C-c C-z") 'cv-term-julia)
              (local-set-key (kbd "C-c C-c") 'cv-julia-send-top-level-form)
              (local-set-key (kbd "C-M-x") 'cv-julia-send-region)
              (local-set-key (kbd "C-c C-k") 'cv-julia-send-buffer))

            (add-hook 'julia-mode-hook #'/julia-mode-hook)

            (defun /inferior-julia-mode-hook ()
              (local-set-key (kbd "C-a") 'comint-bol)
              (local-set-key (kbd "M-p") 'comint-previous-matching-input-from-input)
              (local-set-key (kbd "M-n") 'comint-next-matching-input-from-input)
              (local-set-key (kbd "<up>") 'comint-previous-input)
              (local-set-key (kbd "<down>") 'comint-next-input)
              (local-set-key (kbd "C-r") 'helm-comint-input-ring)
              (local-set-key (kbd "C-c C-z") 'flip-windows)
              (local-set-key (kbd "C-S-d") (lambda ()
                                             (interactive)
                                             (comint-interrupt-subjob)
                                             (end-of-buffer)
                                             (insert "quit()")
                                             (comint-send-input)
                                             (sleep-for 0 100)
                                             (kill-buffer)
                                             (delete-window))))

            (add-hook 'inferior-julia-mode-hook #'/inferior-julia-mode-hook)

            ))


(use-package ledger-mode
  :pin melpa-stable
  :config (progn

            (add-to-list 'auto-mode-alist '("\\.dat$" . ledger-mode))

            (defun /ledger-mode-hook ()
              (local-set-key (kbd "C-c C-f") 'flush-to-fill-column))

            (add-hook 'ledger-mode-hook #'/ledger-mode-hook)

            ))


(use-package leuven-theme
  :pin melpa
  :defer t)


(use-package lua-mode
  :pin melpa
  :config (progn

            (defun /lua-mode-hook ()
              (subword-mode)
              (setq show-trailing-whitespace t))

            (add-hook 'lua-mode-hook #'/lua-mode-hook)

            ))


(add-to-list 'package-pinned-packages '(ghub . "melpa-stable"))
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

            (defun /magit-status-mode-hook ()
              (define-key magit-status-mode-map (kbd "W")
                (lambda ()
                  (interactive)
                  (if (member "-w" magit-diff-options)
                      (setq magit-diff-options (remove "-w" magit-diff-options))
                    (add-to-list 'magit-diff-options "-w"))
                  (magit-refresh))))

            (add-hook 'magit-status-mode-hook #'/magit-status-mode-hook)

            (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
            (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

            ))


(use-package markdown-mode
  :pin melpa-stable
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
  :pin melpa-stable
  :defer t)


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
            (add-to-list 'term-bind-key-alist '("C-M-o" . ace-window))
            (add-to-list 'term-bind-key-alist '("C-c C-z" . flip-windows))
            ))


;; turn on when built-in Org dependency is up-to-date
;;(use-package ob-async
;;  :pin melpa-stable)


(use-package ob-restclient
  :pin melpa)


(use-package olivetti                   ; focused writing mode
  :pin melpa-stable
  :config (progn

            (defun /olivetti-mode-hook ()
              (setq right-fringe-width 0
                    left-fringe-width 0)
              (set-window-buffer (frame-selected-window) (current-buffer))
              (olivetti-set-width 90))

            (add-hook 'olivetti-mode-hook #'/olivetti-mode-hook)

            ))


(use-package origami
  :pin melpa
  :after (hydra)
  :config (progn

            (setq origami-show-fold-header t)

            (defhydra cv--hydra-origami (:color red)
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

            (define-key origami-mode-map (kbd "C-h M-o") 'cv--hydra-origami/body)

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
  :pin melpa-stable
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
              (let ((live-buffers (-reject #'(lambda (b) (null (buffer-name b))) (persp-buffers (persp-curr)))))
                (setf (persp-buffers (persp-curr)) live-buffers)))

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

            (defun cv--persp-rename (orig-fn &rest args)
              (let* ((new-name (car args))
                     (old-name (persp-name (persp-curr)))
                     (scratch-buf (get-buffer (format "*scratch* (%s)" old-name)))
                     (new-scratch-name (format "*scratch* (%s)" new-name)))
                (apply orig-fn args)
                (when scratch-buf
                  (with-current-buffer scratch-buf
                    (rename-buffer new-scratch-name)))))

            (advice-add 'persp-rename :around #'cv--persp-rename)

            ))


(use-package projectile
  :pin melpa-stable
  :config (progn

            (projectile-global-mode)
            (setq projectile-enable-caching nil)
            (setq projectile-tags-command "ctags -Re -f \"%s\" %s")

            (setq cv--projectile-project-cache (make-hash-table))

            (defun cv--projectile-project-name ()
              (if-let ((name (gethash (current-buffer) cv--projectile-project-cache)))
                  name
                (puthash (current-buffer) (projectile-project-name) cv--projectile-project-cache)))

            (setq-default projectile-mode-line '(:eval (if (file-remote-p default-directory)
                                                           (format " [%s]" (cv--projectile-project-name))
                                                         (format " [%s]" (projectile-project-name)))))

            ))


(use-package restclient
  :pin melpa
  :config (progn
            (setq restclient-log-request nil)
            ))


(use-package restclient-helm
  :pin melpa
  :after (restclient))


(use-package rust-mode
  :pin melpa
  :config (progn

            (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

            (defun /rust-mode-hook ()
              (flycheck-mode))

            (add-hook 'rust-mode-hook #'/rust-mode-hook)

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
  :pin melpa-stable
  :after (:all company slime)
  :config (progn
            ;; XXX: This is here instead of the slime :config section to make
            ;; sure it runs after the slime-company is available.
            (slime-setup '(slime-fancy slime-company))
            ))


(use-package smex                       ; smart M-x completion
  :pin melpa-stable
  :config (progn
            (setq smex-save-file (concat user-emacs-directory ".smex-items"))
            ;;(global-set-key (kbd "M-x") (lambda ()
            ;;                              (interactive)
            ;;                              (or (boundp 'smex-cache)
            ;;                                  (smex-initialize))
            ;;                              (smex)))
            ))


(use-package solarized-theme
  :pin melpa-stable
  :defer t)


(use-package swift-mode
  :pin melpa
  :config (progn

            (defun /swift-mode-hook ()
              (local-unset-key (kbd "C-c C-z")))

            (add-hook 'swift-mode-hook #'/swift-mode-hook)

            ))


(use-package swiper                     ; Ivy / Swiper / Counsel
  :pin melpa
  :config (progn
            (global-set-key (kbd "M-S-C-s") 'swiper)
            ))


(use-package treemacs
  :pin melpa-stable
  :config (progn

            (setq treemacs-persist-file (expand-file-name "treemacs/treemacs-persist" user-emacs-directory))
            (setq treemacs-no-png-images t)

            (defun /treemacs-mode-hook ()
              (define-key treemacs-mode-map (kbd "<S-return>") #'treemacs-visit-node-ace))

            (add-hook 'treemacs-mode-hook #'/treemacs-mode-hook)

            ))


(use-package tide                       ; TypeScript IDE
  :pin melpa-stable
  :after (:all company flycheck typescript-mode)
  :diminish " Tide"
  :config (progn

            (setq tide-node-executable "~/.nvm/versions/node/v8.10.0/bin/node")

            ))


;;; Installed purely because it's a tide dependency. web-mode provides superior
;;; TS indentation.
(use-package typescript-mode
  :pin melpa-stable
  :diminish " TS"
  :config (progn

            (setq auto-mode-alist (delete '("\\.ts$" . typescript-mode) auto-mode-alist))

            ))


(use-package web-mode
  :pin melpa-stable
  :after (:all flycheck tide)
  :config (progn

            (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.ts?\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

            (setq web-mode-enable-current-element-highlight t
                  web-mode-markup-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-enable-auto-quoting nil)

            (defun /web-mode-hook ()
              (when (-contains? '("ts" "tsx") (file-name-extension buffer-file-name))
                (tide-setup)
                (flycheck-mode))
              (local-set-key (kbd "C-m") 'newline-and-indent))

            (add-hook 'web-mode-hook #'/web-mode-hook)

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


(use-package yaml-mode
  :pin melpa-stable)


(use-package zenburn-theme
  :pin melpa-stable
  :defer t)
