(require 'cl)
(require 'subr-x)

(setq load-prefer-newer t)                                      ; deal with outdated .elc files
(setq-default indent-tabs-mode nil)                             ; replace tabs with spaces
(transient-mark-mode -1)                                        ; turn off transient-mark mode
(setq-default fill-column 80)                                   ; 80-character screens, not 70
(setq sentence-end-double-space nil)                            ; one space between sentences
(show-paren-mode t)                                             ; show matching parens
(setq parens-require-spaces nil)                                ; m-( should not insert spaces
(setq scroll-step 1)                                            ; stop scrolling by half-screens
(column-number-mode t)                                          ; show column numbers
(setq require-final-newline t)                                  ; newline required at end of file
(fset 'yes-or-no-p 'y-or-n-p)                                   ; substitute y-or-n for yes-or-no
;;(setq make-backup-files nil)                                  ; supress backup "~" files
(when window-system (tool-bar-mode -1))                         ; remove the toolbar
(unless window-system (menu-bar-mode -1))                       ; remove the menu bar in terminal
(blink-cursor-mode -1)                                          ; traditional steady cursor
;;(setq-default show-trailing-whitespace t)                     ; always show trailing whitespace
(setq font-lock-verbose nil)                                    ; silence slow compile messages
(setq inhibit-startup-screen t)                                 ; turn off the splash screen
(setq initial-scratch-message nil)                              ; nothing in *scratch*
(global-auto-revert-mode 1)                                     ; track externally changed files
;;(setq global-auto-revert-non-file-buffers t)                  ; not ready for kqueue+directories
;;(setq auto-revert-use-notify nil)                             ; not ready for kqueue+directories
;;(global-hl-line-mode 1)                                       ; highlight the current line
(setq ns-antialias-text t)                                      ; anti-aliased fonts on Mac OS
(setq ns-pop-up-frames nil)                                     ; don't open files in new frame
(setq ns-command-modifier 'meta)                                ; fix Cmd as Meta on Mac OS
(setq ns-alternate-modifier 'none)                              ; leave Alt alone
(when window-system (scroll-bar-mode -1))                       ; no scrollbars (bugs on Mac OS)
(setq truncate-partial-width-windows nil)                       ; no more truncated lines
(setq woman-use-own-frame nil)                                  ; stop opening useless frames
(setq woman-fill-frame t)                                       ; frames display >72 characters
(setq save-abbrevs nil)                                         ; don't use abbrev-mode
(setq save-interprogram-paste-before-kill t)                    ; don't lose clipboard on kill
(setq tags-revert-without-query t)                              ; minimize TAGS use annoyance

(setq custom-file (concat user-emacs-directory "custom.el"))    ; customize: don't touch init.el
(load custom-file 'noerror)                                     ; customize: load customizations


;; security
(setq network-security-level 'high)
(setq tls-checktrust t)


;; control beeping
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort
                        abort-recursive-edit
                        exit-minibuffer
                        keyboard-quit))
          (message "*beep*"))))


;; Unicode and UTF-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;;(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)


;; enable useful commands
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


;; history
(savehist-mode 1)
(setq history-length 250
      history-delete-duplicates t)
(put 'file-name-history 'history-length 500)
(add-to-list 'savehist-additional-variables 'load-project-history)


;; Emacs server
(unless (file-exists-p (concat (getenv "TMPDIR") "emacs"
                               (number-to-string
                                (user-real-uid)) "/server"))
  (server-start))


;; exit safety
(when window-system
  (global-set-key (kbd "C-x C-c") (lambda ()
                                    (interactive)
                                    (if (y-or-n-p "Are you sure you want to exit Emacs? ")
                                        (save-buffers-kill-emacs)
                                      (message "Canceled exit")))))



;;; ----------------------------------------------------------------------------
;;; paths
;;; ----------------------------------------------------------------------------

(defun add-to-system-path (path)
  "Add a path to the exec-path variable and the Emacs process' PATH environmental variable."
  ;; exec-path
  (unless (member path exec-path)
    (add-to-list 'exec-path path))
  ;; PATH
  (let ((current-path (getenv "PATH")))
    (unless (and (equal current-path "")
                 (member path (split-string current-path ":")))
      (setenv "PATH" (concat path ":" current-path))))
  exec-path)

(defun add-subdirs-to-load-path (dir)
  (let ((default-directory (concat dir "/")))
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-system-path (expand-file-name "~/.local/bin"))
(add-to-system-path "/opt/brew/bin")
(add-to-system-path "/opt/local/bin")
(add-to-system-path "/usr/local/bin")

(setenv "MANPATH" "/usr/share/man:/usr/local/share/man:/usr/X11/man:/opt/brew/share/man:/opt/local/man")

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-subdirs-to-load-path (concat user-emacs-directory "site-lisp"))



;;; ----------------------------------------------------------------------------
;;; package system configuration
;;; ----------------------------------------------------------------------------

(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; bootstrap use-package
(setq package-pinned-packages
      '((bind-key    . "melpa-stable")
        (diminish    . "melpa-stable")
        (use-package . "melpa-stable")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)



;;; ----------------------------------------------------------------------------
;;; buffers
;;; ----------------------------------------------------------------------------

;; boring buffers: stop showing these in various lists
(setq boring-buffers
      (list "\\*buffer-selection\\*"
            "\\*Echo Area"
            "\\*Minibuf"
            "\\*Calendar\\*"
            "\\*Completions\\*"
            "\\*Quail Completions\\*"
            "TAGS"
            "\\*ESS\\*"
            "\\*helm"
            "\\*helm-mode"))

(defun cv--boring-buffer? (buf)
  (-all? #'null (mapcar #'(lambda (x)
                            (string-match-p x (buffer-name buf)))
                        boring-buffers)))


;; unique naming
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*")


;; bs (a nice built-in buffer switcher)

(require 'bs)

(setq bs-attributes-list '(("" 1 1 left bs--get-marked-string)
                           ("M" 1 1 left bs--get-modified-string)
                           ("R" 2 2 left bs--get-readonly-string)
                           ("Buffer" bs--get-name-length 10 left bs--get-name)
                           ("" 1 1 left " ")
                           ("" 1 1 left " ")
                           ("File" 12 12 left bs--get-file-name)
                           ("" 2 2 left "  ")))

(setq bs-default-configuration "all")

(add-to-list 'bs-configurations
             '("persp" nil nil nil
               (lambda (buf)
                 (with-current-buffer buf
                   (or (not (member buf (persp-buffers (persp-curr))))
                       (not (cv--boring-buffer? buf))))))
             nil)

(add-to-list 'bs-configurations
             '("all-not-boring" nil nil nil
               (lambda (buf)
                 (not (cv--boring-buffer? buf)))))

(global-set-key (kbd "C-x C-b") (lambda ()
                                  (interactive)
                                  (if (and (fboundp 'persp-buffers) persp-mode)
                                      (bs--show-with-configuration "persp")
                                    (bs--show-with-configuration "all-not-boring"))))

;;(global-set-key (kbd "C-x C-M-b") 'bs-show)
(global-set-key (kbd "C-x C-M-b") (lambda ()
                                    (interactive)
                                    (bs--show-with-configuration "all-not-boring")))


;; minibuffer configuration

;;(setq enable-recursive-minibuffers nil)
;;(minibuffer-depth-indicate-mode 1)
(setq max-mini-window-height 10)

;; exit the minibuffer when clicking outside it
(add-hook 'mouse-leave-buffer-hook
  (lambda ()
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (abort-recursive-edit))))

;; tweak the minibuffer to always kill some annoying buffers on exit
;; NB: This is now deprecated in favor filtering out boring-buffers, but
;; it is pretty good sample code. And old.
;;(add-hook 'minibuffer-exit-hook
;;  (lambda ()
;;    (let ((remove-these-buffers (list "*Completions*"
;;                                      "*Quail Completions*")))
;;      (mapc #'(lambda (buffer-name)
;;                (let ((buf (get-buffer buffer-name)))
;;                  (when buf
;;                    (kill-buffer buf))))
;;            remove-these-buffers))))



;;; ----------------------------------------------------------------------------
;;; backups
;;; ----------------------------------------------------------------------------

(define-minor-mode sensitive-mode
  "Disable backup creation and auto saving for sensitive files."
  nil " Sensitive" nil
  (if (symbol-value sensitive-mode)
      (progn (set (make-local-variable 'backup-inhibited) t)
             (when auto-save-default
               (auto-save-mode -1)))
    ;; else
    (kill-local-variable 'backup-inhibited)
    (when auto-save-default
      (auto-save-mode 1))))

(defvar backup-ignore-regexps
  '("\\.gpg$"
    "/Secure Files/"
    "[Pp]asswords\\.\\(note\\|text\\|txt\\|org\\)$"))

(add-hook 'find-file-hook
  (lambda ()
    (when (normal-backup-enable-predicate buffer-file-name)
      (let ((backup t))
        (mapc (lambda (re)
                (setq backup (and backup (not (string-match re buffer-file-name)))))
              backup-ignore-regexps)
        (unless backup (sensitive-mode))))))

(setq backup-by-copying t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      tramp-backup-directory-alist backup-directory-alist
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t
      vc-cvs-stay-local nil)

(setq auto-save-list-file-prefix (concat user-emacs-directory "auto-save-list/.saves-"))



;;; ----------------------------------------------------------------------------
;;; window and frame control
;;; ----------------------------------------------------------------------------

(setq frame-title-format
      (list "%b"
            '(:eval (if (fboundp 'cv--mode-line-abbrev-file-name)
                        (cv--mode-line-abbrev-file-name)
                      ""))
            '(:eval (if (and (fboundp 'persp-curr) (persp-curr))
                        (concatenate 'string " — " (persp-name (persp-curr)))
                      "")))
      ;; org-mode needs a copy of this for when it might manipulate the frame title
      org-frame-title-format-backup frame-title-format)


(setq display-buffer-reuse-frames t)         ; reuse windows in other frames
(setq pop-up-windows nil)                    ; display-buffer: avoid splitting
(setq even-window-heights nil)               ; display-buffer: avoid resizing


(winner-mode 1)                              ; restore windows: C-c right-arrow
(windmove-default-keybindings)               ; shift-arrow keys switch windows


;; fix window splitting behavior when possible
(setq display-buffer-alist
      '(("\\*cider-repl .*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (reusable-frames . visible)
         (side . bottom)
         (window-height . 0.2))
        ("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-same-window))
        ;; default
        (".*"
         (display-buffer-same-window))))


;; clicking in a different window should not move the cursor inside that window

(defun cv--mouse-set-point (orig-fn &rest args)
  (let* ((event (car args))
         (event-name (car event))
         (event-target-window (caadr event)))
    (if (and (eql 'down-mouse-1 event-name)
             (eql event-target-window (frame-selected-window)))
        (apply orig-fn args)
      (set-frame-selected-window nil event-target-window))))

(advice-add 'mouse-set-point :around #'cv--mouse-set-point)


(defun cv--mouse-drag-region (orig-fn &rest args)
  (let* ((event (car args))
         (event-target-window (caadr event)))
    (if (eql event-target-window (frame-selected-window))
        (apply orig-fn args)
      (set-frame-selected-window nil event-target-window))))

(advice-add 'mouse-drag-region :around #'cv--mouse-drag-region)


;; window resizing keys
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)



;;; ----------------------------------------------------------------------------
;;; global key bindings, not specific to other modes or functionality
;;; ----------------------------------------------------------------------------

(global-unset-key (kbd "C-0"))
(global-unset-key (kbd "C-1"))
(global-unset-key (kbd "C-2"))
(global-unset-key (kbd "C-3"))
(global-unset-key (kbd "C-4"))
(global-unset-key (kbd "C-5"))
(global-unset-key (kbd "C-6"))
(global-unset-key (kbd "C-7"))
(global-unset-key (kbd "C-8"))
(global-unset-key (kbd "C-9"))

(when window-system (global-unset-key (kbd "C-z")))
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)

(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-~") #'(lambda () (interactive) (other-frame -1)))
(global-set-key (kbd "M-o") 'other-window)
(when window-system (global-set-key (kbd "M-O") #'(lambda () (interactive) (other-window -1))))
(global-set-key (kbd "C-S-b") 'switch-to-prev-buffer)
(global-set-key (kbd "C-S-n") 'switch-to-next-buffer)
(global-set-key (kbd "C-M-S-o") 'ido-switch-buffer-other-window)

(global-set-key (kbd "C-M-n") 'up-list)
(global-set-key (kbd "C-M-p") 'backward-down-list)
(global-set-key (kbd "M-<up>") 'scroll-up-line)
(global-set-key (kbd "M-<down>") 'scroll-down-line)

(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

(global-set-key (kbd "C-S-j")
 (lambda ()
   "An improvement (?) over the built-in join-line function, which
   does not leave a single space in between the joined lines."
   (interactive)
   (end-of-line)
   (delete-char 1)
   ;;(delete-horizontal-space)
   ))

(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)

(global-set-key (kbd "M-q") (lambda ()
                              (interactive)
                              (if (y-or-n-p "Really fill paragraph?")
                                  (fill-paragraph))
                              (message "Done.")))

(macrolet ((define-matching-insert (key-chord open-character close-character)
             `(global-set-key (kbd ,key-chord)
                              (lambda ()
                                (interactive)
                                (insert ,open-character)
                                (save-excursion
                                  (insert ,close-character))))))
  (define-matching-insert "M-{" ?\{ ?\})
  (when window-system (define-matching-insert "M-[" ?\[ ?\]))
  (define-matching-insert "M-\"" ?\" ?\"))



;;; ----------------------------------------------------------------------------
;;; configure smaller built-in features and modes
;;; ----------------------------------------------------------------------------

;; abbrev

(diminish 'abbrev-mode)


;; grep

(setq-default grep-command "grep -nHr -e ")

(setq grep-find-command
      (concatenate
       'string
       "find . \\( -path '*.svn' -o -path '*.git' -o -path '*.hg' \\)"
       " "
       "-prune -o -type f -print0 | xargs -0 grep -I -i -n -e "))


;; man

;; reuse the current window
(setq Man-notify-method 'pushy)


;; spell checking

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
;;(setq ispell-extra-args '("--sug-mode=fast"))


;; completion

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))


;; compressed files

(auto-compression-mode 1)
(setq archive-zip-use-pkzip nil)
(setq archive-zip-extract '("unzip" "-qq" "-c"))
(setq archive-zip-update '("zip" "-u"))
(setq archive-zip-expunge '("zip" "-d"))
(add-to-list 'auto-mode-alist '("\\.war\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.ear\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.rar\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.jar\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.xpi\\'" . archive-mode))


;; gud (grand unified debugger)

(global-set-key [f5] 'gud-step)
(global-set-key [f6] 'gud-next)
(global-set-key [f7] 'gud-up)
(global-set-key [f8] 'gud-cont)


;; compile-mode (also see display-buffer-alist customization)

(setq compilation-scroll-output 'first-error)

(defun cv--compile-goto-error (orig-fn &rest args)
  (let ((display-buffer-overriding-action '(display-buffer-reuse-window (inhibit-same-window . nil))))
    (apply orig-fn args)))

(advice-add 'compile-goto-error :around #'cv--compile-goto-error)


;; linum: display fix with right-aligned padding

(defun cv--linum-update-window (orig-fn &rest args)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    (apply orig-fn args)))

(advice-add 'linum-update-window :around #'cv--linum-update-window)


;; recentf

(require 'recentf)

(setq recentf-max-menu-items 250)
(recentf-mode 1)


;; imenu

(setq imenu-auto-rescan t)


;; visual-line-mode

(diminish 'visual-line-mode " ν")


;; subword-mode

(add-hook 'subword-mode-hook
  (lambda ()
    (diminish 'subword-mode)))


;; dired

(put 'dired-find-alternate-file 'disabled nil)

;; quick hack for using GNU ls
(when-let ((gnu-ls (executable-find "gls")))
  (setq insert-directory-program gnu-ls)
  (setq dired-listing-switches "-gGha")
  (setq dired-use-ls-dired t))

(add-hook 'dired-mode-hook
  (lambda ()
    (define-key dired-mode-map (kbd "<RET>")
      (lambda ()
        (interactive)
        (let ((f (dired-get-file-for-visit)))
          (if (file-directory-p f)
              (dired-find-alternate-file)
            (dired-find-file)))))
    (define-key dired-mode-map (kbd "a") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "^")
      (lambda ()
        (interactive)
        (let ((curr (directory-file-name (dired-current-directory))))
          (find-alternate-file "..")
          (message curr)
          (dired-goto-file curr))))))


;; ediff

(require 'ediff)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


;; comint

(setq-default comint-input-ignoredups t)


;; eshell

(setq eshell-banner-message ""
      eshell-hist-ignoredups t
      eshell-save-history-index t)

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "[" 'face `(:foreground "lightgreen"))
         (propertize (user-real-login-name) 'face `(:foreground "lightblue"))
         (propertize "@" 'face `(:foreground "lightgreen"))
         (propertize (system-name) 'face `(:foreground "blanchedalmond"))
         (propertize "]" 'face `(:foreground "lightgreen"))
         ;; (propertize (if (= (user-uid) 0) " #" " ∴") 'face `(:foreground "lightgreen"))
         " "
         )))

(setq eshell-prompt-regexp "^\\[.*?\\] ")

(defun eshell/shortpwd ()
  (cv--display-dir (eshell/pwd)))

(defun eshell/... ()
  (eshell/cd "../.."))

(defun eshell/.... ()
  (eshell/cd "../../.."))

(defun eshell/..... ()
  (eshell/cd "../../../.."))

(defun eshell/...... ()
  (eshell/cd "../../../../.."))

(defun eshell/o (filename)
  "Just type o filename in eshell to open the file."
  (find-file filename))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun eshell-here ()
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(add-hook 'eshell-mode-hook
  (lambda ()
    (local-set-key (kbd "C-p") 'eshell-previous-input)
    (local-set-key (kbd "C-p") 'eshell-next-input)
    (local-set-key (kbd "C-r") 'eshell-isearch-backward)
    (local-set-key (kbd "C-M-r") 'helm-eshell-history)
    (local-set-key (kbd "C-S-d") (lambda () (interactive) (insert "exit") (eshell-send-input) (delete-window)))
    (local-set-key (kbd "C-c C-z") 'flip-windows)
    (local-set-key (kbd "<tab>") 'company-complete)
    (eshell/alias "v" "ls -la")))


;; TODO: Delete these after Emacs 25.3.

(defun eshell-next-prompt (n)
    "Move to end of Nth next prompt in the buffer.
See `eshell-prompt-regexp'."
    (interactive "p")
    (re-search-forward eshell-prompt-regexp nil t n)
    (when eshell-highlight-prompt
      (while (not (get-text-property (line-beginning-position) 'read-only) )
        (re-search-forward eshell-prompt-regexp nil t n)))
    (eshell-skip-prompt))

(defun eshell-previous-prompt (n)
    "Move to end of Nth previous prompt in the buffer.
See `eshell-prompt-regexp'."
    (interactive "p")
    (backward-char)
    (eshell-next-prompt (- n)))


;; ido mode

(setq ido-everywhere nil
      ido-auto-merge-work-directories-length -1 ; don't look for files in strange places
      ido-confirm-unique-completion t
      ido-enable-flex-matching t
      ido-default-buffer-method 'samewindow
      ido-max-prospects 20)

(ido-mode 'buffer)

(mapcar #'(lambda (x)
            (add-to-list 'ido-ignore-buffers x))
        boring-buffers)

(global-set-key (kbd "C-x M-f") 'ido-find-file)
(global-set-key (kbd "C-x M-d") 'ido-dired)
(global-set-key (kbd "C-x C-d") 'ido-dired)
(global-set-key (kbd "C-x M-i") 'ido-insert-file)


;; Gnus

(setq gnus-directory (concat user-emacs-directory "News/")
      gnus-startup-file (concat user-emacs-directory "News/.newsrc")
      message-directory (concat user-emacs-directory "Mail/")
      news-reply-header-hook nil
      gnus-large-newsgroup 300
      nntp-authinfo-function 'nntp-send-nosy-authinfo
      gnus-select-method '(nntp "news.eternal-september.org"
                                (nntp-open-connection-function nntp-open-ssl-stream)
                                (nntp-port-number "nntps")
                                (nntp-address "news.eternal-september.org"))
      gnus-secondary-select-methods '((nntp "news.gmane.org"))
      user-login-name "nobody"
      ;; performance and cleanliness
      gnus-read-active-file nil
      gnus-check-bogus-newsgroups nil
      gnus-check-new-newsgroups nil
      gnus-save-killed-list nil)


;; term-mode

(require 'term)

;; term-suppress-hard-newline is an interesting animal. With normal term use, it
;; seems to be better kept as t. Otherwise, it generates hard newlines whenever
;; terminal output wraps around the screen. However, GNU screen doesn't like t,
;; it needs nil. But tmux works perfectly well with t. See
;; https://stackoverflow.com/questions/24517172/is-there-a-way-to-make-regions-in-term-modes-respect-line-wrapping/48634830.
(setq term-suppress-hard-newline t)

(add-hook 'term-mode-hook
  (lambda ()
    (define-key term-raw-map (kbd "M-:") 'eval-expression)
    (define-key term-raw-map (kbd "C-m") 'term-send-raw)
    (define-key term-raw-map (kbd "C-p") 'term-send-raw)
    (define-key term-raw-map (kbd "C-n") 'term-send-raw)
    (define-key term-raw-map (kbd "M-C-n") 'next-line)
    (define-key term-raw-map (kbd "M-C-p") 'previous-line)
    (define-key term-raw-map (kbd "M-C-f") 'forward-char)
    (define-key term-raw-map (kbd "M-C-b") 'backward-char)
    (define-key term-raw-map (kbd "C-<left>") (kbd "M-b"))
    (define-key term-raw-map (kbd "C-<right>") (kbd "M-f"))
    (define-key term-raw-map (kbd "M-<left>") (kbd "M-b"))
    (define-key term-raw-map (kbd "M-<right>") (kbd "M-f"))
    (define-key term-raw-map (kbd "C-<backspace>") 'term-send-backward-kill-word)
    (define-key term-raw-map (kbd "M-w") 'kill-ring-save)
    (define-key term-raw-map (kbd "C-y")
      (lambda (&optional string)
        (interactive)
        (process-send-string
         (get-buffer-process (current-buffer))
         (if string string (current-kill 0)))))
    (define-key term-raw-map (kbd "C-M-y") 'helm-show-kill-ring)
    (define-key term-raw-map (kbd "M-o") 'other-window)
    (define-key term-raw-map (kbd "C-M-o") 'ace-window)
    (define-key term-raw-map (kbd "C-c C-e") #'(lambda ()
                                                 (interactive)
                                                 (term-send-raw-string "\e")))))


;; text-mode

(add-hook 'text-mode-hook
  (lambda ()
    (when (string-equal major-mode "text-mode")
      (visual-line-mode))))


;; tex-mode

(add-hook 'latex-mode-hook
  (lambda ()
    (visual-line-mode)))


;; css-mode

(add-to-list 'auto-mode-alist '("\\.sass$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(setq-default css-indent-offset 2)


;; ruby-mode

(add-hook 'ruby-mode-hook
  (lambda ()
    (subword-mode)
    (define-key ruby-mode-map (kbd "C-m") 'newline-and-indent)
    (define-key ruby-mode-map (kbd "M-,") 'pop-tag-mark)
    (setq show-trailing-whitespace t)))


;; sh-mode (shell script)

(add-to-list 'auto-mode-alist '("zshrc" . sh-mode))

(add-hook 'sh-mode
  (lambda ()
    (local-unset-key (kbd "C-c C-z"))))


;; nxml-mode

(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng$" . nxml-mode))


;; python-mode

(setq python-python-command "python2.7")

(add-hook 'python-mode-hook
  (lambda ()
    (setq show-trailing-whitespace t)
    (define-key python-mode-map (kbd "C-c C-z") nil)
    (subword-mode)
    (defconst python-block-pairs
      '(("else" "if" "elif" "while" "for" "try" "except")
        ("elif" "if" "elif")
        ;; fix finally - try - except indentation
        ("except" "try" "except")
        ("finally" "try" "except")))))


;; cc-mode (C, C++, Objective-C)

(add-hook 'c-mode-common-hook
  (lambda ()
    (c-set-style "ellemtel")
    (local-set-key (kbd "C-m") 'newline-and-indent)
    (local-set-key (kbd "C-M-x") 'compile)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (local-unset-key (kbd "M-j"))
    (electric-indent-local-mode t)
    (c-toggle-auto-newline -1)
    (subword-mode)
    (helm-gtags-mode 1)
    (setq show-trailing-whitespace t)))

(add-hook 'objc-mode-hook
  (lambda ()
    (setq c-basic-offset 4)))

(add-hook 'c++-mode-hook
  (lambda ()
    ;; See http://stackoverflow.com/questions/14939608/how-to-change-emacs-struct-indents-from-4-to-2-spaces for more information.
    ;; take care of indenting "class ... { private: ... }" blocks
    (c-set-offset 'inclass '++)
    (c-set-offset 'access-label '-)
    (c-set-offset 'friend '-)
    ;; take care of indenting "struct ... { ... }" blocks
    (c-set-offset 'topmost-intro
                  (lambda (langelem)
                    (let ((inclass (assoc 'inclass c-syntactic-context)))
                      (if (not inclass)
                          0
                        (save-excursion
                          (goto-char (c-langelem-pos inclass))
                          (if (looking-at "struct") '- '=))))))))


;; Common Lisp

(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

(require 'refactor)                     ; custom Lisp symbol renaming

(add-hook 'lisp-mode-hook
  (lambda ()
    (paredit-mode 1)
    (setq lisp-indent-function 'common-lisp-indent-function)
    (define-key lisp-mode-map (kbd "C-m") 'newline-and-indent)
    ;;(define-key lisp-mode-map (kbd "C-.") 'slime-complete-symbol)
    (setq show-trailing-whitespace t)
    ;; fix loop macro indentation
    (setq lisp-simple-loop-indentation 1)
    (setq lisp-loop-keyword-indentation 6)
    (setq lisp-loop-forms-indentation 6)
    ;; make square brackets and parentheses equivalent for indentation
    (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
    (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)))



;;; ----------------------------------------------------------------------------
;;; finish up
;;; ----------------------------------------------------------------------------

;; load more startup files; order matters!
(let ((startup-files (list
                      "utils.el"
                      "interactives.el"
                      "packages.el"
                      "org.el"
                      "elisp.el"
                      "themes.el"
                      "modeline.el"
                      "../private/emacs-private.el")))
  (dolist (f startup-files)
    (load-file (expand-file-name (concat "../" f) user-emacs-directory))))



;;; ----------------------------------------------------------------------------
;;;             better to reign in hell, than to serve in heaven
;;; ----------------------------------------------------------------------------
