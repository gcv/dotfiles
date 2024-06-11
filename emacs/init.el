;;; -*- lexical-binding: t -*-

(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs started in %s seconds (garbage collections: %d)"
             (format "%.2f"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done)))

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(setq custom-file (concat user-emacs-directory "custom.el"))    ; customize: don't touch init.el
(load custom-file 'noerror)                                     ; customize: load customizations

(setq confirm-kill-emacs 'yes-or-no-p)                          ; ask before exiting
(setq debug-on-error nil)                                       ; turn this on only when needed
(setq enable-remote-dir-locals t)                               ; use remote .dir-locals.el
(setq font-lock-verbose nil)                                    ; silence slow compile messages
(setq gnutls-verify-error t)                                    ; enable all TLS checks
(setq inhibit-startup-screen t)                                 ; turn off the splash screen
(setq initial-scratch-message nil)                              ; nothing in *scratch*
(setq load-prefer-newer t)                                      ; deal with outdated .elc files
(setq network-security-level 'high)                             ; security!
(setq parens-require-spaces nil)                                ; m-( should not insert spaces
(setq require-final-newline t)                                  ; newline required at end of file
(setq ring-bell-function #'ignore)                              ; control beeping
(setq save-abbrevs nil)                                         ; don't use abbrev-mode
(setq save-interprogram-paste-before-kill t)                    ; don't lose clipboard on kill
(setq scroll-step 1)                                            ; stop scrolling by half-screens
(setq sentence-end-double-space nil)                            ; one space between sentences
(setq tags-revert-without-query t)                              ; minimize TAGS use annoyance
(setq truncate-partial-width-windows nil)                       ; no more truncated lines
(setq use-dialog-box nil)                                       ; turn off lame GUI dialogs
(setq-default indent-tabs-mode nil)                             ; replace tabs with spaces
(setq-default fill-column 80)                                   ; 80-character screens, not 70

(fset 'yes-or-no-p 'y-or-n-p)                                   ; substitute y-or-n for yes-or-no

(blink-cursor-mode -1)                                          ; traditional steady cursor
(column-number-mode 1)                                          ; show column numbers
(global-auto-revert-mode 1)                                     ; track externally changed files
(show-paren-mode 1)                                             ; show matching parens
(transient-mark-mode -1)                                        ; muscle memory


;;; OS-specific configuration
(pcase system-type
  ('darwin
   (setq ns-antialias-text t)                                   ; anti-aliased fonts on Mac OS
   (setq ns-pop-up-frames nil)                                  ; don't open files in new frame
   (setq ns-command-modifier 'meta)                             ; fix Cmd as Meta on Mac OS
   (setq mac-command-modifier 'meta)                            ; fix Cmd as Meta on Mac OS
   (setq ns-alternate-modifier 'none)                           ; leave left Alt alone
   (setq mac-alternate-modifier 'none)                          ; leave left Alt alone
   (setq ns-right-command-modifier 'hyper)                      ; right Cmd should be Hyper
   (setq mac-right-command-modifier 'hyper)                     ; right Cmd should be Hyper
   t)
  ('windows-nt
   ;; This is much more limited than the Darwin version. It does not have access
   ;; to Fn keys, and does not distinguish between left and right Alt keys. :(
   (setq w32-rwindow-modifier 'hyper)                           ; right Windows should be Hyper
   t)
  (_ "generic Unix" t))


;;; activate mouse-based scrolling
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))


;;; https://consoledonottrack.com
(setenv "DO_NOT_TRACK" "1")


;;; Unicode and UTF-8
(set-locale-environment "en_US.UTF-8")
;;(set-language-environment "UTF-8")
;;(prefer-coding-system 'utf-8)
;;(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)


;;; enable useful commands
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


;;; history
(savehist-mode 1)
(setq history-length 250
      history-delete-duplicates t)
(put 'file-name-history 'history-length 1500)


;;; Emacs server
(unless (or (bound-and-true-p server-process)
            (file-exists-p (concat (getenv "TMPDIR") "emacs"
                                   (number-to-string
                                    (user-real-uid)) "/server")))
  (server-start))


;;; ----------------------------------------------------------------------------
;;; paths
;;; ----------------------------------------------------------------------------

(defun add-to-env (env path-raw &optional separator)
  (let* ((path (expand-file-name path-raw))
         (separator (if (null separator) ":" separator))
         (current-env-raw (or (getenv env) ""))
         (current-env (if (string-equal current-env-raw "")
                          (list)
                        (split-string current-env-raw separator))))
    (unless (member path current-env)
      (setenv env (if (null current-env)
                      path
                    (mapconcat 'identity (cons path current-env) separator))))
    (getenv env)))

(defun add-to-system-path (path-raw)
  "Add a path to the exec-path variable and the Emacs process' PATH environmental variable."
  (let ((path (expand-file-name path-raw)))
    (when (and (file-exists-p path) (file-directory-p path))
      (unless (member path exec-path)
        (add-to-list 'exec-path path))
      (add-to-env "PATH" path))
    exec-path))

(defun add-subdirs-to-load-path (dir)
  (let ((default-directory (concat dir "/")))
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-system-path "/bin")
(add-to-system-path "/sbin")
(add-to-system-path "/usr/bin")
(add-to-system-path "/usr/sbin")
(add-to-system-path "/usr/local/bin")
(add-to-system-path "/usr/local/sbin")
(add-to-system-path "/opt/brew/bin")
(add-to-system-path "/opt/brew/sbin")
(add-to-system-path "~/.linuxbrew/bin")
(add-to-system-path "~/.linuxbrew/sbin")
(add-to-system-path "~/.nix-profile/bin")
(add-to-system-path "~/.nix-profile/sbin")
(add-to-system-path "~/.local/bin")
(add-to-system-path "~/.local/sbin")

(add-to-env "MANPATH" "/usr/share/man")
(add-to-env "MANPATH" "/usr/local/share/man")
(add-to-env "MANPATH" "/opt/brew/share/man")
(add-to-env "MANPATH" "~/.nix-profile/share/man")

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-subdirs-to-load-path (concat user-emacs-directory "site-lisp"))


;;; ----------------------------------------------------------------------------
;;; nix
;;; ----------------------------------------------------------------------------

(when (file-exists-p "~/.nix-defexpr")
  (setenv "NIX_PATH" (format "nixpkgs=%s/.nix-defexpr/channels/nixpkgs" (getenv "HOME"))))
(when (file-exists-p "~/.nix-profile")
  (setenv "NIX_SSL_CERT_FILE" (format "%s/.nix-profile/etc/ssl/certs/ca-bundle.crt" (getenv "HOME"))))


;;; ----------------------------------------------------------------------------
;;; package system configuration
;;; ----------------------------------------------------------------------------

(require 'package)

(setq package-gnupghome-dir nil)

;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-archive-priorities
      '(;;("org" . 50)
        ("nongnu" . 40)
        ("gnu" . 30)
        ("melpa-stable" . 20)
        ("melpa" . 10)))

(package-initialize)

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))
(require 'use-package)

(customize-set-variable 'use-package-enable-imenu-support t)
(customize-set-variable 'use-package-always-ensure t)
(customize-set-variable 'use-package-always-defer t)
(customize-set-variable 'use-package-always-pin "melpa-stable")
;;(setq use-package-verbose t)

(defmacro use-feature (name &rest args)
  "Like use-package but for built-in Emacs features."
  (declare (indent defun))
  `(use-package ,name
     ;;:straight nil
     :ensure nil
     ,@args))

;; add :vc or :quelpa keywords to use-package
(cond ((< emacs-major-version 29)
       ;; quelpa, and quelpa-use-package must be manually installed
       (unless (and (package-installed-p 'quelpa)
                    (package-installed-p 'quelpa-use-package))
         (package-refresh-contents)
         (package-install 'quelpa)
         (package-install 'quelpa-use-package))
       (require 'quelpa)
       (require 'quelpa-use-package)
       (setq quelpa-stable-p nil
             quelpa-checkout-melpa-p nil)
       (quelpa-use-package-activate-advice) ; undocumented!
       )
      ;; vc-use-package must be manually installed
      ((and (>= emacs-major-version 29) (< emacs-major-version 30))
       (unless (package-installed-p 'vc-use-package)
         (package-vc-install "https://github.com/slotThe/vc-use-package"))
       (require 'vc-use-package))
      ;; Emacs 30+ have vc-use-package built-in
      (t
       (require 'vc-use-package)))


;;; ----------------------------------------------------------------------------
;;; buffer behavior and control
;;; ----------------------------------------------------------------------------

;;; boring buffers: stop showing these in various lists
;;; This variable is used as a master list
(setq ignore-buffers
      (list "\\*Messages\\*"
            "\\` "
            "\\*Calendar\\*"
            "\\*Completions\\*"
            "\\*ESS\\*"
            "\\*Echo Area"
            "\\*Minibuf"
            "\\*Quail Completions\\*"
            "\\*Shell Command Output\\*"
            "\\*buffer-selection\\*"
            "\\*envrc\\*"
            "\\*Async-native-compile-log\\*"
            "\\*helm"
            "\\*helm-mode"))


;;; unique naming
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*")


;;; bs (a nice built-in buffer switcher)
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

(global-set-key (kbd "C-x C-b") (lambda (arg)
                                  (interactive "P")
                                  (if (fboundp 'persp-bs-show)
                                      (persp-bs-show arg)
                                    (bs-show "all"))))


;;; minibuffer configuration
;;(setq enable-recursive-minibuffers nil)
;;(minibuffer-depth-indicate-mode 1)
(setq max-mini-window-height 10)

;;; exit the minibuffer when clicking outside it
(defun /mouse-leave-buffer-hook ()
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook #'/mouse-leave-buffer-hook)

;;; tweak the minibuffer to always kill some annoying buffers on exit
;;; NB: This is now deprecated in favor filtering out boring-buffers, but
;;; it is pretty good sample code. And old.
;;(add-hook 'minibuffer-exit-hook
;;  (lambda ()
;;    (let ((remove-these-buffers (list "*Completions*"
;;                                      "*Quail Completions*")))
;;      (mapc #'(lambda (buffer-name)
;;                (let ((buf (get-buffer buffer-name)))
;;                  (when buf
;;                    (kill-buffer buf))))
;;            remove-these-buffers))))

;;; advanced hack: disable GC in the minibuffer to keep display snappy for,
;;; e.g., helm-M-x
(defun /minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun /minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'/minibuffer-exit-hook)


;;; ----------------------------------------------------------------------------
;;; backups and sensitive-mode definition
;;; ----------------------------------------------------------------------------

(define-minor-mode sensitive-mode
  "Disable backup creation and auto saving for sensitive files."
  :init-value nil
  :lighter " Sensitive"
  :keymap nil
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

(defun /find-file-hook ()
  (when (normal-backup-enable-predicate buffer-file-name)
    (let ((backup t))
      (mapc (lambda (re)
              (setq backup (and backup (not (string-match re buffer-file-name)))))
            backup-ignore-regexps)
      (unless backup (sensitive-mode)))))

(add-hook 'find-file-hook #'/find-file-hook)

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
            '(:eval (if (fboundp '/mode-line-abbrev-file-name)
                        (/mode-line-abbrev-file-name)
                      ""))
            ;;'(:eval (concat " — " (format-time-string "%b %d")))
            '(:eval (if (and (fboundp 'persp-curr) (boundp 'persp-mode) persp-mode (persp-curr))
                        (concat " — " (persp-name (persp-curr)))
                      "")))
      ;; org-mode needs a copy of this for when it might manipulate the frame title
      org-frame-title-format-backup frame-title-format)


;;; fix window splitting behavior when possible
(setq window-resize-pixelwise t)             ; smoother window resizing?

;; display-buffer customization; note that this formerly changed
;; display-buffer-alist, but this is deemed too aggressive
;; (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=49069#28)
(customize-set-variable 'even-window-sizes nil)     ; avoid resizing
(customize-set-variable 'split-height-threshold 15) ; for rare occasions
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(winner-mode 1)                              ; restore windows: C-c right-arrow
(windmove-default-keybindings)               ; shift-arrow keys switch windows


;;; clicking in a different window should not move the cursor inside that window
(defun /mouse-set-point (orig-fn &rest args)
  (condition-case err
      (let* ((event (car args))
             (event-name (car event))
             (event-target-window (caadr event)))
        (if (and (eql 'down-mouse-1 event-name)
                 (eql event-target-window (frame-selected-window)))
            (apply orig-fn args)
          (set-frame-selected-window nil event-target-window)))
    (wrong-type-argument
     ;; Ignore this error and continue; it seems to happen when a keyboard
     ;; action emulates a mouse click for some reason. In devdocs-mode,
     ;; following a link with the keyboard makes an event object that goes calls
     ;; mouse-set-point but with an event object which does not destructure the
     ;; way this code expects. /shrugs
     (apply orig-fn args)
     )))

(advice-add 'mouse-set-point :around #'/mouse-set-point)

(defun /mouse-drag-region (orig-fn &rest args)
  (let* ((event (car args))
         (event-target-window (caadr event)))
    (if (eql event-target-window (frame-selected-window))
        (apply orig-fn args)
      (set-frame-selected-window nil event-target-window))))

(advice-add 'mouse-drag-region :around #'/mouse-drag-region)


;;; window resizing keys
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


;;; terminal mode: show a prettier separator
(unless window-system
  (set-face-inverse-video-p 'vertical-border nil)
  (set-face-background 'vertical-border (face-background 'default))
  (set-display-table-slot standard-display-table
                          'vertical-border
                          (make-glyph-code ?│)
                          ;;(make-glyph-code ? )
                          ))


;;; terminal mode: clipboard integration
(defun osc52-copy-to-clipboard (str)
  "Use ANSI OSC52 escape sequence to send last kill-ring entry to clipboard"
  ;; See https://sunaku.github.io/tmux-yank-osc52.html an explanation. If this
  ;; does not work in some situations, try https://github.com/spudlyo/clipetty
  ;; instead. It tries to detect the exact pseudo-tty to which to send the
  ;; escape sequence.
  ;; Not sure how this interacts with xclip.el.
  (let* ((txt (base64-encode-string (encode-coding-string str 'utf-8) t))
         (esc-seq (format "\033]52;c;%s\a" txt)))
    (send-string-to-terminal esc-seq)))

(defun osc52-kill-advice (orig-fn str)
  (unless (display-graphic-p)
    (osc52-copy-to-clipboard str))
  (funcall orig-fn str))
(unless (display-graphic-p)
  (add-function :around interprogram-cut-function #'osc52-kill-advice)
  ;;(remove-function interprogram-cut-function #'osc52-kill-advice)
  )

(defun copy-last-kill-to-clipboard ()
  (interactive)
  (when (display-graphic-p)
    (user-error "copy-last-kill-to-clipboard is not necessary in GUI Emacs"))
  (osc52-copy-to-clipboard (substring-no-properties (nth 0 kill-ring))))


;;; terminal mode: fix key bindings
;;; use ctrl-v in a terminal to capture the exact code to remap
;;; XXX: Do not use raw M-[ for any key binding! It can match the escape
;;; sequence of many other commands as a prefix!
(add-hook 'tty-setup-hook
  (lambda ()
    ;; function-key-map also works
    ;; iTerm xterm emulation:
    (define-key input-decode-map "\e[1;9A" [M-up])
    (define-key input-decode-map "\e[1;9B" [M-down])
    (define-key input-decode-map "\e[1;9D" [M-left])
    (define-key input-decode-map "\e[1;9C" [M-right])
    (define-key input-decode-map "[1;10A" [M-S-up])
    (define-key input-decode-map "[1;10B" [M-S-down])
    (define-key input-decode-map "[1;10D" [M-S-left])
    (define-key input-decode-map "[1;10C" [M-S-right])
    ;; common to iTerm and Blink:
    (define-key input-decode-map "[1;5A" [C-up])
    (define-key input-decode-map "[1;5B" [C-down])
    (define-key input-decode-map "[1;5D" [C-left])
    (define-key input-decode-map "[1;5C" [C-right])
    (define-key input-decode-map "[1;2A" [S-up])
    (define-key input-decode-map "[1;2B" [S-down])
    (define-key input-decode-map "[1;2D" [S-left])
    (define-key input-decode-map "[1;2C" [S-right])
    (define-key input-decode-map "[1;6A" [C-S-up])
    (define-key input-decode-map "[1;6B" [C-S-down])
    (define-key input-decode-map "[1;6D" [C-S-left])
    (define-key input-decode-map "[1;6C" [C-S-right])
    ;; iTerm Apple Terminal.app emulation (by default does not distinguish M-up, C-up, and C-S-up):
    (define-key input-decode-map "[A" [M-up])
    (define-key input-decode-map "[B" [M-down])
    (define-key input-decode-map "b" [M-left])
    (define-key input-decode-map "f" [M-right])
    ;; Blink:
    (define-key input-decode-map "[1;3A" [M-up])
    (define-key input-decode-map "[1;3B" [M-down])
    (define-key input-decode-map "[1;3D" [M-left])
    (define-key input-decode-map "[1;3C" [M-right])
    (define-key input-decode-map "[1;4A" [M-S-up])
    (define-key input-decode-map "[1;4B" [M-S-down])
    (define-key input-decode-map "[1;4D" [M-S-left])
    (define-key input-decode-map "[1;4C" [M-S-right])
    ))


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

;; pop-global-mark is confusing, especially with perspectives
(global-unset-key (kbd "C-x C-@"))
(global-set-key (kbd "C-x C-SPC") 'pop-to-mark-command)

(when window-system (global-unset-key (kbd "C-z")))
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)

(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'redo-only)

(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-~") (lambda () (interactive) (other-frame -1)))
(global-set-key (kbd "M-o") 'other-window)
(when window-system (global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1))))
(global-set-key (kbd "C-S-b") 'switch-to-prev-buffer)
(global-set-key (kbd "C-S-n") 'switch-to-next-buffer)
(global-set-key (kbd "C-M-S-o") 'ido-switch-buffer-other-window)

(global-set-key (kbd "C-M-n") 'up-list)
(global-set-key (kbd "C-M-p") 'backward-down-list)
(global-set-key (kbd "M-<up>") 'scroll-up-line)
(global-set-key (kbd "H-<up>") 'scroll-up-line)
(global-set-key (kbd "M-<down>") 'scroll-down-line)
(global-set-key (kbd "H-<down>") 'scroll-down-line)

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
(global-set-key (kbd "C-c M-;") 'comment-or-uncomment-region)

(global-set-key (kbd "M-q") (lambda ()
                              (interactive)
                              (if (y-or-n-p "Really fill paragraph?")
                                  (fill-paragraph))
                              (message "Done.")))

(cl-macrolet ((define-matching-insert (key-chord open-character close-character)
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
;;; dired
;;; ----------------------------------------------------------------------------

(use-feature dired
  :init
  (require 'dired-x)

  (put 'dired-find-alternate-file 'disabled nil)

  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (setq dired-clean-up-buffers-too t
        dired-clean-confirm-killing-deleted-buffers t)

  ;; quick hack for using GNU ls
  (let* ((ls-binary (executable-find "ls"))
         (gnu-ls? (= 0 (call-process ls-binary nil nil nil "--version"))))
    (setq insert-directory-program ls-binary)
    (if gnu-ls?
        ;; XXX: DO NOT use --indicator-style flags with ls! They confuse dired!
        (setq dired-listing-switches "-g --almost-all --human-readable --time-style=long-iso --sort=version --no-group")
      (setq dired-listing-switches "-lAgh")
      (setq dired-use-ls-dired nil)))

  (defun /dired-mode-hook ()
    (dired-hide-details-mode 1)
    (define-key dired-mode-map (kbd "<RET>")
      (lambda ()
        (interactive)
        (let ((f (dired-get-file-for-visit)))
          (if (file-directory-p f)
              (dired-find-alternate-file)
            (dired-find-file)))))
    (define-key dired-mode-map (kbd "<S-return>")
      (lambda ()
        (interactive)
        (let ((f (dired-get-file-for-visit)))
          (ace-select-window)
          (find-file f))))
    (define-key dired-mode-map (kbd "a") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "^")
      (lambda ()
        (interactive)
        (let ((curr (directory-file-name (dired-current-directory))))
          (find-alternate-file "..")
          (message curr)
          (dired-goto-file curr)))))

  (add-hook 'dired-mode-hook #'/dired-mode-hook)
  )


;;; ----------------------------------------------------------------------------
;;; eshell
;;; ----------------------------------------------------------------------------

(use-feature eshell
  :bind
  (("C-!" . eshell-here))

  :custom
  (eshell-banner-message "")
  (eshell-history-size 10000)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-index t)
  (eshell-prompt-regexp "^\\[.*?\\] ")
  (eshell-prompt-function
   (lambda ()
     (concat
      (propertize "[" 'face `(:foreground "lightgreen"))
      (propertize (user-real-login-name) 'face `(:foreground "lightblue"))
      (propertize "@" 'face `(:foreground "lightgreen"))
      (propertize (system-name) 'face `(:foreground "blanchedalmond"))
      (propertize "]" 'face `(:foreground "lightgreen"))
      ;;(propertize (if (= (user-uid) 0) " #" " ∴") 'face `(:foreground "lightgreen"))
      " "
      )))

  :hook
  (eshell-mode . /eshell-mode-hook)

  :init
  (defun eshell/h ()
    (interactive)
    (insert
     (completing-read "History: " (delete-dups (ring-elements eshell-history-ring)))))

  (defun eshell/shortpwd ()
    (/display-dir (eshell/pwd)))

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

  (defun /eshell-mode-hook ()
    (local-set-key (kbd "C-p") 'eshell-previous-input)
    (local-set-key (kbd "C-p") 'eshell-next-input)
    (local-set-key (kbd "C-r") 'eshell-isearch-backward)
    (local-set-key (kbd "C-M-r") 'helm-eshell-history)
    (local-set-key (kbd "C-S-d") (lambda () (interactive) (insert "exit") (eshell-send-input) (delete-window)))
    (local-set-key (kbd "C-c C-z") 'flip-windows)
    (add-to-list 'eshell-visual-commands "htop")
    (add-to-list 'eshell-visual-commands "tmux")
    (eshell/alias "dir" "ls -a")
    (eshell/alias "v" "ls -laH"))

;;; TODO: Delete these after Emacs 25.3.
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
  )


;;; ----------------------------------------------------------------------------
;;; tree-sitter
;;; ----------------------------------------------------------------------------

(use-feature treesit
  :if (>= emacs-major-version 29)
  :custom
  ;;(treesit-extra-load-path (list (concat user-emacs-directory "tree-sitter/")))
  (treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                                   (c "https://github.com/tree-sitter/tree-sitter-c")
                                   (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
                                   (clojure "https://github.com/sogaiu/tree-sitter-clojure")
                                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                                   (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
                                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                   (css "https://github.com/tree-sitter/tree-sitter-css")
                                   (scss "https://github.com/serenadeai/tree-sitter-scss")
                                   (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                                   (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
                                   (glsl "https://github.com/theHamsta/tree-sitter-glsl")
                                   (go "https://github.com/tree-sitter/tree-sitter-go")
                                   (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
                                   (heex "https://github.com/phoenixframework/tree-sitter-heex")
                                   (html "https://github.com/tree-sitter/tree-sitter-html")
                                   (janet-simple "https://github.com/sogaiu/tree-sitter-janet-simple")
                                   (java "https://github.com/tree-sitter/tree-sitter-java")
                                   (javascript ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
                                   (json "https://github.com/tree-sitter/tree-sitter-json")
                                   (julia "https://github.com/tree-sitter/tree-sitter-julia")
                                   (lua "https://github.com/MunifTanjim/tree-sitter-lua")
                                   (make "https://github.com/alemuller/tree-sitter-make")
                                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                                   (org "https://github.com/milisims/tree-sitter-org")
                                   (perl "https://github.com/ganezdragon/tree-sitter-perl")
                                   (proto "https://github.com/mitchellh/tree-sitter-proto")
                                   (python "https://github.com/tree-sitter/tree-sitter-python")
                                   (r "https://github.com/r-lib/tree-sitter-r")
                                   (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
                                   (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                   (scala "https://github.com/tree-sitter/tree-sitter-scala")
                                   (surface "https://github.com/connorlay/tree-sitter-surface")
                                   (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")
                                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                                   (tsx ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                                   (typescript ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
                                   (verilog "https://github.com/tree-sitter/tree-sitter-verilog")
                                   (vhdl "https://github.com/alemuller/tree-sitter-vhdl")
                                   (wgsl "https://github.com/mehmetoguzderin/tree-sitter-wgsl")
                                   (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                                   (dart "https://github.com/ast-grep/tree-sitter-dart")
                                   (souffle "https://github.com/chaosite/tree-sitter-souffle")
                                   (kotlin "https://github.com/fwcd/tree-sitter-kotlin")))
  )


;;; ----------------------------------------------------------------------------
;;; configure smaller built-in features and modes
;;; ----------------------------------------------------------------------------

;;; project.el
(use-feature project
  :bind
  (("C-c p f" . project-find-file)))


;;; abbrev
(defun /abbrev-mode-hook ()
  (diminish 'abbrev-mode))

(add-hook 'abbrev-mode-hook #'/abbrev-mode-hook)


;;; ansi-color
(use-feature ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))


;;; eldoc
(use-feature eldoc
  :custom
  ;;(eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit) ; default
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  )


;;; EasyPG (epg)
(setq epg-pinentry-mode 'loopback)


;;; isearch
(setq lazy-highlight-initial-delay 0.25)


;;; grep
(setq grep-command "grep -nHr -e ")

(setq grep-find-command
      (concat
       "find . \\( -path '*.svn' -o -path '*.git' -o -path '*.hg' \\)"
       " "
       "-prune -o -type f -print0 | xargs -0 grep -I -i -n -e "))


;;; man and woman
(setq Man-notify-method 'pushy)         ; reuse the current window
(setq woman-use-own-frame nil)          ; stop opening useless frames
(setq woman-fill-frame t)               ; frames display >72 characters


;;; spell checking
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
;;(setq ispell-extra-args '("--sug-mode=fast"))


;;; completion
(setq hippie-expand-try-functions-list
      '(try-complete-file-name
        try-expand-dabbrev))
(global-set-key (kbd "M-/") #'hippie-expand)

(global-set-key (kbd "C-.") #'completion-at-point)
(global-set-key (kbd "C-c .") #'completion-at-point)


;;; compressed files
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


;;; gud (grand unified debugger)
(global-set-key [f5] 'gud-step)
(global-set-key [f6] 'gud-next)
(global-set-key [f7] 'gud-up)
(global-set-key [f8] 'gud-cont)


;;; compile-mode (also see display-buffer-alist customization)
(setq compilation-scroll-output 'first-error)

(defun /compile-goto-error (orig-fn &rest args)
  (let ((display-buffer-overriding-action '(display-buffer-reuse-window (inhibit-same-window . nil))))
    (apply orig-fn args)))

(advice-add 'compile-goto-error :around #'/compile-goto-error)


;;; linum: display fix with right-aligned padding
;;; NB: linum-mode is deprecated in Emacs 26, use display-line-numbers-mode
;;(defun /linum-update-window (orig-fn &rest args)
;;  (let* ((w (length (number-to-string
;;                     (count-lines (point-min) (point-max)))))
;;         (linum-format (concat "%" (number-to-string w) "d ")))
;;    (apply orig-fn args)))
;;
;;(advice-add 'linum-update-window :around #'/linum-update-window)

(defun linum-mode ()
  (interactive)
  (error "linum-mode is deprecated; use display-line-numbers-mode"))


;;; display-line-numbers-mode
(setq display-line-numbers-width-start t)
(setq display-line-numbers-type t)
;;(setq display-line-numbers-type 'relative)
;;(setq display-line-numbers-type 'visual)


;;; recentf
(setq recentf-max-menu-items 250
      recentf-keep '(file-remote-p file-readable-p))
(recentf-mode 1)


;;; imenu
(setq imenu-auto-rescan t)


;;; visual-line-mode
(defun /visual-line-mode-hook ()
  (diminish 'visual-line-mode " ν"))

(add-hook 'visual-line-mode-hook #'/visual-line-mode-hook)


;;; subword-mode
(defun /subword-mode-hook ()
  (diminish 'subword-mode))

(add-hook 'subword-mode-hook #'/subword-mode-hook)


;;; ediff
(customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)
(customize-set-variable 'ediff-split-window-function 'split-window-horizontally)


;;; outline
(use-feature outline-minor-mode
  :hook
  (outline-minor-mode . /outline-minor-mode-hook)

  :init
  (let ((outline-cycle-state 0))
    (defun outline-cycle ()
      (interactive)
      (cond ((and (not (= 2 outline-cycle-state))
                  (outline-invisible-p (line-end-position)))
             (outline-show-children)
             (setq outline-cycle-state 1))
            ((and (not (= 2 outline-cycle-state))
                  (equal 'outline-cycle last-command))
             (outline-show-subtree)
             (setq outline-cycle-state 2))
            ((or (= 2 outline-cycle-state)
                 (and (not (outline-invisible-p (line-end-position)))
                      (equal 'outline-cycle last-command)))
             (outline-hide-subtree)
             (setq outline-cycle-state 0))
            (t (outline-hide-subtree)
               (setq outline-cycle-state 0)))))

  (defun /outline-minor-mode-hook ()
    (diminish 'outline-minor-mode " ☰") ; XXX: :diminish form does not work for some reason
    (local-set-key (kbd "H-o") outline-mode-prefix-map)
    (local-set-key (kbd "C-<tab>") 'outline-cycle))
  )


;;; comint
(setq comint-input-ignoredups t)


;;; icomplete mode
;;; Pretty cool with Emacs 27 flex matching and fido-mode, but I am unlikely to
;;; use it. It overrides behavior everywhere in the minibuffer. Unless it can be
;;; selectively enabled like Ido can be (e.g., for buffer operations), it is a
;;; little too intrusive. The same applies for global ivy-mode and
;;; selectrum-mode.
;;; Also, there is no case-insensitive matching?
;;(add-to-list 'completion-styles 'flex)
;;(setq icomplete-show-matches-on-no-input t
;;      icomplete-hide-common-prefix nil
;;      icomplete-prospects-height 1)
;;
;;(icomplete-mode 1)


;;; ido mode
(setq ido-everywhere nil
      ido-auto-merge-work-directories-length -1 ; don't look for files in strange places
      ido-confirm-unique-completion t
      ido-enable-flex-matching t
      ido-default-buffer-method 'samewindow
      ido-max-prospects 20
      ido-ignore-buffers ignore-buffers)

;;(ido-mode 'buffer)
;;
;;(global-set-key (kbd "C-x M-f") 'ido-find-file)
;;(global-set-key (kbd "C-x M-d") 'ido-dired)
;;(global-set-key (kbd "C-x C-d") 'ido-dired)
;;(global-set-key (kbd "C-x M-i") 'ido-insert-file)


;;; Gnus
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


;;; term-mode
;; term-suppress-hard-newline is an interesting animal. With normal term use, it
;; seems to be better kept as t. Otherwise, it generates hard newlines whenever
;; terminal output wraps around the screen. However, GNU screen doesn't like t,
;; it needs nil. But tmux works perfectly well with t. See
;; https://stackoverflow.com/questions/24517172/is-there-a-way-to-make-regions-in-term-modes-respect-line-wrapping/48634830.
(setq term-suppress-hard-newline t)

(defun /term-mode-hook ()
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
  (define-key term-raw-map (kbd "C-M-o") 'ace-window))

(add-hook 'term-mode-hook #'/term-mode-hook)


;;; text-mode
(defun /text-mode-hook ()
  (when (string-equal major-mode "text-mode")
    (visual-line-mode)))

(add-hook 'text-mode-hook #'/text-mode-hook)


;;; tex-mode
(defun /tex-mode-hook ()
  (define-key tex-mode-map (kbd "C-c C-p") nil))

(defun /latex-mode-hook ()
  (visual-line-mode))

(add-hook 'tex-mode-hook #'/tex-mode-hook)
(add-hook 'latex-mode-hook #'/latex-mode-hook)


;;; css-mode
(add-to-list 'auto-mode-alist '("\\.sass$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(setq css-indent-offset 2)


;;; ruby-mode
(defun /ruby-mode-hook ()
  (subword-mode)
  (define-key ruby-mode-map (kbd "C-m") 'newline-and-indent)
  (define-key ruby-mode-map (kbd "M-,") 'pop-tag-mark)
  (setq ruby-insert-encoding-magic-comment nil)
  (setq show-trailing-whitespace t))

(add-hook 'ruby-mode-hook #'/ruby-mode-hook)


;;; sh-mode (shell scripts)
(add-to-list 'auto-mode-alist '("zshrc" . sh-mode))

(defun /sh-mode ()
  (local-unset-key (kbd "C-c C-z")))

(add-hook 'sh-mode #'/sh-mode)


;;; nxml-mode
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng$" . nxml-mode))


;;; python-mode
(setq python-python-command "python2.7")

(defun /python-mode-hook ()
  (setq show-trailing-whitespace t)
  (define-key python-mode-map (kbd "C-c C-z") nil)
  (define-key python-mode-map (kbd "C-c C-p") nil)
  (subword-mode)
  (defconst python-block-pairs
    '(("else" "if" "elif" "while" "for" "try" "except")
      ("elif" "if" "elif")
      ;; fix finally - try - except indentation
      ("except" "try" "except")
      ("finally" "try" "except"))))

(add-hook 'python-mode-hook #'/python-mode-hook)


;;; js-mode (JavaScript, separate configuration from js2-mode)
(use-feature js
  :custom
  (js-indent-level 2)
  (js-chain-indent t))


;;; sh-script (shell script)
(use-feature sh-script
  :hook
  (sh-mode . flymake-mode)
  )


;;; cc-mode (C, C++, Objective-C)
(defun /c-mode-common-hook ()
  (c-set-style "ellemtel")
  (local-set-key (kbd "C-m") 'newline-and-indent)
  (local-set-key (kbd "C-M-x") 'compile)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (local-unset-key (kbd "M-j"))
  (electric-indent-local-mode t)
  (c-toggle-auto-newline -1)
  (subword-mode)
  (setq show-trailing-whitespace t))

(add-hook 'c-mode-common-hook #'/c-mode-common-hook)

(defun /objc-mode-hook ()
  (setq c-basic-offset 4))

(add-hook 'objc-mode-hook #'/objc-mode-hook)

(defun /c++-mode-hook ()
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
                        (if (looking-at "struct") '- '=)))))))

(add-hook 'c++-mode-hook #'/c++-mode-hook)


;;; Common Lisp
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

(defun /lisp-mode-hook ()
  (paredit-mode 1)
  ;; lisp-indent-function should be implicitly set to common-lisp-indent-function now:
  ;;(setq-local lisp-indent-function 'common-lisp-indent-function)
  (define-key lisp-mode-map (kbd "C-m") 'newline-and-indent)
  ;;(define-key lisp-mode-map (kbd "C-.") 'slime-complete-symbol)
  (setq show-trailing-whitespace t)
  ;; fix loop macro indentation
  (setq-local lisp-simple-loop-indentation 1)
  (setq-local lisp-loop-keyword-indentation 6)
  (setq-local lisp-loop-forms-indentation 3)
  ;; make square brackets and parentheses equivalent for indentation
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table))

(add-hook 'lisp-mode-hook #'/lisp-mode-hook)


;;; ----------------------------------------------------------------------------
;;; finish up
;;; ----------------------------------------------------------------------------

;;; load more startup files; order matters!
(let ((startup-files (list
                      "packages.el"
                      "utils.el"
                      "interactives.el"
                      "org.el"
                      "elisp.el"
                      "themes.el"
                      "modeline.el"
                      "ngc.el"
                      "casual.el"
                      ;;"helm.el"
                      ;;"ivy.el"
                      ;;"selectrum.el"
                      "../private/emacs-private.el")))
  (dolist (f startup-files)
    (load-file (expand-file-name (concat "../../" f) (file-truename user-emacs-directory)))))

;; reset normal garbage collection limits (disabled in early-init.el)
(setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)


;;; ----------------------------------------------------------------------------
;;;             better to reign in hell, than to serve in heaven
;;; ----------------------------------------------------------------------------
