(with-eval-after-load "org"

  (require 'org-crypt)
  (require 'org-clock)
  (require 'org-tempo)

  (require 'ob-C)
  (require 'ob-R)
  (require 'ob-awk)
  (require 'ob-calc)
  (require 'ob-emacs-lisp)
  (require 'ob-gnuplot)
  (require 'ob-ledger)
  (require 'ob-lilypond)
  (require 'ob-lua)
  (require 'ob-octave)
  (require 'ob-octave)
  (require 'ob-perl)
  (require 'ob-python)
  (require 'ob-ruby)
  (require 'ob-shell)
  (require 'ob-sql)
  (require 'ob-sqlite)

  (require 'ox-md)
  (require 'ox-texinfo)
  (require 'ox-beamer)

  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.note$" . org-mode))

  (setq org-directory "~/Notes")

  ;;(setq org-agenda-files '("~/Notes/Tasks.note"))
  (setq org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t)

  (setq org-startup-indented t)
  (setq org-startup-folded 'nofold)
  (setq org-startup-align-all-tables t)

  (setq org-hide-leading-stars nil)
  (setq org-odd-levels-only nil)
  (setq org-alphabetical-lists t)
  (when window-system (setq org-hide-emphasis-markers t))
  (setq org-link-search-must-match-exact-headline nil)
  (setq org-replace-disputed-keys t)
  (setq org-archive-location "%s-archive::")
  (setq org-drawers '("PROPERTIES" "CLOCK" "note"))
  ;;(setq org-todo-keywords '("TODO" "|" "DONE"))
  ;;(setq org-todo-keywords '("TODO(t)" "PROGRESS(p)" "|" "DONE(d!)" "CANCELED(c@)"))
  ;;(setq org-todo-keyword-faces '(("TODO" . org-warning) ("PROGRESS" . "orange") ("CANCELED" . (:foreground "lightblue" :weight bold))))
  (setq org-deadline-warning-days 0)
  (setq org-agenda-ndays 30)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-cycle-emulate-tab 'white)
  (setq org-cycle-separator-lines 0)
  (setq org-fontify-whole-heading-line t)
  (setq org-pretty-entities t)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "*") ("*" . "-")))

  (setq org-src-fontify-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-src-preserve-indentation t)
  (setq org-confirm-babel-evaluate nil)

  (setq calendar-week-start-day 1)

  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))

  ;; open directory links in dired:
  (add-to-list 'org-file-apps '(directory . emacs))

  (defun /org-mode-hook ()
    (setq mode-name "Ω")
    (diminish 'org-indent-mode)
    (setq show-trailing-whitespace t)
    (visual-line-mode)
    ;; set the input method which makes it easy to type special characters, but
    ;; don't turn it on: a simple C-\ then toggles it with no further prompting
    (set-input-method 'TeX)
    (deactivate-input-method)
    ;; keybindings
    (define-key org-mode-map (kbd "M-+") 'org-shiftright)
    (define-key org-mode-map (kbd "M--") 'org-shiftleft)
    (define-key org-mode-map (kbd "S-<up>") nil)
    (define-key org-mode-map (kbd "S-<down>") nil)
    (define-key org-mode-map (kbd "S-<left>") nil)
    (define-key org-mode-map (kbd "S-<right>") nil)
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c C-z") nil)
    (define-key org-mode-map (kbd "C-c C-p") nil)
    (define-key org-mode-map (kbd "C-c M-z") 'org-add-note)
    (define-key org-mode-map (kbd "M-C-<up>") 'org-timestamp-up)
    (define-key org-mode-map (kbd "M-C-<down>") 'org-timestamp-down)
    (define-key org-mode-map (kbd "M-<up>") 'scroll-up-line)
    (define-key org-mode-map (kbd "M-<down>") 'scroll-down-line)
    (define-key org-mode-map (kbd "M-S-<up>") 'org-move-subtree-up)
    (define-key org-mode-map (kbd "M-S-<down>") 'org-move-subtree-down)
    (define-key org-mode-map (kbd "M-C-S-<up>") 'org-shiftmetaup)
    (define-key org-mode-map (kbd "M-C-S-<down>") 'org-shiftmetadown)
    (define-key org-mode-map (kbd "C-S-a") 'beginning-of-line)
    (define-key org-mode-map (kbd "C-S-e") 'end-of-line)
    (define-key org-mode-map (kbd "C-c C-M-l") 'org-store-link-by-id)
    ;; turn off all archiving shortcuts
    (define-key org-mode-map (kbd "C-c $") nil) ;; default: org-archive-subtree
    (define-key org-mode-map (kbd "C-c C-x C-a") nil) ;; default: org-archive-subtree-default
    (define-key org-mode-map (kbd "C-c C-x A") nil) ;; default: org-archive-to-archive-sibling
    (define-key org-mode-map (kbd "C-c C-x a") nil) ;; default: org-toggle-archive-tag
    (define-key org-mode-map (kbd "C-c C-x C-s") nil) ;; default: org-advertized-archive-subtree
    )

  (add-hook 'org-mode-hook #'/org-mode-hook)

  (defun /org-kill-calendar-buffer (&rest args)
    (let ((buf (get-buffer "*Calendar*")))
      (when buf
        (kill-buffer buf))))

  (advice-add 'org-time-stamp-inactive :after #'/org-kill-calendar-buffer)
  (advice-add 'org-time-stamp-active :after #'/org-kill-calendar-buffer)

  (defun /org-toggle-heading (&rest args)
    (end-of-line))

  (advice-add 'org-toggle-heading :after #'/org-toggle-heading)

  ;; XXX: Massive kludge. org-babel--shell-command-on-region does not respect
  ;; buffer-local exec-path because it executes in the context of a temporary
  ;; buffer which does not copy in buffer-local exec-path. It also knows nothing
  ;; about envrc-mode setting the PATH environment variable. This installs an
  ;; advice at a point that runs in the Org buffer which then installs a piece of
  ;; advice on org-babel--shell-command-on-region to do the right thing. What a
  ;; mess.
  (defun /org-ctrl-c-ctrl-c (orig-fn &rest args)
    (let* ((buf (current-buffer))
           (ep (exec-path))
           (shell-path (getenv "PATH"))
           (overrider (lambda (orig-fn &rest args)
                        (let ((exec-path ep)
                              (saved-path (getenv "PATH")))
                          (unwind-protect
                              (progn
                                (setenv "PATH" (s-join ":" ep))
                                (apply orig-fn args))
                            (setenv "PATH" saved-path))))))
      (unwind-protect
          (progn
            (advice-add 'org-babel--shell-command-on-region :around overrider)
            (apply orig-fn args))
        (advice-remove 'org-babel--shell-command-on-region overrider))))

  (advice-add 'org-ctrl-c-ctrl-c :around #'/org-ctrl-c-ctrl-c)
  (advice-remove 'org-ctrl-c-ctrl-c #'/org-ctrl-c-ctrl-c)

  )
