(require 'org)
(require 'org-crypt)
(require 'org-clock)

(require 'ob-python)
(require 'ob-ruby)
(require 'ob-C)
(require 'ob-ledger)
(require 'ob-gnuplot)
(require 'ob-calc)
(require 'ob-sh)
(require 'ob-sqlite)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.note$" . org-mode))

(setq org-directory "~/Notes")
;;(setq org-agenda-files '("~/Notes/Tasks.note"))

(setq org-startup-indented t)
(setq org-startup-folded 'nofold)
(setq org-hide-leading-stars nil)
(setq org-odd-levels-only nil)
(setq org-alphabetical-lists t)
(setq org-hide-emphasis-markers t)
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
(setq org-fontify-whole-heading-line t)

(setq org-src-fontify-natively t)
(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)

(setq calendar-week-start-day 1)

(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(add-hook 'org-mode-hook
  (lambda ()
    (setq mode-name "Ω")
    (diminish 'org-indent-mode)
    (setq show-trailing-whitespace t)
    (visual-line-mode)
    (define-key org-mode-map (kbd "M-+") 'org-shiftright)
    (define-key org-mode-map (kbd "M--") 'org-shiftleft)
    (define-key org-mode-map (kbd "S-<up>") nil)
    (define-key org-mode-map (kbd "S-<down>") nil)
    (define-key org-mode-map (kbd "S-<left>") nil)
    (define-key org-mode-map (kbd "S-<right>") nil)
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c C-z") nil)
    (define-key org-mode-map (kbd "C-c M-z") 'org-add-note)
    (define-key org-mode-map (kbd "M-C-<up>") 'org-timestamp-up)
    (define-key org-mode-map (kbd "M-C-<down>") 'org-timestamp-down)
    (define-key org-mode-map (kbd "M-<up>") 'scroll-up-line)
    (define-key org-mode-map (kbd "M-<down>") 'scroll-down-line)
    (define-key org-mode-map (kbd "C-S-a") 'beginning-of-line)
    (define-key org-mode-map (kbd "C-S-e") 'end-of-line)
    ;; turn off all archiving shortcuts
    (define-key org-mode-map (kbd "C-c $") nil) ;; default: org-archive-subtree
    (define-key org-mode-map (kbd "C-c C-x C-a") nil) ;; default: org-archive-subtree-default
    (define-key org-mode-map (kbd "C-c C-x A") nil) ;; default: org-archive-to-archive-sibling
    (define-key org-mode-map (kbd "C-c C-x a") nil) ;; default: org-toggle-archive-tag
    (define-key org-mode-map (kbd "C-c C-x C-s") nil) ;; default: org-advertized-archive-subtree
    ))

(defun cv--org-kill-calendar-buffer (&rest args)
  (let ((buf (get-buffer "*Calendar*")))
    (when buf
      (kill-buffer buf))))

(advice-add 'org-time-stamp-inactive :after #'cv--org-kill-calendar-buffer)
(advice-add 'org-time-stamp-active :after #'cv--org-kill-calendar-buffer)

(defun cv--org-toggle-heading (&rest args)
  (end-of-line))

(advice-add 'org-toggle-heading :after #'cv--org-toggle-heading)
