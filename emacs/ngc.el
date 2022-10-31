;;; ngc - new generation completion -*- lexical-binding: t; -*-

(use-package consult
  :pin gnu

  :bind
  (("C-M-y" . consult-yank-from-kill-ring)
   ("C-M-s" . consult-line)
   ("C-c p s r" . consult-ripgrep))

  :custom
  ;;(consult-preview-key nil) ; disable Consult preview
  ;;(consult-project-root-function #'projectile-project-root)
  (consult-project-root-function (lambda ()
                                   (when-let (project (project-current))
                                     (car (project-roots project)))))
  (completion-in-region-function (lambda (&rest args)
                                   (apply (if vertico-mode
                                              #'consult-completion-in-region
                                            #'completion--in-region)
                                          args)))

  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source) ; Perspective integration

  (consult-customize consult-theme :preview-key '(:debounce 0.5 any))

  ;; turn off preview for a bunch of modes that don't play well with
  ;; vertico-buffer-display-action set to '(display-buffer-same-window)
  (consult-customize consult-ripgrep consult-git-grep consult-grep :preview-key nil)
  (consult-customize consult-imenu :preview-key nil)
  )


(use-package consult-notes
  :commands (consult-notes consult-notes-search-in-all-notes)
  :config
  (setq consult-notes-sources
        '(("Denote" ?n "~/Files/Notes/Denote/")
          ("Deft" ?f "~/Files/Notes/Deft/")
          ("Computing"  ?c "~/Files/Notes/Deft/Computing/")))
  (add-to-list 'vertico-multiform-commands '(consult-notes buffer))
  )


(use-package corfu
  :pin gnu

  :custom
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)

  :init
  (global-corfu-mode 1)
  (setq tab-always-indent 'complete)
  )


(use-package corfu-doc
  :pin melpa
  ;;:hook (corfu-mode . corfu-doc-mode)
  :bind
  (:map corfu-map
        ("M-d" . corfu-doc-toggle))
  )


(use-package cape
  :pin gnu

  :bind
  (("C-M-/" . cape-dabbrev))

  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )


(use-package embark
  :pin gnu

  :bind
  (("C-," . embark-act)
   ("C-c ," . embark-act)
   ("C-M-," . embark-dwim)
   ("C-c M-," . embark-dwim))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; use which-key for Embark prompts
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  )


(use-package embark-consult
  :pin gnu
  :after (embark consult)
  :demand t                             ; required!
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )


(use-package marginalia
  :pin gnu

  :bind
  (:map minibuffer-local-map
        ("C-'" . marginalia-cycle))

  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode 1)

  :config
  (defun /marginalia-cycle ()
    (let ((inhibit-message t))
      (customize-save-variable 'marginalia-annotator-registry
                               marginalia-annotator-registry)))
  (advice-add 'marginalia-cycle :after #'/marginalia-cycle)
  )


(use-package orderless
  :pin melpa

  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;;(orderless-matching-styles '(orderless-literal orderless-regexp))
  )


(use-package vertico
  :pin gnu
  ;;:quelpa (vertico :fetcher github :repo "minad/vertico" :files (:defaults "extensions/*"))

  :bind
  (("C-x b" . persp-switch-to-buffer*)
   ("C-c r" . vertico-repeat)
   (:map vertico-map
         ("TAB" . /vertico-smart-tab)
         ("M-g" . vertico-multiform-grid)
         ("M-f" . vertico-multiform-flat)
         ("M-R" . vertico-multiform-reverse)
         ("M-u" . vertico-multiform-unobtrusive)
         ;; emulate ido-mode directory navigation
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ;; consult
         ("C-M-r" . consult-history)
         ))

  ;; tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)

  :custom
  (vertico-count (if window-system 16 10))
  (vertico-buffer-display-action '(display-buffer-same-window))

  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)

  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (imenu-cr buffer indexed)
          (project-find-file buffer indexed)
          ("persp-.*" flat)
          ("cider-.*" flat)
          ))
  (if window-system
      (add-to-list 'vertico-multiform-commands '(execute-extended-command indexed posframe))
    (add-to-list 'vertico-multiform-commands '(execute-extended-command unobtrusive)))

  (setq vertico-multiform-categories
        '((file unobtrusive)
          (buffer flat (vertico-cycle . t))
          (consult-grep buffer)
          ))

  (defun /vertico-smart-tab ()
    (interactive)
    (if (or (not vertico-unobtrusive-mode)
            (= vertico--total 1))
        (call-interactively #'vertico-insert)
      (vertico-multiform-unobtrusive)))

  ;; Sample use:
  ;; (setq vertico-multiform-commands
  ;;       '(("\\`consult-" buffer)
  ;;         (t disabled)))
  (define-minor-mode vertico-disabled-mode
    "Disable Vertico."
    :global t
    :group 'vertico
    ;; shrink current minibuffer window
    (when-let (win (active-minibuffer-window))
      (window-resize win (- (window-pixel-height win)) nil nil 'pixelwise))
    (if vertico-disabled-mode
        (advice-add 'vertico--setup :override #'ignore)
      (advice-remove 'vertico--setup #'ignore)))

  ;; tmm-menubar sucks, but just in case it's used:
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)

  ;; find-file-at-point advice:
  (advice-add #'ffap-menu-ask :around (lambda (&rest args)
                                        (cl-letf (((symbol-function #'minibuffer-completion-help)
                                                   #'ignore))
                                          (apply args))))

  ;; enable repeat
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  )

(use-package vertico-posframe
  :pin gnu
  ;;:quelpa (vertico-posframe :fetcher github :repo "tumashu/vertico-posframe")

  :if window-system

  :custom
  (vertico-posframe-border-width 1)
  (vertico-posframe-poshandler
   (lambda (info)
     (cons (/ (- (plist-get info :parent-frame-width)
                 (plist-get info :posframe-width))
              2)
           (/ (- (plist-get info :parent-frame-height)
                 (plist-get info :posframe-height))
              5))))
  (vertico-posframe-parameters
   '((left-fringe . 0)
     (right-fringe . 0)))
  )
