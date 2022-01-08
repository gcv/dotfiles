;;; ngc - new generation completion -*- lexical-binding: t; -*-

(use-package consult
  :pin gnu

  :bind
  (("C-M-y" . consult-yank-from-kill-ring)
   ("C-c p s r" . consult-ripgrep))

  :custom
  (consult-preview-key nil)
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

  ;; (advice-add #'completing-read-multiple
  ;;             :override #'consult-completing-read-multiple)
  )


(use-package embark
  :pin melpa

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
  :pin melpa
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
         ))

  ;;tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)

  :custom
  (vertico-count (if window-system 16 10))
  (vertico-buffer-display-action '(display-buffer-same-window))

  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)

  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          ;;("^consult-.*" buffer indexed)
          (imenu-cr buffer indexed)
          (find-file-in-project buffer indexed grid)
          ;;(find-file-in-project buffer indexed (:not grid))
          ("persp-.*" flat)
          ))
  (if window-system
      (add-to-list 'vertico-multiform-commands '(execute-extended-command indexed posframe))
    (add-to-list 'vertico-multiform-commands '(execute-extended-command unobtrusive)))

  (setq vertico-multiform-categories
        '(;;(file grid)
          ;;(file flat (vertico-cycle . t))
          (file unobtrusive)
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
