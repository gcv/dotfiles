;;; ngc - new generation completion -*- lexical-binding: t; -*-

;;; --- shared NGC setup: consult, embark, marginalia, orderless

(use-package consult
  :pin melpa

  :bind
  (("C-c p s r" . consult-ripgrep))

  :custom
  (consult-preview-key nil)
  ;;(consult-project-root-function #'projectile-project-root)
  (consult-project-root-function (lambda ()
                                   (when-let (project (project-current))
                                     (car (project-roots project)))))

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
  :pin melpa

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
  (advice-add #'marginalia-cycle :after #'/marginalia-cycle)
  )


(use-package orderless
  :pin melpa

  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;;(orderless-matching-styles '(orderless-literal orderless-regexp))
  )


;;; --- vertico

(use-package vertico
  :pin gnu
  ;;:quelpa (vertico :fetcher github :repo "minad/vertico")

  :init

  ;; --- vertico-on-demand-mode
  ;; derived from https://github.com/minad/vertico/blob/demand/extensions/vertico-on-demand.el
  ;; can probably be retired when and if this extension becomes an official Vertico feature

  (defvar vertico-on-demand-map
    (let ((map (make-composed-keymap nil minibuffer-local-map)))
      (define-key map [remap next-line] #'vertico-on-demand-show)
      (define-key map [remap previous-line] #'vertico-on-demand-show)
      (define-key map [remap next-line-or-history-element] #'vertico-on-demand-show)
      (define-key map [remap previous-line-or-history-element] #'vertico-on-demand-show)
      (define-key map "?" #'vertico-on-demand-show)
      (define-key map "\r" #'vertico-on-demand-complete-and-exit)
      (define-key map "\t" #'vertico-on-demand-complete)
      (define-key map [return] #'vertico-exit-input)
      (define-key map [C-return] #'vertico-exit-input)
      map)
    "Vertico on-demand minibuffer keymap derived from `minibuffer-local-map'.")

  (defun vertico-on-demand-complete ()
    "Complete minibuffer input or open Vertico UI."
    (interactive)
    (cl-letf (((symbol-function #'minibuffer-completion-help) #'vertico-on-demand-show))
      (minibuffer-complete)))

  (defun vertico-on-demand-complete-and-exit ()
    "Complete minibuffer input and exit or open Vertico UI."
    (interactive)
    (cl-letf (((symbol-function #'minibuffer-completion-help) #'vertico-on-demand-show)
              (completion-cycle-threshold nil)) ;; disable cycling; ensure unique match!
      (minibuffer-complete-and-exit)))

  (defun vertico-on-demand-show (&rest _)
    "Show Vertico UI."
    (interactive)
    (vertico--setup))

  (defun vertico-on-demand--setup ()
    "Setup Vertico on-demand mode in the minibuffer."
    (setq-local completion-show-inline-help nil
                completion-auto-help t)
    (use-local-map vertico-on-demand-map))

  (defvar vertico-on-demand-auto-this nil)

  (defcustom vertico-on-demand-auto-commands
    (list 'execute-extended-command
          'eval-expression
          "edebug-eval-expression"
          'debugger-eval-expression
          "consult-.*"
          "persp-switch-to-buffer\\*"
          "projectile-.*"
          "imenu-.*")
    "List of commands which automatically activate Vertico, bypassing on-demand mode."
    :group 'vertico
    :type '(repeat (choice function regexp)))

  (defun vertico-on-demand--completion-at-point-advice (orig-fn &rest args)
    (let ((vertico-on-demand-auto-this t)
          ;; consult-completion-in-region makes completion work in minibuffer
          ;; commands like eval-expression (where company-mode is not
          ;; supported), as well as ielm
          (completion-in-region-function 'consult-completion-in-region))
      (apply orig-fn args)))

  (defun vertico-on-demand--advice (&rest args)
    "Advice for completion function, receiving ARGS."
    (minibuffer-with-setup-hook
        (if (or vertico-on-demand-auto-this
                (and (symbolp this-command)
                     (catch 'auto
                       (dolist (auto-command vertico-on-demand-auto-commands)
                         (when (if (stringp auto-command)
                                   (string-match-p auto-command (symbol-name this-command))
                                 (eq auto-command this-command))
                           (throw 'auto t))))))
            #'vertico--setup
          #'vertico-on-demand--setup)
      (apply args)))

  (define-minor-mode vertico-on-demand-mode
    "Open Vertico on demand."
    :global t
    :group 'vertico
    (if vertico-on-demand-mode
        (progn
          (advice-add #'vertico--advice :override #'vertico-on-demand--advice)
          (advice-add #'completion-at-point :around #'vertico-on-demand--completion-at-point-advice))
      (advice-remove #'completion-at-point #'vertico-on-demand--completion-at-point-advice)
      (advice-remove #'vertico--advice #'vertico-on-demand--advice)))

  ;; --- end of vertico-on-demand-mode

  (vertico-mode 1)
  (vertico-on-demand-mode 1)
  (when window-system
    (selective-mini-frame-mode 1))
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
