;;; ngc - new generation completion -*- lexical-binding: t; -*-

;;; --- mini-frame

(use-package mini-frame
  :pin melpa
  :if window-system

  :custom
  (mini-frame-detach-on-hide nil)       ; workaround for hidden frames showing up
  (mini-frame-resize nil)               ; a bit disconcerting when t
  (mini-frame-show-parameters
   '((top . 100)
     (left . 0.5)
     (height . 15)                      ; explicit even with mini-frame-resize t
     (width . 0.7)
     (left-fringe . 5)
     (right-fringe . 5)))
  (mini-frame-resize-max-height 15)

  :commands (mini-frame-read-from-minibuffer)

  ;;:init
  ;;(mini-frame-mode 1)

  :config
  (add-to-list 'mini-frame-ignore-commands 'find-alternate-file)
  (add-to-list 'mini-frame-ignore-commands 'kill-buffer)
  (add-to-list 'mini-frame-ignore-commands 'switch-to-buffer)
  (add-to-list 'mini-frame-ignore-commands "consult-.*")
  (add-to-list 'mini-frame-ignore-commands "helm-.*")
  (add-to-list 'mini-frame-ignore-commands "ido-.*")
  (add-to-list 'mini-frame-ignore-commands "magit-.*")
  (add-to-list 'mini-frame-ignore-commands "persp-.*")
  )


;; --- selective mini-frame

(defun selective-mini-frame (orig-fn &rest args)
  "(advice-add 'cmd :around #'selective-mini-frame) activates mini-frame for cmd."
  (let ((mini-frame-active mini-frame-mode))
    ;; This should not just enable and disable mini-frame-mode because that has
    ;; a significant performance penalty for destroying and recreating the
    ;; frame.
    (unwind-protect
        (progn
          (when (and window-system (not mini-frame-active))
            (mini-frame--advice mini-frame-advice-functions #'mini-frame-read-from-minibuffer)
            (mini-frame--advice mini-frame-ignore-functions #'mini-frame--ignore-function)
            (advice-add 'minibuffer-selected-window :around #'mini-frame--minibuffer-selected-window)
            ;; (advice-add 'read-from-minibuffer :around #'mini-frame-read-from-minibuffer)
            ;; (advice-add 'find-file-read-args :around #'mini-frame-read-from-minibuffer)
            )
          (apply orig-fn args))
      (when (and window-system (not mini-frame-active))
        (mini-frame--advice mini-frame-advice-functions #'mini-frame-read-from-minibuffer t)
        (mini-frame--advice mini-frame-ignore-functions #'mini-frame--ignore-function t)
        (advice-remove 'minibuffer-selected-window #'mini-frame--minibuffer-selected-window)
        ;; (advice-remove 'find-file-read-args #'mini-frame-read-from-minibuffer)
        ;; (advice-remove 'read-from-minibuffer #'mini-frame-read-from-minibuffer)
        ))))

(define-minor-mode selective-mini-frame-mode
  "Selectively enable mini-frame-mode for some commands."
  :global t
  :init-value nil
  :lighter nil
  (if selective-mini-frame-mode
      ;; enable
      (progn
        (require 'mini-frame)
        (advice-add 'read-extended-command :around #'selective-mini-frame)
        (advice-add 'imenu-cr :around #'selective-mini-frame))
    ;; disable
    (advice-remove 'imenu-cr #'selective-mini-frame)
    (advice-remove 'read-extended-command #'selective-mini-frame)))


;;; --- shared NGC setup: consult, embark, marginalia, orderless

(use-package consult
  :pin melpa

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
   ("C-M-," . embark-dwim))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
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
  (completion-styles '(orderless))
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
    (list 'execute-extended-command "consult-.*" "imenu-.*")
    "List of commands which automatically activate Vertico, bypassing on-demand mode."
    :group 'vertico
    :type '(repeat (choice function regexp))
    )

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
        (advice-add #'vertico--advice :override #'vertico-on-demand--advice)
      (advice-remove #'vertico--advice #'vertico-on-demand--advice)))

  ;; --- end of vertico-on-demand-mode

  (vertico-mode 1)
  (vertico-on-demand-mode 1)
  (when window-system
    (selective-mini-frame-mode 1))

  :config
  (when window-system
    ;; match mini-frame height - 1
    (setq vertico-count 14))
  )


;;; --- selectrum

(use-package prescient
  :pin melpa

  :config
  ;;(setq prescient-aggressive-file-save t)
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode 1)
  )


(use-package selectrum
  :pin melpa

  :custom
  (selectrum-display-style '(vertical))

  ;;:init
  ;;(selectrum-mode 1)
  ;;(selective-selectrum-mode 1)

  :config
  (when window-system
    ;; match mini-frame height - 1
    (setq selectrum-num-candidates-displayed 14))

  (selectrum-prescient-mode 1)
  )


(use-package selectrum-prescient
  :pin melpa
  :commands (selectrum-prescient-mode))


;;; --- selective-selectrum-mode

(defun selective-selectrum-base (orig-fn &rest args)
  (let ((selectrum-active selectrum-mode))
    (unwind-protect
        (progn
          (selectrum-mode 1)
          (apply orig-fn args))
      (unless selectrum-active (selectrum-mode -1)))))

(defun selective-selectrum-find-file ()
  (interactive)
  (call-interactively 'find-file))

(defvar selective-selectrum-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-M-f") #'selective-selectrum-find-file)
    map))

(define-minor-mode selective-selectrum-mode
  "Selectively enable Selectrum or mini-frame-mode for some commands."
  :global t
  :init-value nil
  :lighter nil
  :keymap selective-selectrum-mode-map
  (if selective-selectrum-mode
      ;; enable
      (progn
        (advice-add 'read-extended-command :around #'selective-selectrum-base)
        (advice-add 'read-extended-command :around #'selective-mini-frame)
        (advice-add 'selective-selectrum-find-file :around #'selective-selectrum-base)
        (advice-add 'selective-selectrum-find-file :around #'selective-mini-frame)
        (advice-add 'imenu-cr :around #'selective-selectrum-base)
        (advice-add 'imenu-cr :around #'selective-mini-frame)
        ;; XXX: consult-* commands die when called with M-x. This seems to be a
        ;; mini-frame-mode bug. They do, however, require Selectrum.
        (cl-loop for sym the symbols of obarray
                 if (and (string-prefix-p "consult-" (symbol-name sym))
                         (not (string-match-p "--" (symbol-name sym))))
                 do (advice-add sym :around #'selective-selectrum-base)))
    ;; disable
    (cl-loop for sym the symbols of obarray
             if (and (string-prefix-p "consult-" (symbol-name sym))
                     (not (string-match-p "--" (symbol-name sym))))
             do (advice-remove sym #'selective-selectrum-base))
    (advice-remove 'imenu-cr #'selective-mini-frame)
    (advice-remove 'imenu-cr #'selective-selectrum-base)
    (advice-remove 'selective-selectrum-find-file #'selective-mini-frame)
    (advice-remove 'selective-selectrum-find-file #'selective-selectrum-base)
    (advice-remove 'read-extended-command #'selective-mini-frame)
    (advice-remove 'read-extended-command #'selective-selectrum-base)))

