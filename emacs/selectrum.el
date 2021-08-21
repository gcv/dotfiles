(use-package selectrum
  :pin melpa

  :custom
  (selectrum-display-style '(vertical))

  ;;:init
  ;;(selectrum-mode 1)

  :config
  (when window-system
    ;; match mini-frame height - 1
    (setq selectrum-num-candidates-displayed 14))

  selectrum-display-style

  (selectrum-prescient-mode 1)
  )


(use-package selectrum-prescient
  :pin melpa
  :commands (selectrum-prescient-mode))


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


(use-package consult
  :pin melpa

  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
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
  ;; :demand t
  ;; :hook
  ;; (embark-collect-mode . consult-preview-at-point-mode)
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


(defun selective-selectrum-base (orig-fn &rest args)
  (let ((selectrum-active selectrum-mode))
    (unwind-protect
        (progn
          (selectrum-mode 1)
          (apply orig-fn args))
      (unless selectrum-active (selectrum-mode -1)))))

(defun selective-selectrum-mini-frame (orig-fn &rest args)
  (let ((mini-frame-active mini-frame-mode))
    ;; This should not just enable and disable mini-frame-mode because that has
    ;; a significant performance penalty for destroying and recreating the
    ;; frame.
    (unwind-protect
        (progn
          (when (and window-system (not mini-frame-active))
            (advice-add 'read-from-minibuffer :around #'mini-frame-read-from-minibuffer)
            (advice-add 'find-file-read-args :around #'mini-frame-read-from-minibuffer))
          (apply orig-fn args))
      (when (and window-system (not mini-frame-active))
        (advice-remove 'find-file-read-args #'mini-frame-read-from-minibuffer)
        (advice-remove 'read-from-minibuffer #'mini-frame-read-from-minibuffer)))))

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
        (advice-add 'read-extended-command :around #'selective-selectrum-mini-frame)
        (advice-add 'selective-selectrum-find-file :around #'selective-selectrum-base)
        (advice-add 'selective-selectrum-find-file :around #'selective-selectrum-mini-frame)
        (advice-add 'imenu-cr :around #'selective-selectrum-base)
        (advice-add 'imenu-cr :around #'selective-selectrum-mini-frame)
        ;; XXX: consult-* commands die when called with M-x. This seems to be a
        ;; mini-frame-mode bug. They do, however, require Consult.
        (cl-loop for sym the symbols of obarray
                 if (and (string-prefix-p "consult-" (symbol-name sym))
                         (not (string-match-p "--" (symbol-name sym))))
                 do (advice-add sym :around #'selective-selectrum-base)))
    ;; disable
    (cl-loop for sym the symbols of obarray
             if (and (string-prefix-p "consult-" (symbol-name sym))
                     (not (string-match-p "--" (symbol-name sym))))
             do (advice-remove sym #'selective-selectrum-base))
    (advice-remove 'imenu-cr #'selective-selectrum-mini-frame)
    (advice-remove 'imenu-cr #'selective-selectrum-base)
    (advice-remove 'selective-selectrum-find-file #'selective-selectrum-mini-frame)
    (advice-remove 'selective-selectrum-find-file #'selective-selectrum-base)
    (advice-remove 'read-extended-command #'selective-selectrum-mini-frame)
    (advice-remove 'read-extended-command #'selective-selectrum-base)))

(selective-selectrum-mode 1)
