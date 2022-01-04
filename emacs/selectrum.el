;;; -*- lexical-binding: t; -*-

;;; This configuration is superceded by Vertico, and retained here for
;;; reference.


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
     (no-accept-focus . t)              ; XXX: see https://github.com/minad/vertico/issues/115 and no-accept-focus hack below
     (left-fringe . 5)
     (right-fringe . 5)))
  (mini-frame-resize-max-height 15)

  :commands (mini-frame-read-from-minibuffer mini-frame--advice)

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
          (let* ((mini-frame-height (or (ignore-errors (frame-parameter mini-frame-frame 'height)) 10))
                 ;; not sure why these values have to be fudged by 1, let alone why in different directions
                 (selectrum-num-candidates-displayed (- mini-frame-height 1))
                 (vertico-count (+ mini-frame-height 1)))
            (add-to-list 'mini-frame-show-parameters '(no-accept-focus . t)) ; XXX: weirdly this disappears
            (apply orig-fn args)))
      (when (and window-system (not mini-frame-active))
        (mini-frame--advice mini-frame-advice-functions #'mini-frame-read-from-minibuffer t)
        (mini-frame--advice mini-frame-ignore-functions #'mini-frame--ignore-function t)
        (advice-remove 'minibuffer-selected-window #'mini-frame--minibuffer-selected-window)
        ;; (advice-remove 'find-file-read-args #'mini-frame-read-from-minibuffer)
        ;; (advice-remove 'read-from-minibuffer #'mini-frame-read-from-minibuffer)
        ))))

(defun selective-mini-frame-persp-switch-to-buffer* ()
  (interactive)
  (call-interactively 'persp-switch-to-buffer*))

(defvar selective-mini-frame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x M-b") #'selective-mini-frame-persp-switch-to-buffer*)
    map))

(define-minor-mode selective-mini-frame-mode
  "Selectively enable mini-frame-mode for some commands."
  :global t
  :init-value nil
  :lighter nil
  :keymap selective-mini-frame-mode-map
  (if selective-mini-frame-mode
      ;; enable
      (progn
        (advice-add 'read-extended-command :around #'selective-mini-frame)
        (advice-add 'imenu-cr :around #'selective-mini-frame)
        (advice-add 'selective-mini-frame-persp-switch-to-buffer* :around #'selective-mini-frame)
        (advice-add 'projectile-find-file :around #'selective-mini-frame))
    ;; disable
    (advice-remove 'projectile-find-file #'selective-mini-frame)
    (advice-remove 'selective-mini-frame-persp-switch-to-buffer* #'selective-mini-frame)
    (advice-remove 'imenu-cr #'selective-mini-frame)
    (advice-remove 'read-extended-command #'selective-mini-frame)))


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
