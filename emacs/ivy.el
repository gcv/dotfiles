(use-package counsel
  :pin melpa
  :bind (("M-x" . counsel-M-x)
         ("C-x C-M-f" . counsel-find-file)
         ;;("C-M-y" . counsel-yank-pop)
         ("M-i" . counsel-imenu)
         )
  :config (progn

            ;; XXX: This gets clobbered when counsel loads. @@
            (setq ivy-initial-inputs-alist nil)

            (add-to-list 'ivy-re-builders-alist '(counsel-M-x . ivy--regex-fuzzy))

            (add-to-list 'ivy-display-functions-alist '(counsel-M-x . /ivy-display-function-window))
            (add-to-list 'ivy-display-functions-alist '(counsel-yank-pop . /ivy-display-function-window))
            (add-to-list 'ivy-display-functions-alist '(counsel-ag . /ivy-display-function-window))

            (ivy-prescient-mode 1)

            ))


(use-package counsel-projectile
  :pin melpa
  :defer t
  :config (progn
            ;; To turn on, change :defer to nil.
            ;;(counsel-projectile-mode 1)
            ))


(use-package counsel-tramp
  :pin melpa
  :defer t)


(use-package ivy
  :pin melpa
  :after (flx)
  :bind (("C-c c r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-m" . ivy-alt-done)         ; enter navigates into a directory
         )
  :config (progn

            ;;(ivy-mode 1)

            (setq ivy-ignore-buffers ignore-buffers)

            (setq ivy-initial-inputs-alist nil)

            ;; Automatic Ivy window sizing with ivy-posframe awareness.
            (defun /ivy-height-smart ()
              (if (display-graphic-p)
                  (if (bound-and-true-p mini-frame-mode)
                      (a-get mini-frame-show-parameters 'height)
                    (if (eq '/ivy-display-function-window ivy--display-function)
                        (window-height)
                      (if (> (frame-height) 55)
                          (round (/ (frame-height) 2.5))
                        (round (/ (frame-height) 1.75)))))
                ;; non-graphic display
                10))

            ;; Full-window Ivy selection (requires /ivy-height-smart).
            (defun /ivy-display-function-window (text)
              (let ((buffer (get-buffer-create "*ivy-candidates*"))
                    (str (with-current-buffer (get-buffer-create " *Minibuf-1*")
                           (let ((point (point))
                                 (string (concat (buffer-string) "  " text)))
                             (ivy-add-face-text-property (- point 1) point 'ivy-cursor string t)
                             string))))
                (with-current-buffer buffer
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert str)
                    (goto-char (point-min))
                    (setq-local cursor-type nil)))
                (with-ivy-window
                  (display-buffer
                   buffer
                   `((display-buffer-reuse-window
                      display-buffer-below-selected)
                     (window-height . ,(ivy--height (ivy-state-caller ivy-last))))))))

            (defun /ivy-display-function-window-cleanup ()
              (when-let ((ivy-candidate-buf (get-buffer "*ivy-candidates*")))
                (kill-buffer ivy-candidate-buf)))

            (add-hook 'minibuffer-exit-hook #'/ivy-display-function-window-cleanup)

            (setq ivy-height-alist
                  '((swiper . 10)
                    (t . (lambda (_caller)
                           ;; XXX: This needs to match /ivy-posframe-display-smart.
                           (/ivy-height-smart)))))

            (setq ivy-re-builders-alist
                  '((swiper . ivy--regex-plus)
                    (t . ivy--regex-plus)))

            (setq ivy-display-functions-alist
                  '((ivy-completion-in-region . ivy-display-function-overlay)
                    (t . nil)))

            ))


(use-package ivy-posframe
  :pin melpa
  :defer nil                            ; must load eagerly
  :diminish ""
  :config (progn

            (ivy-posframe-mode 1)

            (setq ivy-posframe-width 65
                  ivy-posframe-min-width 65
                  ivy-posframe-height nil
                  ivy-posframe-border-width 1
                  ivy-posframe-parameters '((left-fringe . 0)
                                            (right-fringe . 0)))

            (set-face-attribute 'ivy-posframe-cursor nil :inherit 'ivy-cursor)
            (set-face-attribute 'ivy-posframe nil :foreground nil :background nil :inherit 'default)

            ;; Automatically place the posframe centered in the current window
            ;; if large enough, or in the frame as a fallback.
            (defun /ivy-posframe-display-smart (str)
              (if (or (< (window-total-width (with-ivy-window (selected-window)))
                         ivy-posframe-width)
                      (< (window-total-height (with-ivy-window (selected-window)))
                         ;; XXX: This needs to match ivy-height-alist.
                         (/ivy-height-smart)))
                  (ivy-posframe-display-at-frame-center str)
                (ivy-posframe-display-at-window-center str)))

            (setq ivy-posframe-display-functions-alist
                  '(;;(swiper . ivy-posframe-display-at-window-bottom-left)
                    (swiper . nil)
                    (complete-symbol . ivy-posframe-display-at-point)
                    (counsel-M-x . /ivy-posframe-display-smart)
                    (counsel-yank-pop . /ivy-display-function-window)
                    ;;(t . ivy-posframe-display)
                    (t . /ivy-posframe-display-smart)))

            ))


(use-package ivy-prescient
  :defer t)


(use-package ivy-xref
  :pin melpa
  :defer t
  :init (progn
          (when (>= emacs-major-version 27)
            (setq xref-show-definitions-function #'ivy-xref-show-defs))
          (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))


(use-package swiper
  :pin melpa
  :bind ("M-S-C-s" . swiper))
