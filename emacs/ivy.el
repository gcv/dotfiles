(global-set-key (kbd "C-x C-M-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-i") 'counsel-imenu)
;;(global-set-key (kbd "C-M-y") 'counsel-yank-pop)

(global-set-key (kbd "C-x C-M-b") 'persp-ivy-switch-buffer)
(global-set-key (kbd "C-c c r") 'ivy-resume)

(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done) ; enter navigates into a directory


(setq ivy-initial-inputs-alist nil)

(setq ivy-ignore-buffers ignore-buffers)

(setq ivy-posframe-width 65
      ivy-posframe-min-width 65
      ivy-posframe-height nil
      ivy-posframe-border-width 1
      ivy-posframe-parameters '((left-fringe . 0)
                                (right-fringe . 0)))

(set-face-attribute 'ivy-posframe-cursor nil :inherit 'ivy-cursor)
(set-face-attribute 'ivy-posframe nil :foreground nil :background nil :inherit 'default)


;;; Automatic Ivy window sizing with ivy-posframe awareness.
(defun /ivy-height-smart ()
  (if (display-graphic-p)
      (if (eq '/ivy-display-function-window ivy--display-function)
          (window-height)
        (if (> (frame-height) 55)
            (round (/ (frame-height) 2.5))
          (round (/ (frame-height) 1.75))))
    ;; non-graphic display
    10))


;;; Full-window Ivy selection (requires /ivy-height-smart).
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


;;; Automatically place the posframe centered in the current window if large
;;; enough, or in the frame as a fallback.
(defun /ivy-posframe-display-smart (str)
  (if (or (< (window-total-width (with-ivy-window (selected-window)))
             ivy-posframe-width)
          (< (window-total-height (with-ivy-window (selected-window)))
             ;; XXX: This needs to match ivy-height-alist.
             (/ivy-height-smart)))
      (ivy-posframe-display-at-frame-center str)
    (ivy-posframe-display-at-window-center str)))


(setq ivy-height-alist
      '((swiper . 10)
        (t . (lambda (_caller)
               ;; XXX: This needs to match /ivy-posframe-display-smart.
               (/ivy-height-smart)))))

(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (counsel-M-x . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))

(setq ivy-display-functions-alist
      '((ivy-completion-in-region . ivy-display-function-overlay)
        (counsel-M-x . /ivy-display-function-window)
        (counsel-yank-pop . /ivy-display-function-window)
        (t . nil)))

(setq ivy-posframe-display-functions-alist
      '((swiper . ivy-posframe-display-at-window-bottom-left)
        ;;(swiper . nil)
        (complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x . /ivy-posframe-display-smart)
        (counsel-yank-pop . /ivy-display-function-window)
        ;;(t . ivy-posframe-display)
        (t . /ivy-posframe-display-smart)))
