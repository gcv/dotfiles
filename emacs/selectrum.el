(defun /with-selectrum (fn)
  (let ((status-selectrum selectrum-mode))
    (unwind-protect
        (progn
          (selectrum-mode 1)
          (call-interactively fn))
      (unless status-selectrum (selectrum-mode -1)))))

(defun /with-selectrum-mini-frame (fn advice-level)
  (let ((status-selectrum selectrum-mode))
    (unwind-protect
        (progn
          (selectrum-mode 1)
          (when window-system
            (advice-add advice-level :around #'mini-frame-read-from-minibuffer))
          (call-interactively fn))
      (when window-system
        (advice-remove advice-level #'mini-frame-read-from-minibuffer))
      (unless status-selectrum (selectrum-mode -1)))))

(defun /selectrum-M-x ()
  (interactive)
  (/with-selectrum-mini-frame #'execute-extended-command 'read-extended-command))

(defun /selectrum-find-file ()
  (interactive)
  (/with-selectrum-mini-frame #'find-file 'find-file-read-args))

(defun /selectrum-imenu ()
  (interactive)
  (/with-selectrum-mini-frame #'imenu-cr 'read-from-minibuffer))


(use-package selectrum
  :pin melpa
  :defer t

  :bind
  (("M-x" . /selectrum-M-x)
   ("C-x C-M-f" . /selectrum-find-file)
   ("M-i" . /selectrum-imenu)
   )

  :config (progn

            (when window-system
              ;; match mini-frame height - 1
              (setq selectrum-num-candidates-displayed 14))

            ;;(selectrum-mode 1)
            (selectrum-prescient-mode 1)

            ))


(use-package selectrum-prescient
  :commands (selectrum-prescient-mode))


(use-package mini-frame
  :pin melpa
  :if window-system
  :defer t

  :custom
  (mini-frame-resize nil)               ; cannot be t until frame bugs are fixed
  (mini-frame-show-parameters
   '((top . 100)
     (left . 0.5)
     (height . 15)                      ; needed until frame bugs are fixed
     (width . 0.7)
     (left-fringe . 5)
     (right-fringe . 5)))
  (mini-frame-resize-max-height 15)

  :commands (mini-frame-read-from-minibuffer)

  :config
  ;; XXX: mini-frame-resize set to t does not quite work:
  ;; - https://github.com/muffinmad/emacs-mini-frame/issues/18
  ;; - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44080

  ;;(when window-system
  ;;  ;; only enable mini-frame-mode for find-file (C-x C-f) and
  ;;  ;; execute-extended-command (M-x):
  ;; (setq mini-frame-advice-functions '(find-file-read-args read-extended-command))
  ;; (mini-frame-mode 1))

  ;; Only useful when mini-frame-mode is used broadly, i.e., when
  ;; mini-frame-advice-functions is set to activate itself at a lower level
  ;; (read-from-minibuffer, read-string):
  ;;(add-to-list 'mini-frame-ignore-commands 'find-alternate-file)
  ;;(add-to-list 'mini-frame-ignore-commands "helm-.*")
  ;;(add-to-list 'mini-frame-ignore-commands "magit-.*")
  )
