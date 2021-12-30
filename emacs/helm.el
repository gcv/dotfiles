;;; -*- lexical-binding: t; -*-

(use-package helm
  :diminish ""

  :bind
  (("C-c h" . helm-command-prefix)
   ;;("C-x M-b" . helm-mini)
   ;;("M-x" . helm-M-x)
   ;;("M-i" . helm-semantic-or-imenu)
   ;;("C-M-y" . helm-show-kill-ring)
   )

  :custom
  (helm-ff-keep-cached-candidates nil)

  :config
  ;; XXX: Seems to be necessary to avoid loading order errors in Helm 3.6.4?
  (require 'tramp)

  (require 'helm-config)

  ;; the default C-x c is too close to C-x C-c
  (global-unset-key (kbd "C-x c"))

  (setq helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-echo-input-in-header-line t
        helm-split-window-in-side-p t
        helm-ff-file-name-history-use-recentf t
        ;; the buffer list mode string is not useful and too long
        helm-buffer-max-len-mode 0
        helm-buffer-max-length 35)

  (setq helm-boring-buffer-regexp-list ignore-buffers)

  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-projectile-recentf-list
                                    helm-source-buffer-not-found))

  ;; tab fix? not recommended
  ;;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

  (custom-set-variables '(helm-minibuffer-history-key nil))

  (defun helm-find-file-ace-window (file)
    "Use ace-window to select a window to display file."
    (ace-select-window)
    (find-file file))

  (defun helm-switch-buffer-ace-window (buffer)
    "Use ace-window to select a window to display buffer."
    (ace-select-window)
    (switch-to-buffer buffer))

  ;; XXX: Something about helm-projectile is weird in how it loads
  ;; actions. This is a workaround to enable these ace-window actions.
  (helm-mode 1)

  (add-to-list 'helm-find-files-actions
               '("Find file with ace-window" . helm-find-file-ace-window)
               :append)
  (add-to-list 'helm-type-buffer-actions
               '("Switch to buffer with ace-window" . helm-switch-buffer-ace-window)
               :append)

  (define-key helm-find-files-map (kbd "S-<return>")
    #'(lambda ()
        (interactive)
        (with-helm-alive-p
          (helm-exit-and-execute-action 'helm-find-file-ace-window))))

  (define-key helm-buffer-map (kbd "S-<return>")
    #'(lambda ()
        (interactive)
        (with-helm-alive-p
          (helm-exit-and-execute-action 'helm-switch-buffer-ace-window))))

  (helm-mode -1)
  ;; XXX: Weirdness ends here.
  )


(use-package helm-ag)


;; (use-package helm-posframe
;;   :pin melpa
;;   :if window-system
;;
;;   :custom
;;   (helm-display-buffer-reuse-frame t)
;;   (helm-posframe-poshandler #'(lambda (info)
;;                                 (cons (/ (- (plist-get info :parent-frame-width)
;;                                             (plist-get info :posframe-width))
;;                                          2)
;;                                       100)))
;;   (helm-posframe-parameters '((left-fringe . 5)
;;                               (right-fringe . 5)))
;;
;;   :config
;;   (defun /helm-posframe-display (orig-fn &rest args)
;;     (let* ((frame (apply orig-fn args))
;;            (bg (mini-frame-get-background-color frame)))
;;       (set-face-attribute 'default frame :background bg)
;;       (set-face-attribute 'fringe frame :background nil)
;;       (set-face-attribute 'helm-source-header frame :box `(:color ,bg))))
;;
;;   (advice-add 'helm-posframe-display :around #'/helm-posframe-display)
;;
;;   ;; Not currently enabled by default. It works, but seems to delete the frame
;;   ;; instead of hiding it all the time, which leads to slow frame recreation.
;;   ;;(helm-posframe-enable)
;;   )


(use-package helm-projectile
  :defer 2.0
  :config
  (helm-projectile-toggle 1)
  )


(use-package helm-swoop
  :bind
  (("C-M-S-i" . (lambda () (interactive) (helm-swoop :query "")))
   :map isearch-mode-map
   ("C-M-S-i" . helm-swoop-from-isearch))

  :config
  (setq helm-swoop-split-with-multiple-windows t)
  )
