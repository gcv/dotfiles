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
     (width . 0.7)))
  (mini-frame-resize-max-height 15)

  :commands (mini-frame-read-from-minibuffer)

  :bind
  (;;("M-x" . /smf-M-x)
   ;;("C-x C-M-f" . /smf-find-file)
   ;;("M-i" . /smf-imenu)
   )

  :init
  ;; This implementation effectively enables mini-frame for the command, without
  ;; enabling the full mode. Reason: disabling the mode deletes the temporary
  ;; mini-frame frame, which makes creating it again unnecessarily slow.
  ;;
  ;; Unfortunately, it has some subtle breakage when opening files has an
  ;; additional prompt. For example, opening a symlink vc-controlled file should
  ;; prompt to open the link target. This seems to work fine with full
  ;; mini-frame mode enabled, but not with this hack.

  (defun /with-mf-selectrum (fn)
    (let ((status-selectrum selectrum-mode))
      (unwind-protect
          (progn
            (selectrum-mode 1)
            (advice-add 'read-from-minibuffer :around #'mini-frame-read-from-minibuffer)
            (advice-add 'read-string :around #'mini-frame-read-from-minibuffer)
            (advice-add 'minibuffer-selected-window :around #'mini-frame--minibuffer-selected-window)
            (call-interactively fn))
        (advice-remove 'read-from-minibuffer #'mini-frame-read-from-minibuffer)
        (advice-remove 'read-string #'mini-frame-read-from-minibuffer)
        (advice-remove 'minibuffer-selected-window #'mini-frame--minibuffer-selected-window)
        (unless status-selectrum (selectrum-mode -1)))))

  (defun /smf-M-x ()
    (interactive)
    (/with-mf-selectrum #'execute-extended-command))

  (defun /smf-find-file ()
    (interactive)
    (/with-mf-selectrum #'find-file))

  (defun /smf-imenu ()
    (interactive)
    (/with-mf-selectrum #'imenu-cr))


  :config
  ;; XXX: mini-frame-resize set to t does not quite work:
  ;; - https://github.com/muffinmad/emacs-mini-frame/issues/18
  ;; - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44080

  ;; To selectively enable mini-frame with Selectrum, the following
  ;; works for M-x, but not for C-x C-f:
  ;;
  ;; (defun /xmf-M-x ()
  ;;   (interactive)
  ;;   (let ((completing-read-function #'selectrum-completing-read)
  ;;         (read-buffer-function #'selectrum-read-buffer)
  ;;         (read-file-name-function #'selectrum-read-file-name)
  ;;         (completion-in-region-function #'selectrum-completion-in-region))
  ;;     (mini-frame-read-from-minibuffer
  ;;      (lambda () (call-interactively #'execute-extended-command)))))
  ;;
  ;; Clean behavior for #'find-file requires a full mode flip:

  ;; (when window-system
  ;;   (mini-frame-mode 1))

  (add-to-list 'mini-frame-ignore-commands 'find-alternate-file)
  (add-to-list 'mini-frame-ignore-commands "ctrlf-.*")
  (add-to-list 'mini-frame-ignore-commands "helm-.*")
  (add-to-list 'mini-frame-ignore-commands "magit-.*")
  )


(use-package selectrum
  :pin melpa
  :defer t
  :config (progn

            (when window-system
              ;; match mini-frame height
              (setq selectrum-num-candidates-displayed 15))

            ;;(selectrum-mode 1)
            (selectrum-prescient-mode 1)

            ))


(use-package selectrum-prescient
  :commands (selectrum-prescient-mode))
