(use-package mini-frame
  :pin melpa
  :if window-system
  :defer t
  :config (progn

            ;; XXX: Does not quite work.
            ;; - https://github.com/muffinmad/emacs-mini-frame/issues/18
            ;; - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44080

            ;; The following works:
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
            ;; But it is not reliable for other functions, like #'find-file.
            ;; That requires a full mode flip:
            ;;
            ;; (defun /with-mf-selectrum (fn)
            ;;   (let ((status-mini-frame mini-frame-mode)
            ;;         (status-selectrum selectrum-mode))
            ;;     (unwind-protect
            ;;         (progn (mini-frame-mode 1)
            ;;                (selectrum-mode 1)
            ;;                (call-interactively fn))
            ;;       (unless status-selectrum (selectrum-mode -1))
            ;;       (unless status-mini-frame (mini-frame-mode -1)))))
            ;;
            ;; (defun /smf-M-x ()
            ;;   (interactive)
            ;;   (/with-mf-selectrum #'execute-extended-command))
            ;;
            ;; (defun /smf-find-file ()
            ;;   (interactive)
            ;;   (/with-mf-selectrum #'find-file))
            ;;
            ;; (defun /smf-imenu ()
            ;;   (interactive)
            ;;   (/with-mf-selectrum #'imenu-cr))
            ;;
            ;; However, it still has strange display bugs, mostly related to
            ;; mini-frame itself. For example, opening a symlinked file causes a
            ;; prompt, which then displays in the mini-frame rather than the
            ;; minibuffer, and glitches in strange ways.

            ;; (when window-system
            ;;   (mini-frame-mode 1))
            ;;
            ;; (add-to-list 'mini-frame-ignore-commands 'find-alternate-file)
            ;; (add-to-list 'mini-frame-ignore-commands 'edebug-eval-expression)
            ;; (add-to-list 'mini-frame-ignore-commands "ctrlf-.*")
            ;; (add-to-list 'mini-frame-ignore-commands "helm-.*")
            ;; (add-to-list 'mini-frame-ignore-commands "magit-.*")

            (custom-set-variables
	     '(mini-frame-resize nil)   ; cannot be t until frame bugs are fixed
	     '(mini-frame-show-parameters
               '((top . 100)
                 (left . 0.5)
                 (height . 15)          ; needed until frame bugs are fixed
                 (width . 0.7)))
	     '(mini-frame-resize-max-height 15)
             )

            ))


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
