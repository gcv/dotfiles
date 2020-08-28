(use-package mini-frame
  :pin melpa
  :if window-system
  :defer t
  :config (progn

            ;; XXX: Does not work:
            ;; (1) My custom C-x C-c wrapper's call to exit emacs does not work.
            ;; (2) With Selectrum: many commands (like switch-to-buffer) do not
            ;; show the same completion list as with mini-frame-mode turned off.
            ;; Others (find-file) work fine.
            ;; - https://github.com/muffinmad/emacs-mini-frame/issues/17
            ;; - https://github.com/muffinmad/emacs-mini-frame/issues/18

            ;; The following works with Selectrum:
            ;; (defun /M-x ()
            ;;  (let ((completing-read-function #'selectrum-completing-read))
            ;;    (mini-frame-read-from-minibuffer
            ;;     (lambda () (call-interactively #'execute-extended-command)))))
            ;;
            ;; Something to explore with Emacs 27: icomplete, icomplete-vertical
            ;; (external package), and whether it has a dedicated
            ;; completing-read implementation. That will probably be adequate
            ;; for my needs.

            ;;(when window-system
            ;;  (mini-frame-mode 1))
            ;;
            ;; (add-to-list 'mini-frame-ignore-commands 'find-alternate-file)
            ;; (add-to-list 'mini-frame-ignore-commands 'edebug-eval-expression)
            ;; (add-to-list 'mini-frame-ignore-commands "ctrlf-.*")
            ;; (add-to-list 'mini-frame-ignore-commands "helm-.*")
            ;; (add-to-list 'mini-frame-ignore-commands "magit-.*")

            ;; Is this needed? Documentation implies this should be autoset with
            ;; `mini-frame-resize'.
	    ;;(setq resize-mini-frames nil)

            (custom-set-variables
	     '(mini-frame-resize
	       t)
	     '(mini-frame-show-parameters
               '((top . 100)
                 (left . 0.5)
                 ;;(height . 15) ; needed prior to Emacs 27
                 (width . 0.7)))
	     '(mini-frame-resize-max-height
	       15))

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
