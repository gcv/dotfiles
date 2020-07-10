(use-package mini-frame
  :pin melpa
  :defer t
  :config (progn

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

            ;; TODO: Look into simplifying this with Emacs 27.
            (setq mini-frame-show-parameters
                  '((top . 100)
                    (left . 0.5)
                    (width . 0.7)
                    (height . 15)))

            ))


(use-package selectrum
  :pin melpa
  :defer t
  :config (progn

            (when window-system
              ;; match mini-frame height
              (setq selectrum-num-candidates-displayed 15))

            ;;(selectrum-mode 1)

            ))


(use-package selectrum-prescient
  :defer t
  :config (progn

            (selectrum-prescient-mode 1)

            ))
