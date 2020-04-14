(use-package ctrlf
  :pin melpa
  :defer t
  :config (progn
            ;; waiting on https://github.com/raxod502/ctrlf/issues/41
            ;;(ctrlf-mode 1)
            ))


(use-package selectrum
  :pin melpa
  :defer t
  :config (progn

            (when window-system
              (setq selectrum-num-candidates-displayed 15))

            ;; waiting for https://github.com/raxod502/selectrum/issues/4
            ;;(selectrum-mode 1)
            ;;(selectrum-prescient-mode 1)

            ))


(use-package selectrum-prescient
  :defer t)
