(use-package selectrum
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
