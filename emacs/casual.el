;;; casual emacs - transient-based hint interfaces -*- lexical-binding: t; -*-

(use-package casual-avy
  :bind ("M-g a" . casual-avy-tmenu))


(use-package casual-dired
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))


(use-package casual-info
  :bind (:map Info-mode-map ("C-o" . 'casual-info-tmenu)))


;; (use-package casual-isearch
;;   :bind (:map isearch-mode-map ("C-o" . 'casual-isearch-tmenu)))


;; (use-package casual-calc
;;   :bind (:map calc-mode-map ("C-o" . 'casual-calc-tmenu)))
