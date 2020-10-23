;;; XXX: This is broken in four ways right now:
;;;
;;; - find-file wrapper splits (!) the current window when opening a file
;;; - find-file has inconsistent behavior when opening a symlink to a
;;;   vc-controlled file (sometimes prompts, sometimes not, sometimes errors,
;;;   sometimes prompts in minibuffer, sometimes in mini frame)
;;; - imenu-cr fails with an error
;;; - imeni-cr should use an overlay or something to keep its history clean of "Function: " prefixes
;;;
;;; This breakage is reproducible with mini-frame and maple-minibuffer

;; no mini-frame:
;; (defun /with-selectrum (fn)
;;   (let ((status-selectrum selectrum-mode))
;;     (unwind-protect
;;         (progn
;;           (selectrum-mode 1)
;;           (call-interactively fn))
;;       (unless status-selectrum (selectrum-mode -1)))))

(defun /with-selectrum (fn)
  (let ((status-selectrum selectrum-mode))
    (unwind-protect
        (progn
          (selectrum-mode 1)
          (if window-system
              (mini-frame-read-from-minibuffer (lambda () (call-interactively fn)))
              ;; (maple-minibuffer:with (call-interactively fn))
            (call-interactively fn)))
      (unless status-selectrum (selectrum-mode -1)))))

(defun /selectrum-M-x ()
  (interactive)
  (/with-selectrum #'execute-extended-command))

(defun /selectrum-find-file ()
  (interactive)
  (/with-selectrum #'find-file))

(defun /selectrum-imenu ()
  (interactive)
  (/with-selectrum #'imenu-cr))


(use-package selectrum
  :pin melpa
  :defer t

  :bind
  (;;("M-x" . /selectrum-M-x)
   ;;("C-x C-M-f" . /selectrum-find-file)
   ;;("M-i" . /selectrum-imenu)
   )

  :config (progn

            (when window-system
              ;; match mini-frame height
              (setq selectrum-num-candidates-displayed 15))

            ;;(selectrum-mode 1)
            (selectrum-prescient-mode 1)

            ))


(use-package selectrum-prescient
  :commands (selectrum-prescient-mode))


(use-package maple-minibuffer
  :quelpa (maple-minibuffer :fetcher github :repo "honmaple/emacs-maple-minibuffer")

  :if window-system

  :config
  (setq maple-minibuffer:position-type 'frame-center
        maple-minibuffer:border-color "gray50"
        maple-minibuffer:height nil
        maple-minibuffer:width 0.7
        maple-minibuffer:cache t)

  (setq maple-minibuffer:action (list)
        maple-minibuffer:ignore-action (list))

  (add-to-list 'maple-minibuffer:action 'read-from-minibuffer)
  (add-to-list 'maple-minibuffer:action 'read-string)

  (add-to-list 'maple-minibuffer:ignore-action 'org-schedule)
  (add-to-list 'maple-minibuffer:ignore-action 'find-alternate-file)
  (add-to-list 'maple-minibuffer:ignore-action 'eval-expression)
  (add-to-list 'maple-minibuffer:ignore-action 'edebug-eval-expression)
  (add-to-list 'maple-minibuffer:ignore-action 'debugger-eval-expression)
  (add-to-list 'maple-minibuffer:ignore-action 'find-alternate-file)

  (add-to-list 'maple-minibuffer:ignore-regexp "^helm-")
  (add-to-list 'maple-minibuffer:ignore-regexp "^magit-")

  ;; more custom parameters for frame
  (defun maple-minibuffer:parameters ()
    "Maple minibuffer parameters."
    `((height . ,(or maple-minibuffer:height 10))
      (width . ,(or maple-minibuffer:width (window-pixel-width)))
      (left-fringe . 5)
      (right-fringe . 5)))
  )


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

  ;; (when window-system
  ;;   (mini-frame-mode 1))

  (add-to-list 'mini-frame-ignore-commands 'find-alternate-file)
  (add-to-list 'mini-frame-ignore-commands "ctrlf-.*")
  (add-to-list 'mini-frame-ignore-commands "helm-.*")
  (add-to-list 'mini-frame-ignore-commands "magit-.*")
  )
