(defun cv--mode-line-fill (reserve)
  "Return empty space leaving RESERVE space on the right.
   Adapted from powerline.el."
  (let ((real-reserve (if (and window-system (eq 'right (get-scroll-bar-mode)))
                          (- reserve 3)
                        reserve)))
    (propertize " "
                'display `((space :align-to (- (+ right right-fringe right-margin) ,real-reserve))))))


(defun cv--mode-line-fill-center (reserve)
  "Return empty space to the center of remaining space leaving RESERVE space on the right.
   Adapted from powerline.el."
  (propertize " "
              'display `((space :align-to (- (+ center (0.5 . right-margin)) ,reserve
                                             (0.5 . left-margin))))))


(setq-default mode-line-format
  (list "%e"
        mode-line-front-space
        '(:eval (when (file-remote-p default-directory)
                  (propertize "%1@"
                              'mouse-face 'mode-line-highlight
                              'help-echo (concat "remote: " default-directory))))
        '(:eval (cond (buffer-read-only "%* ")
                      ((buffer-modified-p) "❉ ") ; ❉ is nice for fonts which support it, * suffices otherwise
                      (t "  ")))
        '(:eval (propertize "%12b" 'face 'mode-line-buffer-id 'help-echo default-directory))
        '(:eval (let* ((clean-modes (-remove
                                     #'(lambda (x) (or (equal x "(") (equal x ")")))
                                     mode-line-modes))
                       (vc-state (if (and (stringp vc-mode) (not (file-remote-p default-directory)))
                                     (let* ((branch-name (replace-regexp-in-string
                                                          (format "^\s*%s:?-?" (vc-backend buffer-file-name))
                                                          ""
                                                          vc-mode))
                                            (formatted-branch-name (concat "— " branch-name))
                                            (buffer-vc-state (vc-state buffer-file-name))
                                            (f (cond ((string= "up-to-date" buffer-vc-state)
                                                      '((:slant normal)))
                                                     (t
                                                      '((:slant italic))))))
                                       (propertize formatted-branch-name 'face f))
                                   ""))
                       (ctr (format-mode-line (list clean-modes vc-state))))
                  (list (cv--mode-line-fill-center (/ (length ctr) 2))
                        ctr)))
        '(:eval (let* ((pos (format-mode-line (list (list -3 (propertize "%P" 'help-echo "Position in buffer"))
                                                    "/"
                                                    (propertize "%I" 'help-echo "Buffer size"))))
                       (row (format-mode-line (list (propertize "%l" 'help-echo "Line number"))))
                       (col (format-mode-line (list ":" (propertize "%c" 'help-echo "Column number"))))
                       (col-length (max 5 (+ (length col))))
                       (row-length (+ col-length (length row)))
                       (pos-length (max 18 (+ 1 row-length (length pos)))))
                  (list
                   (cv--mode-line-fill pos-length)
                   (replace-regexp-in-string "%" "%%" pos)  ; XXX: Nasty fix for nested escape problem.
                   (cv--mode-line-fill row-length)
                   row
                   (cv--mode-line-fill col-length)
                   col)))))
