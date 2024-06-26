;;; -*- lexical-binding: t; -*-

(defun /mode-line-fill (reserve)
  "Return empty space leaving RESERVE space on the right.
   Adapted from powerline.el."
  (let ((real-reserve (if (and window-system (eq 'right (get-scroll-bar-mode)))
                          (- reserve 3)
                        reserve)))
    (propertize " "
                'display `((space :align-to (- (+ right right-fringe right-margin) ,real-reserve))))))


(defun /mode-line-fill-center (reserve)
  "Return empty space to the center of remaining space leaving RESERVE space on the right.
   Adapted from powerline.el."
  (propertize " "
              'display `((space :align-to (- (+ center (0.5 . right-margin)) ,reserve
                                             (0.5 . left-margin))))))


(setq /mode-line-buffer-modified-mark
      (if (member "Menlo" (font-family-list))
          "❉ "
        "* "))


(setq-default mode-line-format
  (list "%e"
        mode-line-front-space
        '(:eval (when (file-remote-p default-directory)
                  (propertize "%1@"
                              'mouse-face 'mode-line-highlight
                              'help-echo (concat "remote: " default-directory))))
        '(:eval (cond (buffer-read-only "%* ")
                      ((buffer-modified-p) /mode-line-buffer-modified-mark)
                      (t "  ")))
        '(:eval (propertize "%12b"
                            'face 'mode-line-buffer-id
                            'help-echo (or buffer-file-name default-directory)))
        '(:eval (let* ((clean-modes (-remove
                                     #'(lambda (x) (or (equal x "(") (equal x ")")))
                                     mode-line-modes))
                       (vc-state (if (and (> (window-total-width) (+ 52 (length (format-mode-line (list clean-modes))) (length (buffer-name)))) ; check for room
                                          (stringp vc-mode)
                                          (not (file-remote-p default-directory)))
                                     (let* ((branch-name (replace-regexp-in-string
                                                          (format "^\s*%s:?-?" (vc-backend buffer-file-name))
                                                          ""
                                                          vc-mode))
                                            (formatted-branch-name (concat "— " (if (< (length branch-name) 20)
                                                                                    branch-name
                                                                                  (substring branch-name -20))))
                                            (buffer-vc-state (vc-state (or buffer-file-name default-directory)))
                                            (f (cond ((string= "up-to-date" buffer-vc-state)
                                                      '((:slant normal)))
                                                     (t
                                                      '((:slant italic))))))
                                       (propertize formatted-branch-name 'face f))
                                   ""))
                       (full-modes (if (eq 'eshell-mode major-mode)
                                       (eshell/shortpwd)
				     (list clean-modes vc-state)))
		       (ctr (format-mode-line full-modes)))
                  ;; only show the center mode (and version control) info if there's enough room
                  (if (and (> (window-total-width) (+ 30 (length ctr) (length (buffer-name)))))
                      (list (/mode-line-fill-center (/ (length ctr) 2))
                            " "
                            full-modes)
                    "")))
        '(:eval (let* ((pos (format-mode-line (list (list -3 (propertize "%P" 'help-echo "Position in buffer"))
                                                    "/"
                                                    (propertize "%I" 'help-echo "Buffer size"))))
                       (row (format-mode-line (list (propertize "%l" 'help-echo "Line number"))))
                       (col (format-mode-line (list ":" (propertize "%c" 'help-echo "Column number"))))
                       (col-length (max 5 (+ (length col))))
                       (row-length (+ col-length (length row)))
                       (pos-length (max 18 (+ 1 row-length (length pos)))))
                  (list
                   (/mode-line-fill pos-length)
                   ;; only show buffer position if there's enough room
                   (if (> (window-total-width) (+ 30 (length (buffer-name))))
                       (replace-regexp-in-string "%" "%%" pos)  ; XXX: Nasty fix for nested escape problem.
                     "")
                   (/mode-line-fill row-length)
                   row
                   (/mode-line-fill col-length)
                   col)))))
