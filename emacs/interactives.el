;; fonts
;; best choices:
;; - Monaco (classic Mac, missing bold variant, missing many Unicode symbols, excellent at size 140)
;; - Menlo (has bold variant, better Unicode coverage than Monaco, excellent at size 120)
;; - DejaVu Sans Mono (almost identical to Menlo, distinctions on 0-*~, sometimes better for smaller line spacing)
;; decent choices (have Unicode or character problems): Consolas, Inconsolata, Anonymous Pro
(defun set-font (font-family height)
  (interactive "sFont family: \nnHeight: ")
  (set-face-attribute 'default nil :family font-family :height height)
  (if (member font-family (list "Consolas" "Inconsolata" "Anonymous Pro"))
      (set-fontset-font "fontset-default"
                        'unicode
                        (font-spec :family "DejaVu Sans Mono"
                                   :width 'normal
                                   :size (/ height 10)
                                   :weight 'normal))
    (set-fontset-font "fontset-default" 'unicode font-family)))


(defun set-font-current-buffer (font-family height)
  (interactive "sFont family: \nnHeight: ")
  (let ((face-name (gensym)))
    (make-face face-name)
    (set-face-attribute face-name nil :family font-family :height height)
    (buffer-face-mode t)
    (buffer-face-set face-name)))


(defun scratch ()
  "Open a new scratch buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*scratch*"))
  (lisp-interaction-mode))


(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (cond ((not filename)
           (message "Buffer '%s' is not visiting a file!" name))
          ((get-buffer new-name)
           (message "A buffer named '%s' already exists!" new-name))
          (t (rename-file name new-name 1)
             (rename-buffer new-name)
             (set-visited-file-name new-name)
             (set-buffer-modified-p nil)))))


(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir (if (string-match dir "\\(?:/\\|\\\\)$")
                  (substring dir 0 -1)
                dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (copy-file filename newname 1)
      (delete-file filename)
      (set-visited-file-name newname)
      (set-buffer-modified-p nil)
      t)))


(defun dos-to-unix ()
  (interactive)
  (set-buffer-file-coding-system 'unix t))


(defun swap-windows ()
  "Swap the two most recently used windows."
  (interactive)
  (let* ((buffers (buffer-list (selected-frame)))
         (windows (delq nil (mapcar #'get-buffer-window buffers))))
    (if (and windows (> (length windows) 1))
        (progn
          (let* ((w1 (first windows))
                 (w2 (second windows))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2)))
            (set-window-buffer w1 b2)
            (set-window-buffer w2 b1)
            (set-window-start w1 s2)
            (set-window-start w2 s1)))
      (message "no suitable windows to swap"))))


(defun flip-windows ()
  "Switch to the window that displays the most recently selected buffer."
  (interactive)
  (let* ((buffers (delq (current-buffer) (buffer-list (selected-frame))))
         (windows (delq (selected-window) (delq nil (mapcar #'get-buffer-window buffers)))))
    (if windows
        (select-window (car windows))
      (message "no suitable window to switch to"))))

(global-set-key (kbd "C-o") 'flip-windows)


(defun split-window-3-right ()
  (interactive)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(global-set-key (kbd "C-x M-3") 'split-window-3-right)


(defun flash-active-buffer ()
  (interactive)
  (when (not (facep 'flash-active-buffer-face))
    (make-face 'flash-active-buffer-face)
    (set-face-attribute 'flash-active-buffer-face nil
                        :background "blanchedalmond"
                        :foreground "black"))
  (run-at-time "100 millisec" nil
               (lambda (remap-cookie)
                 (face-remap-remove-relative remap-cookie))
               (face-remap-add-relative 'default 'flash-active-buffer-face)))

(global-set-key (kbd "C-M-?") 'flash-active-buffer)


(defun search-all-buffers (regexp)
  (interactive "sRegexp: ")
  (multi-occur-in-matching-buffers "." regexp t))


(defun ido-goto-symbol ()
  "Updates the imenu index and then uses ido to select a symbol to navigate to.
   From http://www.emacswiki.org/emacs/ImenuMode."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (cl-labels ((addsymbols (symbol-list)
                  (when (listp symbol-list)
                    (dolist (symbol symbol-list)
                      (let ((name nil) (position nil))
                        (cond
                          ((and (listp symbol) (imenu--subalist-p symbol))
                           (addsymbols symbol))
                          ((listp symbol)
                           (setq name (car symbol))
                           (setq position (cdr symbol)))
                          ((stringp symbol)
                           (setq name symbol)
                           (setq position (get-text-property 1 'org-imenu-marker symbol))))
                        (unless (or (null position) (null name))
                          (add-to-list 'symbol-names name)
                          (add-to-list 'name-and-pos (cons name position))))))))
        (addsymbols imenu--index-alist))
      (let* ((selected-symbol (ido-completing-read "Symbol: " symbol-names))
             (position (cdr (assoc selected-symbol name-and-pos))))
        (goto-char position))))


(defun what-face (pos)
  (interactive "d")
  ;; see also: describe-char
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defun find-file-in-git-project ()
  "Deprecated in favor of projectile-mode."
  (interactive)
  (let ((project-root (or (locate-dominating-file default-directory ".git-project-root")
                          (locate-dominating-file default-directory ".git/")
                          (locate-dominating-file default-directory ".project.el"))))
    (assert (not (endp project-root)) t "Not visiting a file under a Git repository.")
    (let* ((default-directory project-root) ; use relative paths in find invocation
           (all-files (split-string
                       (shell-command-to-string
                        ;; Carefully handle whitespace in paths; also, present
                        ;; the list with leading "./" sequences removed.
                        (format "%s | %s"
                                "find . \\( -path '*.git' -o -name '*.o' \\) -prune -o -type f -print0"
                                "sed 's=\\./==g'"))
                       "[\000]+"))
           (file (if (and (boundp 'ido-mode) ido-mode)
                     (ido-completing-read "Find file in project: " all-files)
                     (completing-read "Find file in project: " all-files))))
      (find-file file))))


(defun kill-buffer-and-delete-window ()
  "Kill the current buffer and close the window where it is open."
  (interactive)
  (kill-buffer)
  (delete-window))

(global-set-key (kbd "C-x M-k") 'kill-buffer-and-delete-window)


(defun toggle-window-dedication ()
  "Toggle the dedication flag of the current window to its buffer."
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;;(global-set-key (kbd "C-x C-d") 'toggle-window-dedication)


(defun flush-to-fill-column ()
  (interactive)
  (let ((saved-point (point))
        (start (save-excursion
                 (end-of-line)
                 (current-column))))
    (insert-char ?\  (- fill-column start))
    (goto-char saved-point)))


(defun switch-to-last-terminal-buffer ()
  (interactive)
  ;; priorities:
  ;; 1: a terminal buffer in the current frame
  ;; 2: an iTerm window
  ;; 3: a Terminal.app window
  ;; 4: nothing
  (let* ((buffers (buffer-list (selected-frame)))
         (current (car buffers))
         (term-rx "\\*terminal<.+>\\*"))
    (cond ((catch 'break
             (dolist (buffer buffers)
               (when (string-match term-rx (buffer-name buffer))
                 (let ((window (get-buffer-window buffer)))
                   (when window
                     (select-window window)
                     (throw 'break t))))))
           t)
          ((not (eq 'darwin system-type))
           nil)
          ((= 1 (do-applescript "
if application \"iTerm\" is running then
  tell application \"iTerm\"
    activate
    return 1
  end tell
end if
return 0
"))
           t)
          ((= 1 (do-applescript "
if application \"Terminal\" is running then
  tell application \"Terminal\"
    activate
    return 1
  end tell
end if
return 0
"))
           t)
          (t nil))))

(global-set-key (kbd "C-c C-z") 'switch-to-last-terminal-buffer)


(defun kill-ring-clear ()
  (interactive)
  (setf kill-ring nil))


(defun smart-beginning-of-line ()
  "Move point to the beginning of text on the current line; if that is already
   the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    (when (eq pt (point))
      (beginning-of-line-text))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)


(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))


(defun load-project ()
  (interactive)
  (unless (boundp 'load-project-history)
    (setq load-project-history (list)))
  (let* ((project-dir (file-name-as-directory
                       (if (fboundp 'helm-read-file-name)
                           (helm-read-file-name "Load project: "
                                                :name "Project Root Directory"
                                                :buffer "*helm load-project*"
                                                :initial-input "~/"
                                                :history load-project-history)
                           (read-directory-name "Load project: "))))
         (project-file (concatenate 'string project-dir ".project.el")))
    (if (file-exists-p project-file)
        (progn
          (add-to-list 'load-project-history project-dir)
          (cv--load-trusted-elisp-file project-file))
      (message (concatenate 'string project-file " not found")))))


(defun disable-all-themes ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))


(defun switch-theme (theme)
  (interactive
   (list
    (intern (ido-completing-read "Load custom theme: "
                                 (mapcar 'symbol-name
                                         (custom-available-themes))))))
  (disable-all-themes)
  (load-theme theme t))


(defun pair-programming (global)
  (interactive "P")
  (if global
      (progn
        (global-linum-mode (if global-linum-mode -1 1))
        (global-hl-line-mode (if global-hl-line-mode -1 1)))
    (progn
      (linum-mode (if linum-mode -1 1))
      (hl-line-mode (if hl-line-mode -1 1)))))


(defun ensure-packages-compiled ()
  "If any packages installed with package.el aren't compiled yet, compile them."
  (interactive)
  (--each (f-directories package-user-dir)
    (unless (cv--was-compiled-p it)
      (byte-recompile-directory it 0))))


(defun diminish-minor-mode (mode &optional mode-line-string)
  "An homage to the poetry of http://www.eskimo.com/~seldon/diminish.el.
   Reduces mode-line clutter from too many loud minor modes.
   This implementation is somewhat deprecated now that diminish.el is loaded at startup."
  (interactive)
  (let ((mode-line-string (if mode-line-string mode-line-string "")))
    (let* ((old-mls (assoc mode minor-mode-alist)))
      (when old-mls
        (setf (cadr old-mls) mode-line-string)))))


(defun url-encode-region (start end)
  "URL-encode the region between START and END in current buffer."
  (interactive "r")
  (cv--apply-fn-region #'url-hexify-string start end))


(defun url-decode-region (start end)
  "URL-decode the region between START and END in current buffer."
  (interactive "r")
  (cv--apply-fn-region #'url-unhex-string start end))


(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let* ((actives-raw (mapcar (lambda (mode)
                                (condition-case nil
                                    (when (and (symbolp mode) (symbol-value mode))
                                      mode)
                                  (error nil)))
                              minor-mode-list))
         (actives (-remove #'null actives-raw))
         (sorted-actives (sort actives #'string-lessp)))
    (message "Active modes are %s" sorted-actives)))
