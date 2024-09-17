;;; -*- lexical-binding: t; -*-

(require 's)


;;; fonts
;;; best choices:
;;; - Monaco (classic Mac, missing bold variant, missing many Unicode symbols, excellent at size 140)
;;; - Menlo (has bold variant, better Unicode coverage than Monaco, excellent at size 120)
;;; - DejaVu Sans Mono (almost identical to Menlo, distinctions on 0-*~, sometimes better for smaller line spacing)
;;; - Roboto Mono
;;; - Noto Mono
;;; decent choices (have Unicode or character problems): Consolas, Inconsolata, Anonymous Pro
;;; note about fontsets:
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Modifying-Fontsets.html
;;; shows how to use a specific font for a different codepoint range
(defun set-font-old (font-family height emoji-factor)
  (interactive "sFont family: \nnHeight: ")
  (set-face-attribute 'default nil :family font-family :height height)
  (if (member font-family (list "Consolas" "Inconsolata" "Anonymous Pro"))
      (set-fontset-font "fontset-default"
                        'unicode
                        (font-spec :family "DejaVu Sans Mono"
                                   :width 'normal
                                   :size (/ height 10.0)
                                   :weight 'normal))
    (set-fontset-font "fontset-default" 'unicode font-family))
  ;; Emoji support:
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji" :size (/ height emoji-factor)))
  ;; These probably need size adjustment, not sure about factors:
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append))

(defun set-font (font-family size)
  (interactive)
  (set-frame-font
   (format "%s:pixelsize=%s:weight=medium:slant=normal:width=normal:spacing=100" font-family size)
   t ;; keep-size: do not resize the frame when changing font size
   t ;; frames: apply to all frames
   ))


(cl-defun toggle-header-line (&optional (header-on t header-given))
  (interactive)
  ;; XXX: Sometimes, the header-line face is undefined. Then copy it from the
  ;; mode-line face.
  (condition-case err (face-id 'header-line) (copy-face 'mode-line 'header-line))
  (if (or (and header-given (not header-on))
          (and (not header-given) header-line-format))
      (progn (setq-default header-line-format nil)
             (when (and (boundp 'which-func-table) which-func-table) (clrhash which-func-table)))
    (let* ((default-height (face-attribute 'default :height))
           ;; XXX: This makes the header line smaller than the default height.
           ;; It's... not ideal.
           ;; (header-line-height (cond ((= 150 default-height) 120)
           ;;                           ((= 120 default-height) 100)
           ;;                           ((= 100 default-height) 100)
           ;;                           (t (round (* 0.80 (face-attribute 'default :height))))))
           (header-line-height default-height)
           ;; XXX: centering-multiplier is necessary because
           ;; /mode-line-fill-center does not adapt to different font sizes in
           ;; the main buffer and the header. :(
           (centering-multiplier (cond ((= 120 default-height) 2.3)
                                       ((= 150 default-height) 2.5)
                                       (t 2))))
      (set-face-attribute 'header-line nil :height header-line-height)
      (setq-default header-line-format
        (list `(:eval (let* ((pwd (if (buffer-file-name)
                                      (/display-dir (buffer-file-name) t)
                                    ""))
                             ;; which-function-mode
                             (current-function
                              (when (and (boundp 'which-func-table) which-func-table)
                                (replace-regexp-in-string
                                 "%" "%%"
                                 (or (gethash (selected-window) which-func-table) ""))))
                             (full-text (cond ((and current-function (not (string= pwd "")) (not (string= current-function "")))
                                               (concat pwd " — " current-function))
                                              ((and current-function (string= pwd "") (not (string= current-function "")))
                                               current-function)
                                              (t pwd)))
                             ;; only show the current function if it fits
                             (actual-text (if (> (- (window-total-width) 10) (length full-text))
                                              full-text
                                            pwd)))
                        (list (/mode-line-fill-center (/ (length actual-text) ,centering-multiplier))
                              actual-text))))))))


(defun m150 ()
  (interactive)
  ;;(set-font-old "Menlo" 150 13.5)
  (set-font "Menlo" 15)
  (toggle-header-line header-line-format))


(defun m120 ()
  (interactive)
  ;;(set-font-old "Menlo" 120 15.0)
  (set-font "Menlo" 12)
  (toggle-header-line header-line-format))


(defun m100 ()
  (interactive)
  ;;(set-font-old "Menlo" 100 15.5)
  (set-font "Menlo" 10)
  (toggle-header-line header-line-format))


(defun n150 ()
  (interactive)
  (set-font "Hack Nerd Font Mono" 15)
  (toggle-header-line header-line-format))


(defun n140 ()
  (interactive)
  (set-font "Hack Nerd Font Mono" 14)
  (toggle-header-line header-line-format))


(defun n120 ()
  (interactive)
  (set-font "Hack Nerd Font Mono" 12)
  (toggle-header-line header-line-format))


(defun n100 ()
  (interactive)
  (set-font "Hack Nerd Font Mono" 10)
  (toggle-header-line header-line-format))


(defun j100 ()
  (interactive)
  ;;(set-font-old "JuliaMono" 100 15.5)
  (set-font "JuliaMono" 10)
  (toggle-header-line header-line-format))


(defun j120 ()
  (interactive)
  ;;(set-font-old "JuliaMono" 120 15.0)
  (set-font "JuliaMono" 12)
  (toggle-header-line header-line-format))


(defun j150 ()
  (interactive)
  ;;(set-font-old "JuliaMono" 150 15.0)
  (set-font "JuliaMono" 15)
  (toggle-header-line header-line-format))


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
  (interactive (list
                (read-string
                 "New name: "
                 (file-name-nondirectory (buffer-file-name (current-buffer))))))
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
          (let* ((w1 (-first-item windows))
                 (w2 (-second-item windows))
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


(defun pivot-window-split ()
  "Convert a vertical window split to a horizontal split and vice versa.
Written by ChatGPT 4o."
  (interactive)
  (when (not (= (count-windows) 2))
    (error "Cannot pivot with current window configuration"))
  (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                                     (car next-win-edges))
                                 (<= (cadr this-win-edges)
                                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
                 (car (window-edges (next-window))))
              'split-window-horizontally
            'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1)))))


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


(defun imenu-ido ()
  "Updates the imenu index and then uses ido to select a symbol to navigate to.
   From http://www.emacswiki.org/emacs/ImenuMode."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos (list))
        (symbol-names (list)))
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
    (let* ((selected-symbol (ido-completing-read "Symbol: " symbol-names
                                                 nil nil nil 'imenu--history-list))
             (position (cdr (assoc selected-symbol name-and-pos))))
        (goto-char position))))


(defun imenu-cr ()
  "Custom imenu implementation using completing-read. Cribbed from counsel-imenu, but useful outside Ivy."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (let* ((imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (if current-prefix-arg
                                       (buffer-size)
                                     imenu-auto-rescan-maxout))
         (imenu-space-replacement nil)
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items))
         (items (if (eq major-mode 'emacs-lisp-mode)
                    (let ((fns (cl-remove-if #'listp items :key #'cdr)))
                      (if fns
                          (nconc (cl-remove-if #'nlistp items :key #'cdr)
                                 `(("Functions" ,@fns)))
                        items))
                  items))
         (flat-items (list)))
    (cl-labels ((helper (node tag)
                        (cl-loop for entry in node do
                                 (if (listp (cdr entry))
                                     (helper (cdr entry)
                                             (append tag (list (car entry))))
                                   (let ((real-tag (if (null tag)
                                                       (car entry)
                                                     (concat
                                                      (propertize (concat (s-join "/" tag) ":") 'face 'bold) " " (car entry)))))
                                     (add-to-list 'flat-items
                                                  (cons real-tag
                                                        (cdr entry))))))))
      (helper items (list))
      (let* ((selection (imenu-choose-buffer-index "Items: " flat-items))
             (raw-key (car selection))
             (prop-change (next-property-change 0 raw-key))
             (key (if prop-change
                      (s-trim (substring raw-key (+ 1 prop-change) nil))
                    raw-key)))
        (imenu selection)
        ;; replace selection in history with key
        (setf (car imenu--history-list) key)))))

(global-set-key (kbd "M-i") 'imenu-cr)


(defun what-face (pos)
  (interactive "d")
  ;; see also: describe-char
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defun what-face-mouse (click-event)
  "`describe-char' at CLICK-EVENT's position. CLICK-EVENT should be a mouse-click event."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)
  (let ((pos (cadr (event-start click-event))))
    (describe-char pos)))
;;(global-set-key (kbd "H-d <down-mouse-1>") #'what-face-mouse)


(defun find-file-in-git-project ()
  (interactive)
  (user-error "Deprecated in favor of projectile-mode.")
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
         (eshell-rx "\\*eshell.*\\*")
         (vterm-rx "vterm\\(<.+>\\)?")
         (term-rx "\\*terminal<.+>\\*"))
    (cl-flet ((internal-switch (rx)
             (catch 'break
               (dolist (buffer buffers)
                 (when (string-match rx (buffer-name buffer))
                   (let ((window (get-buffer-window buffer)))
                     (when window
                       (select-window window)
                       (throw 'break t))))))))
      (cond ((internal-switch eshell-rx)
             t)
            ((internal-switch vterm-rx)
             t)
            ((internal-switch term-rx)
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
            (t nil)))))

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
         (project-file (concat project-dir ".project.el")))
    (if (file-exists-p project-file)
        (progn
          (add-to-list 'load-project-history project-dir)
          (/load-trusted-elisp-file project-file))
      (message (concat project-file " not found")))))

(add-to-list 'savehist-additional-variables 'load-project-history)


(defun disable-all-themes ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))


(defvar switch-theme-history nil)
(defun switch-theme (theme)
  (interactive
   (list
    (intern (ido-completing-read "Load custom theme: "
                                 (mapcar 'symbol-name
                                         (custom-available-themes))
                                 nil t nil 'switch-theme-history))))
  (disable-all-themes)
  (load-theme theme t))


(defun pair-programming (global)
  (interactive "P")
  (if global
      (progn
        (global-display-line-numbers-mode (if global-display-line-numbers-mode -1 1))
        (global-hl-line-mode (if global-hl-line-mode -1 1)))
    (display-line-numbers-mode (if display-line-numbers-mode -1 1))
    (hl-line-mode (if hl-line-mode -1 1))))


(defun ensure-packages-compiled ()
  "If any packages installed with package.el aren't compiled yet, compile them."
  (interactive)
  (--each (f-directories package-user-dir)
    (unless (/was-compiled-p it)
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
  (/apply-fn-region #'url-hexify-string start end))


(defun url-decode-region (start end)
  "URL-decode the region between START and END in current buffer."
  (interactive "r")
  (/apply-fn-region #'url-unhex-string start end))


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


(defun json-reformat ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))


(defun dec-to-hex (dec)
  (interactive "nDecimal: ")
  (message (format "%x" dec)))


(defun hex-to-dec (hex)
  (interactive "sHex: ")
  (message (format "%d" (string-to-number hex 16))))


(defun swap-two-frames ()
  "Swap states of the first two frames in frame-list."
  (interactive)
  (/swap-frames (nth 0 (frame-list)) (nth 1 (frame-list))))


(defun split-main-window (direction size)
  "Split the main window in the DIRECTION where DIRECTION is a
symbol with possible values of right, left, above or below and
SIZE is the final size of the windows, if the window is split
horizontally (i.e. in DIRECTION below or above) SIZE is assumed
to be the target height otherwise SIZE is assumed to be the
target width."
  (interactive "SDirection (right, left, above, below): \nnSize: ")
  (let* ((new-window (split-window (frame-root-window) nil direction))
         (horizontal (member direction '(right left))))
    (save-excursion
      (select-window new-window)
      (enlarge-window (- size (if horizontal
                                  (window-width)
                                (window-height)))
                      horizontal))
    new-window))


(defun timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))


(defun insert-random-uuid ()
  (interactive)
  (shell-command "uuidgen" t))


(defun org-store-link-by-id (&optional pt)
  "Makes a unique ID for an heading structure, and saves it for insertion."
  (interactive)
  (org-with-point-at pt
    (let ((heading (substring-no-properties (org-get-heading)))
          (id (org-entry-get nil "custom_id")))
      (if (and id (stringp id))
          (push (list (concat "#" id) heading) org-stored-links)
        (let ((new-id (org-id-new)))
          (org-entry-put pt "custom_id" new-id)
          (org-id-add-location new-id (buffer-file-name (buffer-base-buffer)))
          (push (list (concat "#" new-id) heading) org-stored-links)))))
  (message "Insert with org-insert-last-stored-link (C-c M-l)"))


(defun destroy-symbol (sym)
  "Destroy a symbol: combines FMAKUNBOUND, MAKUNBOUND, and UNINTERN."
  (interactive "SSymbol: ")
  (fmakunbound sym)
  (makunbound sym)
  (unintern sym))


(defun destroy-symbols-prefixed (sym-prefix)
  "Destroy all symbols prefixed with SYM-PREFIX."
  (interactive "sSymbol: ")
  (when (y-or-n-p (format "Are you sure you want to blow away all symbols prefixed `%s'? " sym-prefix))
    (mapatoms (lambda (sym)
                (when (string-prefix-p sym-prefix (symbol-name sym))
                  (fmakunbound sym)
                  (makunbound sym)
                  (unintern sym))))
    (message "Done")))


(defun mac-color-picker (&optional list buffer-name)
  "Call macOS color picker and insert the chosen color."
  (interactive)
  (let ((result
         (do-applescript "tell application \"Finder\"
activate
set result to \"\"
set x to (choose color)
set result to item 1 of x as string
set result to result & \",\"
set result to result & item 2 of x as string
set result to result & \",\"
set result to result & item 3 of x as string
return result
end tell")))
    (insert (/ns-color-to-hex result))
    (do-applescript "tell application \"Emacs\" to activate")))


(defun display-ansi-colors ()
  "When opening a file with ANSI color codes, this modifies the buffer to actually show them."
  (interactive)
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))


;; This is a hack to show notes using posframe while recording a screencast.
;; Save the notes in the list below. Use H-n to advance. That's it.
(let* ((idx -1)
       (notes (list "\n          One.        \n          Two.         \n         Three."
                    "\n         Four.        \n\n        C-c C-z"
                    "\n\n     Happy hacking!")))
  (cl-defun screencast-posframe-notes (&optional arg)
    (interactive "P")
    (let* ((buf-name "*screencast-notes*")
           (buf (get-buffer-create buf-name)))
      (cond ((>= idx (length notes))
             (setq idx -1)
             (posframe-delete-frame buf)
             (kill-buffer buf))
            ;; argument or index is 0: reset and start over
            ((or arg (= -1 idx))
             (setq idx 0)
             (posframe-delete-frame buf)
             (posframe-show buf
                            :internal-border-width 2
                            :internal-border-color "white"
                            :width 23
                            :height 5
                            :poshandler (lambda (info) (cons 20
                                                             (- 0 20
                                                                (plist-get info :mode-line-height)
                                                                (plist-get info :minibuffer-height))))
                            :font (font-spec :family "DejaVu Sans Mono"
                                             :width 'normal
                                             :size 20
                                             :weight 'normal))
             (screencast-posframe-notes)
             )
            ;; normal case: show the note
            (t
             (with-current-buffer buf
               (text-mode)
               (erase-buffer)
               (insert (seq-elt notes idx))
               (incf idx)))))))
;;(global-set-key (kbd "H-n") #'screencast-posframe-notes)


;; Commit updated ELPA packages.
(defun commit-elpa-packages ()
  (interactive)
  (declare-function nil-blank-string "em-unix.el")
  (require 'em-unix)
  (let ((default-directory package-user-dir))
    (let* ((raw (shell-command-to-string "git status --porcelain=v1 -- ."))
           (lines (-filter #'nil-blank-string (s-split "\n" raw)))
           (no-tag (cl-loop for l in lines collect
                            (when (> (length l) 2) (substring l 3))))
           (matcher (rx (group "emacs/elpa/" (opt (+ num) "/"))
                        (group (*? anything))
                        (group "-" (+ (any num ".")) (any "/" ".signed"))))
           (to-version (cl-loop for l in lines collect
                                (progn (string-match matcher l)
                                       (match-string 2 l))))
           (packages (delete-dups to-version))
           ;; sort packages by length so more specific ones commit first, e.g.
           ;; counsel-projectile before counsel
           (sorted-packages (sort packages (lambda (p1 p2) (> (length p1) (length p2)))))
           (commands (-flatten
                      (cl-loop for pkg in sorted-packages collect
                               (list (concat "git add -A '" pkg "*'")
                                     (concat "git commit -q -m 'Emacs package update: " pkg "'"))))))
      (cl-loop for cmd in commands do
               (let ((ans (read-string (concat "Execute {{" cmd "}}? "))))
                 (when (or (string= "y" ans) (string= "Y" ans) (string= "" ans))
                   (shell-command-to-string cmd)))))))


(defun kill-or-delete-region (beg end &optional region)
  "Normally just calls kill-region, just like the default C-w
binding. When used with a prefix argument, it calls delete-region
instead, which does not save the deletion to the kill-ring."
  (interactive (list (mark) (point) 'region))
  (if current-prefix-arg
      (delete-region beg end)
    (kill-region beg end region)))

(global-set-key (kbd "C-w") #'kill-or-delete-region)


(defun open-messages-buffer ()
  "When the *Messages* buffer is in the ignore-buffers list, it's
annoying to open. This function makes it easier."
  (interactive)
  (switch-to-buffer (get-buffer "*Messages*")))


(defun unison ()
  "Synchronize the current buffer's file using the best matching Unison profile.
Written by ChatGPT 4o."
  (interactive)
  (let ((unison-bin (executable-find "unison"))
        (unison-dir (expand-file-name "~/.unison"))
        (current-file (buffer-file-name (current-buffer)))
        best-match-profile best-match-path best-overlap-length)

    ;; Check if the unison binary is available
    (unless unison-bin
      (error "Unison binary not found in exec-path"))

    ;; Check if the ~/.unison directory exists
    (unless (file-directory-p unison-dir)
      (error "~/.unison directory does not exist"))

    ;; Check if the current buffer is visiting a file
    (unless current-file
      (error "Current buffer is not visiting a file"))

    ;; Function to find common prefix length
    (cl-flet ((file-name-common-prefix (a b)
                "Return the common prefix of the file names A and B."
                (let ((a-dirs (split-string (file-name-directory a) "/"))
                      (b-dirs (split-string (file-name-directory b) "/"))
                      (common-dirs '()))
                  (while (and a-dirs b-dirs (string= (car a-dirs) (car b-dirs)))
                    (push (car a-dirs) common-dirs)
                    (setq a-dirs (cdr a-dirs))
                    (setq b-dirs (cdr b-dirs)))
                  (string-join (nreverse common-dirs) "/"))))

      ;; Find all .prf files in the ~/.unison directory
      (dolist (prf-file (directory-files unison-dir t "\\.prf\\'"))
        (with-temp-buffer
          (insert-file-contents prf-file)
          (goto-char (point-min))
          ;; Skip files with existing `path =` entries
          (unless (re-search-forward "^path = " nil t)
            (goto-char (point-min))
            ;; Parse `root = <some-path>` lines
            (while (re-search-forward "^root = \\(.+\\)$" nil t)
              (let* ((root-path (match-string 1))
                     (common-prefix (file-name-common-prefix
                                     (file-name-as-directory current-file)
                                     (file-name-as-directory root-path)))
                     (overlap-length (length common-prefix)))
                (when (and (file-exists-p root-path)
                           (string-prefix-p "/" common-prefix)
                  (or (not best-overlap-length) (> overlap-length best-overlap-length)))
                  (setq best-match-profile prf-file)
                  (setq best-match-path root-path)
                  (setq best-overlap-length overlap-length)))))))

      ;; Error out if no matching profile was found
      (unless best-match-profile
        (error "No matching Unison profile found"))

      ;; Construct and execute the Unison command
      (let ((relative-file-path (file-relative-name current-file best-match-path))
            (output-buffer (get-buffer-create "*Unison*")))
        (start-process "unison" output-buffer unison-bin
                       (file-name-base best-match-profile)
                       "-path" relative-file-path
                       "-color" "false"
                       "-dumbtty"
                       "-batch")
        (pop-to-buffer output-buffer)))))


(defun remove-alist-entry (alist-var)
  "Prompt for an alist variable ALIST-VAR and an entry to remove.
Written by ChatGPT 4o, with fixes."
  (interactive
   (list (intern (completing-read "Alist variable: "
                                  obarray
                                  (lambda (v) (and (boundp v)
                                                   (listp (eval v))))
                                  t))))
  (let* ((alist (eval alist-var))
         (options (mapcar (lambda (pair)
                            (format "%s: %s" (car pair) (cdr pair)))
                          alist))
         (choice (completing-read "Entry to remove: " options))
         (key (car (split-string choice ": "))))
    (set alist-var (assoc-delete-all key alist))
    (message "Entry '%s' removed from %s" key alist-var)))
