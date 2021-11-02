;;; -*- lexical-binding: t; -*-

(defun /println (str &optional buf)
  "Get an *output* buffer going, and write to it. Like println in every language."
  (let ((buf (if buf buf (get-buffer-create "*output*"))))
    (with-current-buffer buf
      (end-of-buffer)
      (insert str "\n"))
    (display-buffer buf)))


(defun /mode-line-abbrev-file-name ()
  "Take the current buffer's file name and return only the last four elements
   of its path."
  (let* ((bfn (or (buffer-file-name) ""))
         (s1 (replace-regexp-in-string (expand-file-name "~") "~" bfn))
         (s2 (replace-regexp-in-string "\\(.*/\\)\\(.*/.*/.*/.*\\)$" "\\2" s1))
         (padded-string (if (not (= 0 (length s2))) (format " (%s)" s2) s2)))
    padded-string))


(defun /simple-lazy-byte-compile (relative-file)
  (let ((file-src (concat user-emacs-directory "site-lisp/" relative-file ".el"))
        (file-bytecode (concat user-emacs-directory "site-lisp/" relative-file ".elc")))
    (unless (file-exists-p file-bytecode)
      (byte-compile-file file-src))))


(defun /was-compiled-p (path)
  "Does the directory at 'path' contain any .elc files?"
  (--any-p (f-ext? it "elc") (f-files path)))


(defun /string-from-file (filename)
  "Reads the file into a string and returns it."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))


(defun /write-symbols-to-file (symbols filename)
  "Writes the symbol-values of each symbol in the list of symbols
   to the given file."
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (loop for sym in symbols do
            (prin1 (list 'setq sym (list 'quote (symbol-value sym))) buf)
            (terpri buf))
      (save-buffer)
      (kill-buffer))))


(defun /load-trusted-elisp-file (filename)
  "Loads trusted Elisp files. If a file is trusted, it is loaded.
   If it is not known, ask the user if the file's contents are
   trustworthy. If they are, mark the file as trusted and save
   this state."
  (let ((trusted-elisp-files (concat user-emacs-directory "trusted-elisp-files.el")))
    (when (file-exists-p trusted-elisp-files)
      (load-file trusted-elisp-files))
    (if (and (boundp '/trusted-elisp-files) (member filename /trusted-elisp-files))
        (load-file filename)
      (if (yes-or-no-p (format "Do you trust the Elisp code in %s?" filename))
          (progn (unless (boundp '/trusted-elisp-files)
                   (setq /trusted-elisp-files (list)))
                 (add-to-list '/trusted-elisp-files filename)
                 (/write-symbols-to-file (list '/trusted-elisp-files) trusted-elisp-files)
                 (load-file filename)
                 (message "%s marked as trusted and loaded." filename))
        (message (format "%s ignored." filename))))))


(cl-defun /display-dir (dir &optional (shorten-to-display t))
  "Display a shortened directory name, where the home directory
   is shortened, as per convention, to ~, but also narrow to the
   display width if desired."
  (let ((dir (f-abbrev (f-canonical dir))))
    (if (not shorten-to-display)
        dir
      ;; Simple, but does not disambiguate:
      ;;(replace-regexp-in-string "\\([^\/]\\)\\(.*?\\)/" "\\1/" dir)
      ;; Complex and nasty, but behaves like zsh disambiguate_keep_last.
      (if (string= "/" dir)
          "/"
        (let* ((split (cdr (f-split dir)))
               (final (-last-item split))
               (so-far (if (string= "~" (car split)) "~" "")))
          (cl-labels ((shared-prefix (entries target)
                                     (cond ((not (-contains? entries target)) "")
                                           ((= 1 (length entries)) (substring (car entries) 0 1))
                                           (t (cl-labels ((go (idx candidates)
                                                              (let* ((subs (mapcar (lambda (x) (if (> idx (length x)) x (substring x 0 idx))) candidates))
                                                                     (target-sub (if (> idx (length target)) target (substring target 0 idx)))
                                                                     (d (-remove (lambda (x) (not (string= x target-sub))) subs))
                                                                     (nc (-remove (lambda (x) (not (s-starts-with? target-sub x))) candidates)))
                                                                (if (= (length d) 1)
                                                                    (car d)
                                                                  (go (1+ idx) nc)))))
                                                (go 0 entries)))))
                      (fun (entry)
                           (if (string= "~" entry)
                               entry
                             (let* ((wc (concat so-far "/" (substring entry 0 1) "*"))
                                    (all (file-expand-wildcards wc))
                                    (dirs (-filter #'file-directory-p all))
                                    (dir-names (mapcar #'f-filename dirs)))
                               (setf so-far (concat so-far "/" entry))
                               (shared-prefix dir-names entry)))))
            (f-abbrev (apply #'f-join (append (list "/") (mapcar #'fun (-butlast split)) (list final))))))))))


(defun /apply-fn-region (fn start end)
  "Run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall fn text)))))


(defun /swap-frames (a b)
  "Swap window states of root windows of two frames."
  (let ((state-a (window-state-get (frame-root-window a)))
        (state-b (window-state-get (frame-root-window b))))
    (window-state-put state-a (frame-root-window b))
    (window-state-put state-b (frame-root-window a))))


(defun /ns-color-to-hex (color)
  "Convert NS format colors to standard hex notation."
  (concat "#"
          (mapconcat 'identity
                     (mapcar (lambda (x)
                               (let ((col (lsh (string-to-number x) -8)))
                                 (if (< col 16)
                                     (format "0%x" col)
                                   (format "%x" col))))
                             (split-string (s-replace "\"" "" color) ",")) "")))
