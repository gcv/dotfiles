(defun cv--mode-line-abbrev-file-name ()
  "Take the current buffer's file name and return only the last four elements
   of its path."
  (let* ((bfn (or (buffer-file-name) ""))
         (s1 (replace-regexp-in-string (expand-file-name "~") "~" bfn))
         (s2 (replace-regexp-in-string "\\(.*/\\)\\(.*/.*/.*/.*\\)$" "\\2" s1))
         (padded-string (if (not (= 0 (length s2))) (format " (%s)" s2) s2)))
    padded-string))


(defun cv--simple-lazy-byte-compile (relative-file)
  (let ((file-src (concat user-emacs-directory "site-lisp/" relative-file ".el"))
        (file-bytecode (concat user-emacs-directory "site-lisp/" relative-file ".elc")))
    (unless (file-exists-p file-bytecode)
      (byte-compile-file file-src))))


(defun cv--was-compiled-p (path)
  "Does the directory at 'path' contain any .elc files?"
  (--any-p (f-ext? it "elc") (f-files path)))


(defun cv--string-from-file (filename)
  "Reads the file into a string and returns it."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))


(defun cv--write-symbols-to-file (symbols filename)
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


(defun cv--load-trusted-elisp-file (filename)
  "Loads trusted Elisp files. If a file is trusted, it is loaded.
   If it is not known, ask the user if the file's contents are
   trustworthy. If they are, mark the file as trusted and save
   this state."
  (let ((trusted-elisp-files (concat user-emacs-directory "trusted-elisp-files.el")))
    (when (file-exists-p trusted-elisp-files)
      (load-file trusted-elisp-files))
    (if (and (boundp 'cv--trusted-elisp-files) (member filename cv--trusted-elisp-files))
        (load-file filename)
      (if (yes-or-no-p (format "Do you trust the Elisp code in %s?" filename))
          (progn (unless (boundp 'cv--trusted-elisp-files)
                   (setq cv--trusted-elisp-files (list)))
                 (add-to-list 'cv--trusted-elisp-files filename)
                 (cv--write-symbols-to-file (list 'cv--trusted-elisp-files) trusted-elisp-files)
                 (load-file filename)
                 (message "%s marked as trusted and loaded." filename))
        (message (format "%s ignored." filename))))))


(defun cv--dir-replace-home (dir)
  "If directory starts with the absolute path of HOME, replace that path with a ~."
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and (>= (length dir) home-len)
             (equal home (substring dir 0 home-len)))
        (concat "~" (substring dir home-len))
      dir)))


(defun cv--apply-fn-region (fn start end)
  "Run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall fn text)))))
