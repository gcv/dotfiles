;;; dirvish-widgets.el --- Core widgets in dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides core attributes / mode-line segments / preview
;; dispatchers (fast and non-blocking media files preview) for dirvish.
;;
;; Attributes:
;; `file-size', `file-time'
;;
;; Mode-line segments:
;;
;; `path', `symlink', `omit', `sort', `index', `free-space', `file-link-number',
;; `file-user', `file-group', `file-time', `file-size', `file-modes',
;; `file-inode-number', `file-device-number'
;;
;; Preview dispatchers:
;;
;; - `image':       preview image files, requires `imagemagick'
;; - `gif':         preview GIF image files with animation
;; - `video':       preview videos files with thumbnail image
;;                    - requires `ffmpegthumbnailer' on Linux/macOS
;;                    - requires `mtn' on Windows (special thanks to @samb233!)
;; - `audio':       preview audio files with metadata, requires `mediainfo'
;; - `epub':        preview epub documents, requires `epub-thumbnail'
;; - `pdf':         preview pdf documents via `pdf-tools'
;; - `archive':     preview archive files, requires `tar' and `unzip'
;; - `dired':       preview directories using `dired' (asynchronously)
;; - `pdf-preface': preview pdf documents with thumbnail image, require `pdftoppm'
;; - `image-dired'  NOT implemented yet | TODO

;;; Code:

(require 'dirvish)

(defcustom dirvish-time-format-string "%y-%m-%d %R"
  "FORMAT-STRING for `file-time' mode line segment.
This value is passed to function `format-time-string'."
  :group 'dirvish :type 'string)

(defcustom dirvish-path-separators '("  ⌂" "  ∀" " ⋗ ")
  "Separators in path mode line segment.
The value is a list with 3 elements:
- icon for home directory [~]
- icon for root directory [/]
- icon for path separators [/]"
  :group 'dirvish :type '(repeat (string :tag "path separator")))

(defcustom dirvish-magick-program "magick"
  "Absolute or reletive name of the `magick' program.
This is used to generate image thumbnails."
  :group 'dirvish :type 'string)

(defcustom dirvish-ffmpegthumbnailer-program "ffmpegthumbnailer"
  "Absolute or reletive name of the `ffmpegthumbnailer' program.
This is used to generate video thumbnails on macOS/Linux."
  :group 'dirvish :type 'string)

(defcustom dirvish-mtn-program "mtn"
  "Absolute or reletive name of the `mtn' program.
This is used to generate video thumbnails on Windows."
  :group 'dirvish :type 'string)

(defcustom dirvish-epub-thumbnailer-program "epub-thumbnailer"
  "Absolute or reletive name of the `epub-thumbnailer' program.
This is used to generate thumbnail for epub files."
  :group 'dirvish :type 'string)

(defcustom dirvish-mediainfo-program "mediainfo"
  "Absolute or reletive name of the `mediainfo' program.
This is used to retrieve metadata for multiple types of media files."
  :group 'dirvish :type 'string)

(defcustom dirvish-pdfinfo-program "pdfinfo"
  "Absolute or reletive name of the `pdfinfo' program.
This is used to retrieve pdf metadata."
  :group 'dirvish :type 'string)

(defcustom dirvish-pdftoppm-program "pdftoppm"
  "Absolute or reletive name of the `pdftoppm' program.
This is used to generate thumbnails for pdf files."
  :group 'dirvish :type 'string)

(defcustom dirvish-zipinfo-program "zipinfo"
  "Absolute or reletive name of the `zipinfo' program.
This is used to list files and their attributes for .zip archives."
  :group 'dirvish :type 'string)

(defcustom dirvish-tar-program "tar"
  "Absolute or reletive name of the `tar' program.
This is used to list files and their attributes for .tar, .gz etc. archives."
  :group 'dirvish :type 'string)

(defcustom dirvish-show-media-properties
  (and (executable-find dirvish-mediainfo-program) t)
  "Show media properties automatically in preview window."
  :group 'dirvish :type 'boolean)

(defvar dirvish-media--cache-pool '())
(defvar dirvish-media--auto-cache-timer nil)
(defcustom dirvish-media-auto-cache-threshold '(500 . 4)
  "Generate cache images automatically.
The value should be a cons cell (FILE-COUNT . PROC-COUNT) where
both FILE-COUNT and PROC-COUNT should be a integer.  Directories
with file count less than FILE-COUNT are cached automatically,
PROC-COUNT is the max number of cache processes.  If this
variable is nil, the auto caching is disabled."
  :group 'dirvish
  :type '(cons (integer :tag "Max number of directory files")
               (integer :tag "Max number of cache process"))
  :set (lambda (k v)
         (set k v)
         (and (timerp dirvish-media--auto-cache-timer)
              (cancel-timer dirvish-media--auto-cache-timer))
         (unless v
           (setq dirvish-media--auto-cache-timer
                 (run-with-timer 0 0.25 #'dirvish-media--autocache)))))

(defconst dirvish-media--img-max-width 2400)
(defconst dirvish-media--img-scale-h 0.75)
(defconst dirvish-media--img-scale-w 0.92)
(defconst dirvish-media--info
  "General;(Full-name . \"\"%FileName%\"\")(Format . \"\"%Format%\"\")(File-size . \"\"%FileSize/String1%\"\")(Duration . \"\"%Duration/String3%\"\")
Image;(Width . \"\"%Width/String%\"\")(Height . \"\"%Height/String%\"\")(Bit-depth . \"\"%BitDepth/String%\"\")(Color-space . \"\"%ColorSpace%\"\")(Chroma-subsampling . \"\"%ChromaSubsampling%\"\")(Compression-mode . \"\"%Compression_Mode/String%\"\")
Video;(Resolution . \"\"%Width% x %Height%\"\")(Video-codec . \"\"%CodecID%\"\")(Framerate . \"\"%FrameRate%\"\")(Video-bitrate . \"\"%BitRate/String%\"\")
Audio;(Audio-codec . \"\"%CodecID%\"\")(Audio-bitrate . \"\"%BitRate/String%\"\")(Audio-sampling-rate . \"\"%SamplingRate/String%\"\")(Audio-channels . \"\"%ChannelLayout%\"\")")

(defface dirvish-free-space
  '((t (:inherit font-lock-constant-face)))
  "Face used for `free-space' mode-line segment."
  :group 'dirvish)

(defface dirvish-file-link-number
  '((t (:inherit font-lock-constant-face)))
  "Face used for file link number mode-line segment."
  :group 'dirvish)

(defface dirvish-file-user-id
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for file size attributes / mode-line segment."
  :group 'dirvish)

(defface dirvish-file-group-id
  '((t (:inherit dirvish-file-user-id)))
  "Face used for file group id mode-line segment."
  :group 'dirvish)

(defface dirvish-file-time
  '((t (:inherit shadow :underline nil :italic nil)))
  "Face used for file access/modify/change time mode-line segment."
  :group 'dirvish)

(defface dirvish-file-size
  '((t (:inherit completions-annotations :underline nil :italic nil)))
  "Face used for display file size attributes / mode-line segment."
  :group 'dirvish)

(defface dirvish-file-modes
  '((t (:inherit font-lock-builtin-face)))
  "Face used for file mode (privilege) mode-line segment."
  :group 'dirvish)

(defface dirvish-file-inode-number
  '((t (:inherit dirvish-file-link-number)))
  "Face used for file inode number mode-line segment."
  :group 'dirvish)

(defface dirvish-file-device-number
  '((t (:inherit dirvish-file-link-number)))
  "Face used for filesystem device number mode-line segment."
  :group 'dirvish)

(defface dirvish-media-info-heading
  '((t :inherit (dired-header bold)))
  "Face used for heading of media property groups."
  :group 'dirvish)

(defface dirvish-media-info-property-key
  '((t :inherit (italic)))
  "Face used for emerge group title."
  :group 'dirvish)

;;;; Helpers

(defun dirvish--attr-size-human-readable (file-size)
  "Produce a string showing FILE-SIZE in human-readable form."
  (let ((power 1024.0)
        (prefixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y")))
    (while (and (>= file-size power) (cdr prefixes))
      (setq file-size (/ file-size power)
            prefixes (cdr prefixes)))
    (substring (format (if (and (< file-size 10)
                                (>= (mod file-size 1.0) 0.05)
                                (< (mod file-size 1.0) 0.95))
                           "      %.1f%s%s"
                         "      %.0f%s%s")
                       file-size (car prefixes)
                       (if (dirvish-prop :gui) " " ""))
               -6)))

(defun dirvish--file-attr-size (name attrs)
  "Get file size of file NAME from ATTRS."
  (cond ((dirvish-prop :remote)
         (substring (format "      %s%s"
                            (or (file-attribute-size attrs) "?")
                            (if (dirvish-prop :gui) " " ""))
                    -6))
        ((stringp (file-attribute-type attrs))
         (let ((ct (dirvish-attribute-cache name :f-count
                     (condition-case nil
                         (let ((files (directory-files name nil nil t)))
                           (dirvish--attr-size-human-readable
                            (- (length files) 2)))
                       (file-error 'file)))))
           (if (not (eq ct 'file)) ct
             (dirvish-attribute-cache name :f-size
               (dirvish--attr-size-human-readable
                 (file-attribute-size (file-attributes name)))))))
        ((file-attribute-type attrs)
         (let ((ct (dirvish-attribute-cache name :f-count
                     (condition-case nil
                         (let ((files (directory-files name nil nil t)))
                           (dirvish--attr-size-human-readable
                            (- (length files) 2)))
                       (file-error 'no-permission)))))
           (if (eq ct 'no-permission) " ---- " ct)))
        (t (dirvish-attribute-cache name :f-size
             (dirvish--attr-size-human-readable
              (or (file-attribute-size attrs) 0))))))

(defun dirvish--file-attr-time (name attrs)
  "File NAME's modified time from ATTRS."
  (if (dirvish-prop :remote)
      (format "  %s " (or (file-attribute-modification-time attrs) "?"))
    (format "  %s " (dirvish-attribute-cache name :f-time
                      (format-time-string
                       dirvish-time-format-string
                       (file-attribute-modification-time attrs))))))

(defun dirvish--format-file-attr (name &optional suffix)
  "Return a (ATTR . FACE) cons of index's attribute NAME.
Use optional SUFFIX or NAME to intern the face symbol."
  (when-let* ((fname (dirvish-prop :index))
              (attrs (dirvish-attribute-cache fname :builtin))
              (attr-getter (intern (format "file-attribute-%s" name)))
              (a-face (intern (format "dirvish-file-%s" (or suffix name))))
              (face (if (dirvish--selected-p) a-face 'dirvish-inactive))
              (attr (and attrs (funcall attr-getter attrs))))
    (cons attr face)))

(defun dirvish-media--cache-path (file &optional base ext no-mkdir)
  "Get FILE's cache path.
BASE is a string indicating the subdir of `dirvish-cache-dir' to
use.  EXT is a suffix such as \".jpg\" that is attached to FILE.
A new directory is created unless NO-MKDIR."
  (let* ((file (if (memq system-type '(windows-nt ms-dos))
                   (concat "/" (replace-regexp-in-string ":" "" file)) file))
         (cache (concat dirvish-cache-dir base file)))
    (and (not no-mkdir) (not (file-exists-p cache))
         (make-directory (file-name-directory cache) t))
    (concat cache ext)))

(defun dirvish-media--cache-sentinel (proc _exitcode)
  "Sentinel for image cache process PROC."
  (when-let* ((dv (dirvish-curr))
              (path (dirvish-prop :index)))
    (and (equal path (process-get proc 'path))
         (dirvish-debounce nil (dirvish--preview-update dv path)))))

(defun dirvish-media--autocache ()
  "Pop and run the cache tasks in `dirvish-media--cache-pool'."
  (when (and dirvish-media--cache-pool
             (< (length (process-list))
                (or (cdr dirvish-media-auto-cache-threshold) 0)))
    (let (process-connection-type proc)
      (pcase-let* ((`(,procname . (,path ,_width ,cmd ,args))
                    (pop dirvish-media--cache-pool)))
        (when path
          (setq proc (apply #'start-process procname
                            (dirvish--util-buffer "img-cache") cmd args))
          (process-put proc 'path path)
          (set-process-sentinel proc #'dirvish-media--cache-sentinel))))))

(defun dirvish-media--group-heading (group-titles)
  "Format media group heading in Dirvish preview buffer.
GROUP-TITLES is a list of group titles."
  (let ((prefix (propertize "    " 'face
                            '(:inherit dirvish-media-info-heading
                                       :strike-through t)))
        (title (propertize
                (format " %s " (mapconcat #'concat group-titles " & "))
                'face 'dirvish-media-info-heading))
        (suffix (propertize " " 'display '(space :align-to right)
                            'face '(:inherit dirvish-media-info-heading
                                             :strike-through t))))
    (format "%s%s%s\n\n" prefix title suffix)))

(defun dirvish-media--metadata-from-mediainfo (file)
  "Return result string from command `mediainfo' for FILE."
  (read (format "(%s)" (shell-command-to-string
                        (format "%s --Output='%s' %s"
                                dirvish-mediainfo-program
                                dirvish-media--info
                                (shell-quote-argument file))))))

(defun dirvish-media--metadata-from-pdfinfo (file)
  "Return result string from command `pdfinfo' for FILE."
  (cl-loop with out = (shell-command-to-string
                       (format "%s %s" dirvish-pdfinfo-program (shell-quote-argument file)))
           with lines = (remove "" (split-string out "\n"))
           for line in lines
           for (title content) = (split-string line ":\s+")
           concat (format "       %s:\t%s\n"
                          (propertize title 'face 'dirvish-media-info-property-key)
                          content)))

(defun dirvish-media--format-metadata (mediainfo properties)
  "Return a formatted string of PROPERTIES from MEDIAINFO."
  (cl-loop for prop in properties
           for p-name = (replace-regexp-in-string
                            "-" " " (format "%s" prop))
           for info = (alist-get prop mediainfo)
           concat (format "       %s:\t%s\n"
                          (propertize p-name 'face 'dirvish-media-info-property-key)
                          info)))

;;;; Attributes

(dirvish-define-attribute file-size
  "File size or directories file count at right fringe."
  :index 1
  :when (and dired-hide-details-mode (> win-width 25))
  (let* ((str (concat (dirvish--file-attr-size f-name f-attrs)))
         (face (or hl-face 'dirvish-file-size)))
    (add-face-text-property 0 (length str) face t str)
    `(right . ,str)))

(dirvish-define-attribute file-time
  "File's modified time at right fringe before the file size."
  :when (and dired-hide-details-mode (> win-width 60))
  (let* ((str (concat (dirvish--file-attr-time f-name f-attrs)))
         (face (or hl-face 'dirvish-file-time)))
    (add-face-text-property 0 (length str) face t str)
    `(right . ,str)))

;;;; Mode line segments

(defun dirvish--register-path-seg (segment path face)
  "Register mode line path SEGMENT with target PATH and FACE."
  (propertize
   segment 'face face 'mouse-face 'highlight
   'help-echo "mouse-1: visit this directory"
   'keymap `(header-line keymap
                         (mouse-1 . (lambda (_ev)
                                      (interactive "e")
                                      (dirvish-find-entry-a ,path))))))

(dirvish-define-mode-line path
  "Path of file under the cursor."
  (let* ((directory-abbrev-alist nil) ; TODO: support custom `directory-abbrev-alist'
         (index (dired-current-directory))
         (face (if (dirvish--selected-p) 'dired-header 'dirvish-inactive))
         (rmt (dirvish-prop :remote))
         (abvname (if rmt (file-local-name index) (abbreviate-file-name index)))
         (host (propertize (if rmt (concat " " (substring rmt 1)) "")
                           'face 'font-lock-builtin-face))
         (segs (nbutlast (split-string abvname "/")))
         (scope (pcase (car segs)
                  ("~" (dirvish--register-path-seg
                        (nth 0 dirvish-path-separators)
                        (concat rmt "~/") face))
                  ("" (dirvish--register-path-seg
                        (nth 1 dirvish-path-separators)
                       (concat rmt "/") face))))
         (path (cl-loop for idx from 2
                        for sp = (format
                                  "%s%s" (or rmt "")
                                  (mapconcat #'concat (seq-take segs idx) "/"))
                        for s in (cdr segs) concat
                        (format "%s%s" (nth 2 dirvish-path-separators)
                                (dirvish--register-path-seg s sp face)))))
    (replace-regexp-in-string "%" "%%%%" (format "%s%s%s " host scope path))))

(dirvish-define-mode-line sort
  "Current sort criteria."
  (let* ((switches (split-string dired-actual-switches))
         (unfocused (unless (dirvish--selected-p) 'dirvish-inactive))
         (crit (cond (dired-sort-inhibit "DISABLED")
                     ((member "--sort=none" switches) "none")
                     ((member "--sort=time" switches) "time")
                     ((member "--sort=version" switches) "version")
                     ((member "--sort=size" switches) "size")
                     ((member "--sort=extension" switches) "extension")
                     ((member "--sort=width" switches) "width")
                     (t "name")))
         (time (cond ((member "--time=use" switches) "use")
                     ((member "--time=ctime" switches) "ctime")
                     ((member "--time=birth" switches) "birth")
                     (t "mtime")))
         (rev (if (member "--reverse" switches) "↓" "↑")))
    (format " %s %s|%s "
            (propertize rev 'face (or unfocused 'font-lock-constant-face))
            (propertize crit 'face (or unfocused 'font-lock-type-face))
            (propertize time 'face (or unfocused 'font-lock-doc-face)))))

(dirvish-define-mode-line omit
  "A `dired-omit-mode' indicator."
  (and (bound-and-true-p dired-omit-mode)
       (propertize "Omit" 'face 'font-lock-negation-char-face)))

(dirvish-define-mode-line symlink
  "Show the truename of symlink file under the cursor."
  (when-let* ((name (dirvish-prop :index))
              (truename (cdr (dirvish-attribute-cache name :type))))
    (format "%s %s"
            (propertize "→ " 'face 'font-lock-comment-delimiter-face)
            (propertize truename 'face 'dired-symlink))))

(dirvish-define-mode-line index
  "Current file's index and total files count."
  (let ((cur-pos (format "%3d " (- (line-number-at-pos (point)) 1)))
        (fin-pos (format "/%3d " (- (line-number-at-pos (point-max)) 2))))
    (if (dirvish--selected-p)
        (put-text-property 0 (length fin-pos) 'face 'bold fin-pos)
      (put-text-property 0 (length cur-pos) 'face 'dirvish-inactive cur-pos)
      (put-text-property 0 (length fin-pos) 'face 'dirvish-inactive fin-pos))
    (format "%s%s" cur-pos fin-pos)))

(dirvish-define-mode-line free-space
  "Amount of free space on `default-directory''s file system."
  (let ((free-space (or (dirvish-prop :free-space)
                        (get-free-disk-space default-directory) "")))
    (dirvish-prop :free-space free-space)
    (format " %s %s " (propertize free-space 'face 'dirvish-free-space)
            (propertize "free" 'face 'font-lock-doc-face))))

(dirvish-define-mode-line file-link-number
  "Number of links to file."
  (pcase-let ((`(,lk . ,face) (dirvish--format-file-attr 'link-number)))
    (propertize (format "%s" lk) 'face face)))

(dirvish-define-mode-line file-user
  "User name of file."
  (pcase-let ((`(,uid . ,face) (dirvish--format-file-attr 'user-id)))
    (unless (dirvish-prop :remote) (setq uid (user-login-name uid)))
    (propertize (format "%s" uid) 'face face)))

(dirvish-define-mode-line file-group
  "Group name of file."
  (pcase-let ((`(,gid . ,face) (dirvish--format-file-attr 'group-id)))
    (unless (dirvish-prop :remote) (setq gid (group-name gid)))
    (propertize (format "%s" gid) 'face face)))

(dirvish-define-mode-line file-time
  "Last modification time of file."
  (pcase-let ((`(,time . ,face)
               (dirvish--format-file-attr 'modification-time 'time)))
    (unless (dirvish-prop :remote)
      (setq time (format-time-string dirvish-time-format-string time)))
    (propertize (format "%s" time) 'face face)))

(dirvish-define-mode-line file-size
  "File size of files or file count of directories."
  (when-let* ((name (dirvish-prop :index))
              (attrs (dirvish-attribute-cache name :builtin))
              (size (dirvish--file-attr-size name attrs)))
    (format "%s" (propertize size 'face 'dirvish-file-size))))

(dirvish-define-mode-line file-modes
  "File modes, as a string of ten letters or dashes as in ls -l."
  (pcase-let ((`(,modes . ,face) (dirvish--format-file-attr 'modes)))
    (propertize (format "%s" modes) 'face face)))

(dirvish-define-mode-line file-inode-number
  "File's inode number, as a nonnegative integer."
  (pcase-let ((`(,attr . ,face) (dirvish--format-file-attr 'inode-number)))
    (propertize (format "%s" attr) 'face face)))

(dirvish-define-mode-line file-device-number
  "Filesystem device number, as an integer."
  (pcase-let ((`(,attr . ,face) (dirvish--format-file-attr 'device-number)))
    (propertize (format "%s" attr) 'face face)))

(dirvish-define-mode-line project
  "Return a string showing current project."
  (let ((project (dirvish--get-project-root))
        (face (if (dirvish--selected-p) 'dired-header 'dirvish-inactive)))
    (if project
        (setq project (file-name-base (directory-file-name project)))
      (setq project "-"))
    (format " %s %s"
            (propertize "Project:" 'face face)
            (propertize project 'face 'font-lock-string-face))))

;;;; Preview dispatchers

(cl-defmethod dirvish-build-cache (&context ((display-graphic-p) (eql t)))
  "Cache image/video-thumbnail when `DISPLAY-GRAPHIC-P'."
  (when-let* ((dv (dirvish-curr))
              ((not (dirvish-prop :remote)))
              ((dv-curr-layout dv))
              (win (dv-preview-window dv))
              ((window-live-p win))
              (width (window-width win))
              ((< (hash-table-count dirvish--attrs-hash)
                  (or (car dirvish-media-auto-cache-threshold) 0))))
    (cl-loop
     with fns = '(dirvish-image-dp dirvish-video-dp dirvish-epub-dp)
     for file in (directory-files default-directory t)
     for ext = (downcase (or (file-name-extension file) ""))
     for (cmd . args) = (cl-loop for fn in fns
                                 for (k . v) = (funcall fn file ext win dv)
                                 thereis (and (eq k 'cache) v))
     when cmd do (push (cons (format "%s-%s-img-cache" file width)
                             (list file width cmd args))
                       dirvish-media--cache-pool))))

(cl-defmethod dirvish-clean-cache (&context ((display-graphic-p) (eql t)))
  "Clean cache images for marked files when `DISPLAY-GRAPHIC-P'."
  (when-let* ((win (dv-preview-window (dirvish-curr)))
              (size (and (window-live-p win) (dirvish-media--img-size win))))
    (clear-image-cache)
    (setq size (dirvish-media--img-size win))
    (dolist (file (dired-get-marked-files))
      (mapc #'delete-file (file-expand-wildcards
                           (dirvish-media--cache-path
                            file (format "images/%s" size) ".*" t)
                           t)))))

(cl-defgeneric dirvish-media-metadata (file)
  "Get media file FILE's metadata.")

(cl-defmethod dirvish-media-metadata ((file (head image)))
  "Get metadata for image FILE."
  (let ((minfo (dirvish-media--metadata-from-mediainfo (cdr file))))
    (format "%s%s\n%s%s"
            (dirvish-media--group-heading '("Image"))
            (dirvish-media--format-metadata
             minfo '(Width Height Color-space Chroma-subsampling Bit-depth Compression-mode))
            (dirvish-media--group-heading '("General"))
            (dirvish-media--format-metadata minfo '(Full-name Format File-size)))))

(cl-defmethod dirvish-media-metadata ((file (head video)))
  "Get metadata for video FILE."
  (let ((minfo (dirvish-media--metadata-from-mediainfo (cdr file))))
    (format "%s%s\n%s%s\n%s%s"
            (dirvish-media--group-heading '("General"))
            (dirvish-media--format-metadata
             minfo '(Full-name Format File-size Duration))
            (dirvish-media--group-heading '("Video"))
            (dirvish-media--format-metadata
             minfo '(Resolution Video-codec Framerate Video-bitrate))
            (dirvish-media--group-heading '("Audio"))
            (dirvish-media--format-metadata
             minfo '(Audio-codec Audio-bitrate Audio-sampling-rate Audio-channels)))))

(cl-defmethod dirvish-media-metadata ((file (head pdf)))
  "Get metadata for pdf FILE."
  (format "%s%s" (dirvish-media--group-heading '("PDF info"))
          (dirvish-media--metadata-from-pdfinfo (cdr file))))

(cl-defmethod dirvish-preview-dispatch ((recipe (head img)) dv)
  "Insert RECIPE as an image at preview window of DV."
  (let ((buf (dirvish--util-buffer 'preview dv nil t))
        (img (cdr recipe)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (font-lock-mode -1)
      (insert " ")
      (add-text-properties 1 2 `(display ,img rear-nonsticky t keymap ,image-map))
      (pcase-let ((`(,iw . ,ih) (image-size img)))
        (let* ((p-window (dv-preview-window dv))
               (w-pad (max (round (/ (- (window-width p-window) iw) 2)) 0))
               (h-pad (max (round (/ (- (window-height p-window) ih) 2)) 0)))
          (goto-char 1)
          (insert (make-string (if dirvish-show-media-properties 2 h-pad) ?\n)
                  (make-string w-pad ?\s))
          (when dirvish-show-media-properties
            (let* ((beg (progn (goto-char (point-max)) (point)))
                   (file (with-current-buffer (cdr (dv-index dv))
                           (dirvish-prop :index)))
                   (ext (downcase (or (file-name-extension file) "")))
                   (type (cond ((member ext dirvish-image-exts) 'image)
                               ((member ext dirvish-video-exts) 'video)
                               ((and (memq 'pdf-preface
                                           dirvish-preview-dispatchers)
                                     (equal ext "pdf") 'pdf))
                               (t (user-error "Not a media file")))))
              ;; ensure the content is higher than the window height to avoid
              ;; unexpected auto scrolling
              (insert "\n\n\n" (dirvish-media-metadata (cons type file))
                      (make-string (* h-pad 2) ?\n))
              (align-regexp beg (point) "\\(\\\t\\)[^\\\t\\\n]+" 1 4 t)
              (goto-char 1)))))
      buf)))

(cl-defmethod dirvish-preview-dispatch ((recipe (head cache)) dv)
  "Generate cache image according to RECIPE and session DV."
  (let* ((path (dirvish-prop :index))
         (buf (dirvish--util-buffer 'preview dv nil t))
         (name (format "%s-%s-img-cache" path
                       (window-width (dv-preview-window dv)))))
    (unless (get-process name)
      (setq dirvish-media--cache-pool
            (delete (assoc name dirvish-media--cache-pool) dirvish-media--cache-pool))
      (let ((proc (apply #'start-process
                         name (dirvish--util-buffer "img-cache")
                         (cadr recipe) (cddr recipe))))
        (process-put proc 'path path)
        (set-process-sentinel proc #'dirvish-media--cache-sentinel)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (insert " [Dirvish] Generating image cache...") buf)))

(defun dirvish-media--img-size (window &optional height)
  "Get corresponding image width or HEIGHT in WINDOW."
  (let ((size (if height (* dirvish-media--img-scale-h (window-pixel-height window))
                (min (* dirvish-media--img-scale-w (window-pixel-width window))
                     dirvish-media--img-max-width))))
    (floor size)))

(dirvish-define-preview audio (file ext)
  "Preview audio files by printing its metadata.
Require: `mediainfo' (executable)"
  :require (dirvish-mediainfo-program)
  (when (member ext dirvish-audio-exts)
    `(shell . (,dirvish-mediainfo-program ,file))))

(dirvish-define-preview image (file ext preview-window)
  "Preview image files.
Require: `magick' (executable from `imagemagick' suite)"
  :require (dirvish-magick-program)
  (when (member ext dirvish-image-exts)
    (let* ((w (dirvish-media--img-size preview-window))
           (h (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-path
                   file (format "images/%s" w) ".jpg")))
      (cond ((file-exists-p cache)
             `(img . ,(create-image cache nil nil :max-width w :max-height h)))
            ((and (< (file-attribute-size (file-attributes file)) 250000)
                  (member ext '("jpg" "jpeg" "png" "ico" "icns" "bmp" "svg")))
             `(img . ,(create-image file nil nil :max-width w :max-height h)))
            (t `(cache . (,dirvish-magick-program
                          ,file "-define" "jpeg:extent=300kb" "-resize"
                          ,(number-to-string w) ,cache)))))))

(dirvish-define-preview gif (file ext)
  "Preview gif images with animations."
  (when (equal ext "gif")
    (let ((gif (dirvish--find-file-temporarily file))
          (callback (lambda (rcp)
                      (when-let* ((buf (cdr rcp)) ((buffer-live-p buf)))
                        (with-current-buffer buf
                          (image-animate (get-char-property 1 'display)))))))
      (run-with-idle-timer 1 nil callback gif) gif)))

(dirvish-define-preview video (file ext preview-window)
  "Preview video files.
Require: `ffmpegthumbnailer' (executable)"
  :require (dirvish-ffmpegthumbnailer-program)
  (when (member ext dirvish-video-exts)
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-path file (format "images/%s" width) ".jpg")))
      (if (file-exists-p cache)
          `(img . ,(create-image cache nil nil :max-width width :max-height height))
        `(cache . (,dirvish-ffmpegthumbnailer-program "-i" ,file "-o" ,cache "-s"
                         ,(number-to-string width) "-m"))))))

(dirvish-define-preview video-mtn (file ext preview-window)
  "Preview video files on MS-Windows.
Require: `mtn' (executable)"
  :require (dirvish-mtn-program)
  (when (member ext dirvish-video-exts)
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-path file (format "images/%s" width) ".jpg"))
           (path (dirvish--get-parent-path cache)))
      (if (file-exists-p cache)
          `(img . ,(create-image cache nil nil :max-width width :max-height height))
        `(cache . (,dirvish-mtn-program "-P" "-i" "-c" "1" "-r" "1" "-O" ,path ,file "-o"
                   ,(format ".%s.jpg" ext) "-w"
                   ,(number-to-string width)))))))

(dirvish-define-preview epub (file preview-window)
  "Preview epub files.
Require: `epub-thumbnailer' (executable)"
  :require (dirvish-epub-thumbnailer-program)
  (when (equal ext "epub")
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-path file (format "images/%s" width) ".jpg")))
      (if (file-exists-p cache)
          `(img . ,(create-image cache nil nil :max-width width :max-height height))
        `(cache . (,dirvish-epub-thumbnailer-program ,file ,cache ,(number-to-string width)))))))

(dirvish-define-preview pdf (file ext)
  "Preview pdf files.
Require: `pdf-tools' (Emacs package)"
  (when (equal ext "pdf")
    (if (and (require 'pdf-tools nil t)
             (bound-and-true-p pdf-info-epdfinfo-program)
             (file-exists-p pdf-info-epdfinfo-program))
        (dirvish--find-file-temporarily file)
      '(info . "`epdfinfo' program required to preview pdfs; run `M-x pdf-tools-install'"))))

(dirvish-define-preview pdf-preface (file ext preview-window)
  "Display the preface image as preview for pdf files."
  :require (dirvish-pdftoppm-program)
  (when (equal ext "pdf")
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-path file (format "images/%s" width)))
           (cache-jpg (concat cache ".jpg")))
      (if (file-exists-p cache-jpg)
          `(img . ,(create-image cache-jpg nil nil :max-width width :max-height height))
        `(cache . (,dirvish-pdftoppm-program "-jpeg" "-f" "1" "-singlefile" ,file ,cache))))))

(dirvish-define-preview archive (file ext)
  "Preview archive files.
Require: `zipinfo' (executable)
Require: `tar' (executable)"
  :require (dirvish-zipinfo-program dirvish-tar-program)
  (cond ((equal ext "zip") `(shell . (,dirvish-zipinfo-program ,file)))
        ;; Emacs source code files
        ((string-suffix-p ".el.gz" file)
         (dirvish--find-file-temporarily file))
        ((member ext '("tar" "zst" "bz2" "bz" "gz" "xz" "tgz"))
         `(shell . (,dirvish-tar-program "-tvf" ,file)))))

(provide 'dirvish-widgets)
;;; dirvish-widgets.el ends here
