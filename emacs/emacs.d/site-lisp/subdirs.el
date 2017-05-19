(defun add-subdirs-to-load-path (dir)
  (let ((default-directory (concat dir "/")))
    (normal-top-level-add-subdirs-to-load-path)))

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path))

