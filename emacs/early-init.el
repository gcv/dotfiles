;;; -*- lexical-binding: t -*-

;; turn off the native compiler by default
(setq native-comp-speed -1)
(setq native-comp-async-report-warnings-errors nil)

(setq package-user-dir (file-truename (concat "~/.emacs.d/elpa/" (number-to-string emacs-major-version))))
(let* ((system-hostname (or (ignore-errors
                              (car (split-string (system-name) "\\.")))
                            "unknown-host"))
       (user-data-dir (file-truename (concat "~/.emacs.d/user/" system-hostname "/")))
       (site-lisp-dir (concat user-data-dir "site-lisp/")))
  (unless (file-directory-p site-lisp-dir) (make-directory site-lisp-dir))
  (setq user-emacs-directory user-data-dir))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(unless (and window-system (eq 'darwin system-type))
  ;; For some reason, fullscreen mode on Mac does not work with menu-bar-mode
  ;; turned off. Otherwise, it should be turned off.
  (menu-bar-mode -1))

(setq package-enable-at-startup nil)

;; set a very high garbage collection limit to improve startup speed
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
