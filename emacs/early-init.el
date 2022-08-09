;;; -*- lexical-binding: t -*-

(setq package-user-dir (file-truename (concat "~/.emacs.d/elpa/" (number-to-string emacs-major-version))))
(let ((system-hostname (or (ignore-errors
                             (car (split-string (system-name) "\\.")))
                           "unknown-host")))
  (setq user-emacs-directory (file-truename (concat "~/.emacs.d/user/" system-hostname "/"))))

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
