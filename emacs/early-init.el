;;; -*- lexical-binding: t -*-

(setq package-user-dir (file-truename "~/.emacs.d/elpa/"))
(setq user-emacs-directory (file-truename "~/.emacs.d/user/"))

(when window-system
  (tool-bar-mode -1))                                           
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq package-enable-at-startup nil)

;; set a very high garbage collection limit to improve startup speed
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
