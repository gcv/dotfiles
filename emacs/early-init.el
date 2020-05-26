(setq package-user-dir "~/.emacs.d/elpa/")
(setq user-emacs-directory "~/.emacs.d/user/")

(tool-bar-mode -1)                                              ; remove the toolbar
(scroll-bar-mode -1)                                            ; no scrollbars (bugs on Mac OS)
(menu-bar-mode -1)                                              ; remove the menu bar

(setq package-enable-at-startup nil)

;; set a very high garbage collection limit to improve startup speed
(setq gc-cons-threshold most-positive-fixnum)
