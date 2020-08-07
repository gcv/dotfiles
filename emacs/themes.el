;;; -*- mode: emacs-lisp; mode: rainbow; -*-

;;; Good resource for making colors: https://www.w3schools.com/colors/colors_picker.asp
;;; Also try the mac-color-picker AppleScript wrapper.

(require 'a)
(require 'dash)


;;; XXX: Emacs 27 breaks custom-theme-set-faces.
;;; https://emacs.stackexchange.com/a/52804
(unless (version< emacs-version "27.0")
  (setq custom--inhibit-theme-enable nil))


(defun /theme-face-spec (face &rest merge-attrs)
  (let* ((attrs (a-merge (ignore-errors (face-all-attributes face (selected-frame)))
                         (apply #'a-alist merge-attrs))))
    `(,face ((t ,(-interleave (a-keys attrs) (a-vals attrs)))))))


(defun /theme-common ()
  (let ((base-font (cond ((member "Menlo" (font-family-list)) "Menlo")
                         ((member "Consolas" (font-family-list)) "Consolas")
                         (t nil))))
    (list
     (/theme-face-spec 'minibuffer-prompt :foreground nil :background nil :family base-font :weight 'bold)
     (/theme-face-spec 'mode-line :family base-font :underline nil :overline nil)
     (/theme-face-spec 'mode-line-inactive :underline nil :overline nil)
     (/theme-face-spec 'mode-line-buffer-id :foreground nil :background nil :family base-font :weight 'bold :slant 'normal)
     (/theme-face-spec 'org-code :family base-font :weight 'bold)
     (/theme-face-spec 'org-checkbox :box nil :background nil :foreground nil)
     (/theme-face-spec 'org-mode-line-clock :foreground nil :background nil)
     (/theme-face-spec 'org-mode-line-clock :inherit 'unspecified)
     (/theme-face-spec 'helm-source-header
                       :foreground nil :background nil
                       :family base-font :weight 'bold :height 1.2
                       :box `(:line-width 5 :color ,(face-attribute 'default :background)))
     (/theme-face-spec 'helm-selection :underline nil)
     (/theme-face-spec 'dired-header :foreground nil :background nil :family base-font :weight 'bold)
     (/theme-face-spec 'bold :family base-font :foreground nil :weight 'bold)
     (/theme-face-spec 'aw-leading-char-face :foreground "red" :background nil :family base-font :height 2.0 :weight 'bold)
     (/theme-face-spec 'aw-background-face :foreground "gray40" :background nil)
     (/theme-face-spec 'fixed-pitch :family base-font)
     (/theme-face-spec 'term :foreground nil :background nil)
     (/theme-face-spec 'markdown-pre-face :background nil :weight 'bold)
     (/theme-face-spec 'markdown-code-face :background (face-attribute 'org-code :background))
     (/theme-face-spec 'markdown-inline-code-face :background nil :weight 'bold)
     (/theme-face-spec 'org-level-1 :height 1.0 :background nil :overline nil :box nil :inherit nil)
     (/theme-face-spec 'org-level-2 :height 1.0 :background nil :overline nil :box nil :inherit nil)
     (/theme-face-spec 'org-level-3 :height 1.0 :background nil :overline nil :box nil :inherit nil)
     (/theme-face-spec 'org-level-4 :height 1.0 :background nil :overline nil :box nil :inherit nil)
     (/theme-face-spec 'org-level-5 :height 1.0 :background nil :overline nil :box nil :inherit nil)
     (/theme-face-spec 'org-level-6 :height 1.0 :background nil :overline nil :box nil :inherit nil)
     (/theme-face-spec 'org-level-7 :height 1.0 :background nil :overline nil :box nil :inherit nil)
     (/theme-face-spec 'org-level-8 :height 1.0 :background nil :overline nil :box nil :inherit nil))))


(defun theme-zenburn ()
  (interactive)
  (disable-all-themes)
  (load-theme 'zenburn t)
  (custom-theme-set-faces
   'zenburn
   (/theme-face-spec 'default :background "grey18" :foreground "#cbcbbb")
   (/theme-face-spec 'cursor :background "grey60")
   (/theme-face-spec 'bold :foreground "papayawhip")
   (let ((bg "grey30"))
     (/theme-face-spec 'mode-line :background bg :foreground "grey68" :box `(:line-width 1 :color ,bg)))
   (let (;;(bg "grey20")
         (bg "grey22"))
     (/theme-face-spec 'mode-line-inactive :background bg :foreground "grey42" :box `(:line-width 1 :color ,bg)))
   (/theme-face-spec 'highlight :background "grey35" :underline nil)
   (/theme-face-spec 'hl-line :background "#525252")
   (/theme-face-spec 'region :background "grey35")
   (/theme-face-spec 'isearch :background "steelblue4")
   (/theme-face-spec 'lazy-highlight :background "grey25")
   (/theme-face-spec 'show-paren-match :background "#5f7f5f")
   (/theme-face-spec 'trailing-whitespace :background "#ff0000")
   (/theme-face-spec 'org-checkbox :background nil :foreground "blanchedalmond")
   (/theme-face-spec 'org-level-2 :foreground "#ccbbdd")
   (/theme-face-spec 'org-level-3 :foreground "#edcbab")
   (/theme-face-spec 'org-archived :foreground "grey50")
   (/theme-face-spec 'company-tooltip-selection :background "#224983")
   (/theme-face-spec 'company-tooltip-common-selection :background "#224983"))
  (apply #'custom-theme-set-faces 'zenburn (/theme-common)))


(defun /theme-solarized-common ()
  (setq solarized-distinct-fringe-background t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-less-bold nil)
  (setq solarized-use-more-italic nil)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil))


(defun theme-solarized-light ()
  (interactive)
  (disable-all-themes)
  (load-theme 'solarized-light t)
  (custom-theme-set-faces
   'solarized-light
   (let ((bg "#e1e1e1"))
     (/theme-face-spec 'mode-line :background bg :foreground "black" :box `(:line-width 2 :color ,bg)))
   (let ((bg "#eee8d5"))
     (/theme-face-spec 'mode-line-inactive :background bg :box `(:line-width 2 :color ,bg)))
   (/theme-face-spec 'bold :foreground "navy")
   (/theme-face-spec 'ido-only-match :foreground "#b589aa" :background nil))
  (apply #'custom-theme-set-faces 'solarized-light (/theme-common))
  (/theme-solarized-common))


(defun theme-solarized-dark ()
  (interactive)
  (disable-all-themes)
  (load-theme 'solarized-dark t)
  (custom-theme-set-faces
   'solarized-dark
   (/theme-face-spec 'default :background "#081a25" :foreground "#adad9c")
   (let ((bg "grey40"))
     (/theme-face-spec 'mode-line :background bg :foreground "#23313a" :box `(:line-width 2 :color ,bg)))
   (let ((bg "grey14"))
     (/theme-face-spec 'mode-line-inactive :background bg :foreground "#888888" :box `(:line-width 2 :color ,bg)))
   (/theme-face-spec 'bold :foreground "papayawhip")
   (/theme-face-spec 'isearch :foreground "#0f9999" :background "grey12")
   (/theme-face-spec 'lazy-highlight :foreground "#0a6666" :background "grey12")
   (/theme-face-spec 'show-paren-match :foreground "black" :background "#2e5c99")
   (/theme-face-spec 'js2-external-variable :foreground "#b37f47")
   (/theme-face-spec 'ido-first-match :foreground "#64b3b3")
   (/theme-face-spec 'ido-only-match :foreground nil)
   (/theme-face-spec 'font-lock-type-face :foreground "#3d9ccc" :background nil)
   (/theme-face-spec 'font-lock-constant-face :foreground "#258c84")
   (/theme-face-spec 'font-lock-string-face :foreground "#1e781e")
   (/theme-face-spec 'font-lock-keyword-face :foreground "grey65")
   (/theme-face-spec 'font-lock-builtin-face :foreground "#995c8a")
   (/theme-face-spec 'font-lock-function-name-face :foreground "#669ccc")
   (/theme-face-spec 'font-lock-variable-name-face :foreground "#669ccc")
   (/theme-face-spec 'font-lock-preprocessor-face :foreground "#008877")
   (/theme-face-spec 'ido-only-match :foreground "#b589aa" :background nil)
   (/theme-face-spec 'org-date :foreground "#009999")
   (/theme-face-spec 'org-todo :foreground "#995c8a" :background "#081a25")
   (/theme-face-spec 'org-done :foreground "#72cccc")
   (/theme-face-spec 'org-link :foreground "#007799")
   (/theme-face-spec 'org-table :foreground "#007799")
   (/theme-face-spec 'org-level-1 :foreground "#7089a6")
   (/theme-face-spec 'org-level-2 :foreground "#a9a9cc")
   (/theme-face-spec 'org-level-3 :foreground "#8aba99")
   (/theme-face-spec 'org-clock-overlay :background "grey50" :foreground "black"))
  (apply #'custom-theme-set-faces 'solarized-dark (/theme-common))
  (/theme-solarized-common))


(defun theme-leuven ()
  (interactive)
  (disable-all-themes)
  (load-theme 'leuven t)
  (custom-theme-set-faces
   'leuven
   (/theme-face-spec 'default :background "#faffff")
   (/theme-face-spec 'org-level-2 :foreground "#000099")
   (/theme-face-spec 'org-level-3 :foreground "#0000cc")
   (/theme-face-spec 'isearch :background "lightblue" :foreground nil :underline nil)
   (/theme-face-spec 'lazy-highlight :background "#e6e6fa" :underline nil)
   (/theme-face-spec 'trailing-whitespace :background "red")
   (/theme-face-spec 'js2-error :box nil)
   (/theme-face-spec 'aw-background-face :foreground "gray60" :background nil)
   (let ((bg "#335ea8"))
     (/theme-face-spec 'mode-line :background bg :box `(:line-width 2 :color ,bg)))
   (let ((bg "#dbdcda"))
     (/theme-face-spec 'mode-line-inactive :background bg :box `(:line-width 2 :color ,bg))))
  (apply #'custom-theme-set-faces 'leuven (/theme-common)))


(defun theme-material-dark ()
  (interactive)
  (disable-all-themes)
  (load-theme 'material t)
  (custom-theme-set-faces
   'material
   (/theme-face-spec 'cursor :background "#21bdff")
   ;; NB: :box '(:line-width -n) draws a box internally, without increasing the rendered size of the text!
   (let ((bg "#35575b"))
     (/theme-face-spec 'mode-line :background bg :box `(:line-width 2 :color ,bg)))
   (let ((bg "black"))
     (/theme-face-spec 'mode-line-inactive :background bg :box `(:line-width 2 :color ,bg)))
   (/theme-face-spec 'fringe :background "#35575b")
   (/theme-face-spec 'vertical-border :foreground "#dcdccc")
   (/theme-face-spec 'magit-diff-removed-highlight :foreground "red"))
  (apply #'custom-theme-set-faces 'material (/theme-common)))


(defun theme-material-light ()
  (interactive)
  (disable-all-themes)
  (load-theme 'material-light t)
  (custom-theme-set-faces
   'material-light
   (/theme-face-spec 'cursor :background "#21bdff")
   (/theme-face-spec 'aw-background-face :foreground "gray40" :background nil)
   (let ((fg "black")
         (bg "#c5c8ca"))
     (/theme-face-spec 'mode-line :foreground fg :background bg :box `(:line-width 1 :color ,bg)))
   (let ((fg "gray30")
         (bg "#d4d7d9"))
     (/theme-face-spec 'mode-line-inactive :foreground fg :background bg :box `(:line-width 1 :color ,bg)))
   (/theme-face-spec 'header-line :box nil))
  (apply #'custom-theme-set-faces 'material-light (/theme-common)))


(defun theme-modus-operandi-light ()
  (interactive)
  (disable-all-themes)
  (load-theme 'modus-operandi t)
  (custom-theme-set-faces
   'modus-operandi
   (/theme-face-spec 'org-hide :foreground "#ffffff" :background "#ffffff"))
  (apply #'custom-theme-set-faces 'modus-operandi (/theme-common)))


(defun theme-modus-vivendi-dark ()
  (interactive)
  (disable-all-themes)
  (load-theme 'modus-vivendi t)
  (custom-theme-set-faces
   'modus-vivendi
   (/theme-face-spec 'org-hide :foreground "black" :background "black"))
  (apply #'custom-theme-set-faces 'modus-vivendi (/theme-common)))


(defun theme-green-phosphor ()
  (interactive)
  (disable-all-themes)
  (load-theme 'green-phosphor t)
  (let ((base-color "#159e2b")
        (base-color-darker-1 "#107d23")
        (base-color-darker-2 "#0f711f")
        (base-color-lighter-1 "#1acb38")
        (base-color-lighter-2 "#1de23e")
        (blue "#7089a6")
        (blue-lighter-1 "#91a4ba")
        (blue-lighter-2 "#b1bece")
        (blue-lighter-3 "#d0d8e2")
        (blue-darker-1 "#627e9d")
        (blue-darker-2 "#58718d"))
    (custom-theme-set-faces
     'green-phosphor
     (/theme-face-spec 'default :foreground base-color)
     (/theme-face-spec 'cursor :background "#00cc00")
     (/theme-face-spec 'show-paren-match :background base-color-darker-1)
     (/theme-face-spec 'hl-line :background "darkgreen")
     (let ((bg base-color-lighter-1))
       (/theme-face-spec 'mode-line :background bg :foreground "black" :box `(:line-width 2 :color ,bg)))
     (let ((bg base-color-darker-2))
       (/theme-face-spec 'mode-line-inactive :background bg :foreground "black" :box `(:line-width 2 :color ,bg)))
     (/theme-face-spec 'header-line :foreground "black" :background base-color-darker-2)
     (/theme-face-spec 'org-hide :foreground "black" :background "black")
     (/theme-face-spec 'org-level-1 :foreground blue-lighter-3)
     (/theme-face-spec 'org-level-2 :foreground blue-lighter-2)
     (/theme-face-spec 'org-level-3 :foreground blue-lighter-1)
     (/theme-face-spec 'org-level-4 :foreground blue)
     (/theme-face-spec 'org-special-keyword :foreground "#007799")
     (/theme-face-spec 'helm-source-header :foreground "black" :background base-color)
     (/theme-face-spec 'helm-selection :background base-color-lighter-1)
     (/theme-face-spec 'isearch :background base-color-lighter-1 :foreground "black" :underline nil)
     (/theme-face-spec 'lazy-highlight :background base-color-darker-1 :foreground "black" :underline nil)))
  (apply #'custom-theme-set-faces 'green-phosphor (/theme-common)))


(defun theme-wilmersdorf ()
  (interactive)
  (disable-all-themes)
  (load-theme 'doom-wilmersdorf t)
  (custom-theme-set-faces
   'doom-wilmersdorf
   (let ((bg "#515462"))
     (/theme-face-spec 'mode-line :background bg :box `(:line-width 1 :color ,bg)))
   (let ((bg (face-attribute 'mode-line-inactive :background)))
     (/theme-face-spec 'mode-line-inactive :box `(:line-width 1 :color ,bg)))
   (/theme-face-spec 'ivy-posframe-border :background "#515462")
   (/theme-face-spec 'fringe :background "#515462" :foreground "#c6c6c6"))
  (apply #'custom-theme-set-faces 'doom-wilmersdorf (/theme-common)))
