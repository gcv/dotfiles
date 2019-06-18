;;; Good resource for making colors: https://www.w3schools.com/colors/colors_picker.asp

(defun /theme-common ()
  (let ((base-font (cond ((member "Menlo" (font-family-list)) "Menlo")
                         ((member "Consolas" (font-family-list)) "Consolas")
                         (t nil))))
    (set-face-attribute 'minibuffer-prompt nil :foreground nil :background nil :family base-font :weight 'bold)
    (set-face-attribute 'mode-line nil :family base-font :underline nil :overline nil)
    (set-face-attribute 'mode-line-inactive nil :underline nil :overline nil)
    (set-face-attribute 'mode-line-buffer-id nil :foreground nil :background nil
                        :family base-font :weight 'bold :slant 'normal)
    (set-face-attribute 'org-code nil :family base-font :weight 'bold)
    (set-face-attribute 'org-checkbox nil :box nil :background nil :foreground nil)
    (set-face-attribute 'org-mode-line-clock nil :foreground nil :background nil)
    (set-face-attribute 'org-mode-line-clock nil :inherit 'unspecified)
    (set-face-attribute 'helm-source-header nil :foreground nil :background nil
                        :family base-font :weight 'bold :height 1.2
                        :box `(:line-width 5 :color ,(face-attribute 'default :background)))
    (set-face-attribute 'helm-selection nil :underline nil)
    (set-face-attribute 'dired-header nil :foreground nil :background nil
                        :family base-font :weight 'bold)
    (set-face-attribute 'bold nil :family base-font :foreground nil :weight 'bold)
    (set-face-attribute 'aw-leading-char-face nil :foreground "red" :background nil
                        :family base-font :height 2.0 :weight 'bold)
    (set-face-attribute 'aw-background-face nil :foreground "gray40" :background nil)
    (set-face-attribute 'fixed-pitch nil :family base-font)
    (set-face-attribute 'term nil :foreground nil :background nil)
    (set-face-attribute 'org-level-1 nil :height 1.0 :background nil :overline nil :box nil :inherit nil)
    (set-face-attribute 'org-level-2 nil :height 1.0 :background nil :overline nil :box nil :inherit nil)
    (set-face-attribute 'org-level-3 nil :height 1.0 :background nil :overline nil :box nil :inherit nil)
    (set-face-attribute 'org-level-4 nil :height 1.0 :background nil :overline nil :box nil :inherit nil)
    (set-face-attribute 'org-level-5 nil :height 1.0 :background nil :overline nil :box nil :inherit nil)
    (set-face-attribute 'org-level-6 nil :height 1.0 :background nil :overline nil :box nil :inherit nil)
    (set-face-attribute 'org-level-7 nil :height 1.0 :background nil :overline nil :box nil :inherit nil)
    (set-face-attribute 'org-level-8 nil :height 1.0 :background nil :overline nil :box nil :inherit nil)))


(defun theme-zenburn ()
  (interactive)
  (disable-all-themes)
  (load-theme 'zenburn t)
  (set-face-background 'default "grey18")
  (set-face-foreground 'default "#cbcbbb")
  (set-face-background 'cursor "grey60")
  (set-face-foreground 'bold "papayawhip")
  (set-face-attribute 'mode-line nil
                      :background "grey30"
                      :foreground "grey68"
                      :box '(:line-width 1 :color "grey30"))
  (let (;;(bg "grey20")
        (bg "grey22"))
    (set-face-attribute 'mode-line-inactive nil
                        :background bg
                        :foreground "grey42"
                        :box `(:line-width 1 :color ,bg)))
  (set-face-background 'highlight "grey35")
  (set-face-underline 'highlight nil)
  (set-face-background 'hl-line "#525252")
  (set-face-background 'region "grey35")
  (set-face-background 'isearch "steelblue4")
  (set-face-background 'lazy-highlight "grey25")
  (set-face-background 'show-paren-match "#5f7f5f")
  (set-face-background 'trailing-whitespace "#ff0000")
  (set-face-attribute 'org-checkbox nil :background nil :foreground "blanchedalmond")
  (set-face-foreground 'org-level-2 "#ccbbdd")
  (set-face-foreground 'org-level-3 "#edcbab")
  (set-face-foreground 'org-archived "grey50")
  (set-face-attribute 'markdown-code-face nil :background "black")
  (set-face-attribute 'company-tooltip-selection nil :background "#224983")
  (set-face-attribute 'company-tooltip-common-selection nil :background "#224983")
  (/theme-common))


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
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 2 :color "#e1e1e1")
                      :foreground "black"
                      :background "#e1e1e1")
  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width 2 :color "#eee8d5")
                      :background "#eee8d5")
  (set-face-attribute 'bold nil :foreground "navy")
  (set-face-attribute 'ido-only-match nil :foreground "#b589aa" :background nil)
  (set-face-attribute 'markdown-pre-face nil :background nil)
  (set-face-attribute 'markdown-code-face nil :background nil)
  (/theme-solarized-common)
  (/theme-common))


(defun theme-solarized-dark ()
  (interactive)
  (disable-all-themes)
  (load-theme 'solarized-dark t)
  (set-face-background 'default "#081a25")
  (set-face-foreground 'default "#adad9c")
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 2 :color "grey40")
                      :foreground "#23313a"
                      :background "grey40")
  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width 2 :color "grey14")
                      :foreground "#888888"
                      :background "grey14")
  (set-face-attribute 'bold nil :foreground "papayawhip")
  (set-face-foreground 'isearch "#0f9999")
  (set-face-background 'isearch "grey12")
  (set-face-foreground 'lazy-highlight "#0a6666")
  (set-face-background 'lazy-highlight "grey12")
  (set-face-foreground 'show-paren-match "black")
  (set-face-background 'show-paren-match "#2e5c99")
  (set-face-foreground 'js2-external-variable "#b37f47")
  (set-face-foreground 'ido-first-match "#64b3b3")
  (set-face-foreground 'ido-only-match nil)
  (set-face-foreground 'font-lock-type-face "#3d9ccc")
  (set-face-background 'font-lock-type-face nil)
  (set-face-foreground 'font-lock-constant-face "#258c84")
  (set-face-foreground 'font-lock-string-face "#1e781e")
  (set-face-foreground 'font-lock-keyword-face "grey65")
  (set-face-foreground 'font-lock-builtin-face "#995c8a")
  (set-face-foreground 'font-lock-function-name-face "#669ccc")
  (set-face-foreground 'font-lock-variable-name-face "#669ccc")
  (set-face-foreground 'font-lock-preprocessor-face "#008877")
  (set-face-attribute 'ido-only-match nil :foreground "#b589aa" :background nil)
  (set-face-foreground 'org-date "#009999")
  (set-face-background 'org-todo "#081a25")
  (set-face-foreground 'org-todo "#995c8a")
  (set-face-foreground 'org-done "#72cccc")
  (set-face-foreground 'org-link "#007799")
  (set-face-foreground 'org-table "#007799")
  (set-face-foreground 'org-level-1 "#7089a6")
  (set-face-foreground 'org-level-2 "#a9a9cc")
  (set-face-foreground 'org-level-3 "#8aba99")
  (set-face-attribute 'org-clock-overlay nil :background "grey50" :foreground "black")
  (/theme-solarized-common)
  (/theme-common))


(defun theme-leuven ()
  (interactive)
  (disable-all-themes)
  (load-theme 'leuven t)
  (set-face-background 'default "#faffff")
  (set-face-attribute 'org-level-2 nil :foreground "#000099")
  (set-face-attribute 'org-level-3 nil :foreground "#0000cc")
  (set-face-attribute 'isearch nil :background "lightblue" :foreground nil :underline nil)
  (set-face-attribute 'lazy-highlight nil :background "#e6e6fa" :underline nil)
  (set-face-attribute 'trailing-whitespace nil :background "red")
  (set-face-attribute 'js2-error nil :box nil)
  (set-face-attribute 'aw-background-face nil :foreground "gray60" :background nil)
  (set-face-attribute 'mode-line-inactive nil :background "#dbdcda" :box '(:line-width 1 :color "#f0f0ef"))
  (/theme-common))


(defun /theme-material-common ()
  (set-face-attribute 'cursor nil :background "#21bdff"))


(defun theme-material-dark ()
  (interactive)
  (disable-all-themes)
  (load-theme 'material t)
  ;; NB: :box '(:line-width -n) draws a box internally, without increasing the rendered size of the text!
  (set-face-attribute 'mode-line nil
                      :background "#35575b"
                      :box '(:line-width 2 :color "#35575b"))
  (set-face-attribute 'mode-line-inactive nil
                      :background "black"
                      :box '(:line-width 2 :color "black"))
  (set-face-attribute 'fringe nil :background "#35575b")
  (set-face-attribute 'vertical-border nil :foreground "#dcdccc")
  (set-face-attribute 'magit-diff-removed-highlight nil :foreground "red")
  (set-face-attribute 'markdown-inline-code-face nil :background "black" :foreground "#8bc34a")
  (set-face-attribute 'markdown-pre-face nil :background "black")
  (set-face-attribute 'markdown-code-face nil :background "black")
  (/theme-material-common)
  (/theme-common))


(defun theme-material-light ()
  (interactive)
  (disable-all-themes)
  (load-theme 'material-light t)
  (set-face-attribute 'aw-background-face nil :foreground "gray40" :background nil)
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 1 :color "#90a4ae"))
  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width 1 :color "#eceff1"))
  (set-face-attribute 'header-line nil :box nil)
  (set-face-attribute 'markdown-pre-face nil :background nil)
  (set-face-attribute 'markdown-code-face nil :background nil)
  (/theme-material-common)
  (/theme-common))


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
    (set-face-attribute 'default nil :foreground base-color)
    (set-face-attribute 'cursor nil :background "#00cc00")
    (set-face-attribute 'show-paren-match nil :background base-color-darker-1)
    (set-face-background 'hl-line "darkgreen")
    (set-face-attribute 'mode-line nil
                        :foreground "black"
                        :background base-color-lighter-1
                        :box `(:line-width 2 :color ,base-color-lighter-1))
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "black"
                        :background base-color-darker-2
                        :box `(:line-width 2 :color ,base-color-darker-2))
    (set-face-attribute 'header-line nil :foreground "black" :background base-color-darker-2)
    (set-face-attribute 'org-hide nil :foreground "black" :background "black")
    (set-face-attribute 'org-level-1 nil :foreground blue-lighter-3)
    (set-face-attribute 'org-level-2 nil :foreground blue-lighter-2)
    (set-face-attribute 'org-level-3 nil :foreground blue-lighter-1)
    (set-face-attribute 'org-level-4 nil :foreground blue)
    (set-face-attribute 'org-special-keyword nil :foreground "#007799")
    (set-face-attribute 'helm-source-header nil :foreground "black" :background base-color)
    (set-face-attribute 'helm-selection nil :background base-color-lighter-1)
    (set-face-attribute 'markdown-pre-face nil :background nil)
    (set-face-attribute 'markdown-code-face nil :background nil)
    (set-face-attribute 'isearch nil :background base-color-lighter-1 :foreground "black" :underline nil)
    (set-face-attribute 'lazy-highlight nil :background base-color-darker-1 :foreground "black" :underline nil))
  (/theme-common))


(defun theme-anti-zenburn ()
  (interactive)
  (load-theme 'anti-zenburn t)
  (let (;;(bg "#d4d4d4")
        (bg "#d9d9d9"))
    (set-face-attribute 'mode-line nil
                        :background bg
                        :box `(:line-width 2 :color ,bg))
    (set-face-attribute 'mode-line-inactive nil
                        :box '(:line-width 2 :color "#c7c7c7")))
  (/theme-common))
