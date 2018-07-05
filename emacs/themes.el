(defun cv--theme-common ()
  (let ((base-font (cond ((member "Menlo" (font-family-list)) "Menlo")
                         ((member "Consolas" (font-family-list)) "Consolas")
                         (t nil))))
    (set-face-attribute 'minibuffer-prompt nil :family base-font)
    (set-face-attribute 'mode-line nil :family base-font)
    (set-face-attribute 'mode-line-buffer-id nil :foreground nil :background nil
                        :family base-font :weight 'bold :slant 'normal)
    (set-face-attribute 'org-code nil :family base-font :weight 'bold)
    (set-face-attribute 'org-checkbox nil :box nil :background nil :foreground nil)
    (set-face-attribute 'org-mode-line-clock nil :foreground nil :background nil)
    (set-face-attribute 'org-mode-line-clock nil :inherit 'unspecified)
    (set-face-attribute 'helm-source-header nil :foreground nil :background nil
                        :family base-font :weight 'bold)
    (set-face-attribute 'helm-selection nil :underline nil)
    (set-face-attribute 'dired-header nil :foreground nil :background nil
                        :family base-font :weight 'bold)
    (set-face-attribute 'bold nil :family base-font :weight 'bold)
    (set-face-attribute 'aw-leading-char-face nil :foreground "red" :background nil
                        :family base-font :height 2.0 :weight 'bold)
    (set-face-attribute 'aw-background-face nil :foreground "gray40" :background nil)
    (set-face-attribute 'term nil :foreground nil :background nil)))


(defun theme-zenburn ()
  (interactive)
  (disable-all-themes)
  (load-theme 'zenburn t)
  (set-face-background 'default "grey12")
  (set-face-foreground 'default "#cbcbbb")
  (set-face-background 'cursor "grey60")
  (set-face-foreground 'bold "papayawhip")
  (set-face-background 'mode-line "grey25")
  (set-face-foreground 'mode-line "grey65")
  (set-face-background 'mode-line-inactive "grey12")
  (set-face-foreground 'mode-line-inactive "grey35")
  (set-face-background 'highlight "grey35")
  (set-face-underline 'highlight nil)
  ;;(set-face-background 'hl-line "#343335")
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
  (cv--theme-common))


(defun cv--theme-solarized-common ()
  (setq solarized-distinct-fringe-background t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-less-bold nil)
  (setq solarized-use-more-italic nil)
  (setq solarized-scale-org-headlines t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil))


(defun theme-solarized-light ()
  (interactive)
  (disable-all-themes)
  (load-theme 'solarized-light t)
  (set-face-attribute 'bold nil :foreground "navy")
  (set-face-attribute 'ido-only-match nil :foreground "#b589aa" :background nil)
  (cv--theme-solarized-common)
  (cv--theme-common))


(defun theme-solarized-dark ()
  (interactive)
  (disable-all-themes)
  (load-theme 'solarized-dark t)
  (set-face-background 'default "#081a25")
  (set-face-foreground 'default "#adad9c")
  (set-face-attribute 'bold nil :foreground "papayawhip")
  (set-face-foreground 'isearch "#0f9999")
  (set-face-background 'isearch "grey12")
  (set-face-foreground 'lazy-highlight "#0a6666")
  (set-face-background 'lazy-highlight "grey12")
  (set-face-foreground 'mode-line-inactive "#888888")
  (set-face-background 'mode-line-inactive "grey14")
  (set-face-foreground 'mode-line "#23313a")
  (set-face-background 'mode-line "grey40")
  (set-face-attribute 'mode-line nil :box nil)
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
  (cv--theme-solarized-common)
  (cv--theme-common))


(defun theme-leuven ()
  (interactive)
  (disable-all-themes)
  (load-theme 'leuven t)
  (set-face-background 'default "#faffff")
  (set-face-attribute 'minibuffer-prompt nil :background nil)
  (set-face-attribute 'org-level-1 nil :height 1.0 :background nil :overline nil)
  (set-face-attribute 'org-level-2 nil :height 1.0 :background nil :foreground "#000099" :overline nil)
  (set-face-attribute 'org-level-3 nil :height 1.0 :background nil :foreground "#0000cc" :overline nil)
  (set-face-attribute 'isearch nil :background "lightblue" :foreground nil :underline nil)
  (set-face-attribute 'lazy-highlight nil :background "#e6e6fa" :underline nil)
  (set-face-attribute 'trailing-whitespace nil :background "red")
  (set-face-attribute 'js2-error nil :box nil)
  (set-face-attribute 'aw-background-face nil :foreground "gray60" :background nil)
  (set-face-attribute 'mode-line-inactive nil :background "#dbdcda" :box '(:line-width 1 :color "#f0f0ef"))
  (cv--theme-common))


(defun cv--theme-material-common ()
  (set-face-attribute 'cursor nil :background "#21bdff")
  (set-face-attribute 'org-level-1 nil :height 1.0 :box nil)
  (set-face-attribute 'org-level-2 nil :height 1.0 :box nil)
  (set-face-attribute 'org-level-3 nil :height 1.0 :box nil))


(defun theme-material-dark ()
  (interactive)
  (disable-all-themes)
  (load-theme 'material t)
  ;; NB: :box '(:line-width -n) draws a box internally, without increasing the rendered size of the text!
  (set-face-attribute 'mode-line nil :box '(:line-width 1 :color "#35575b"))
  (set-face-attribute 'mode-line nil :background "#35575b")
  (set-face-attribute 'mode-line-inactive nil :background "black")
  (set-face-attribute 'fringe nil :background "#35575b")
  (set-face-attribute 'vertical-border nil :foreground "#dcdccc")
  (set-face-attribute 'magit-diff-removed-highlight nil :foreground "red")
  (set-face-attribute 'markdown-inline-code-face nil :background "black" :foreground "#8bc34a")
  (set-face-attribute 'markdown-pre-face nil :background "black")
  (set-face-attribute 'markdown-code-face nil :background "black")
  (cv--theme-material-common)
  (cv--theme-common))


(defun theme-material-light ()
  (interactive)
  (disable-all-themes)
  (load-theme 'material-light t)
  (set-face-attribute 'aw-background-face nil :foreground "gray40" :background nil)
  (set-face-attribute 'mode-line nil :box '(:line-width 1 :color "#90a4ae"))
  (set-face-attribute 'mode-line-inactive nil :box '(:line-width 1 :color "#eceff1"))
  (cv--theme-material-common)
  (cv--theme-common))
