;;; kurecolor-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from kurecolor.el

(autoload 'kurecolor-hex-set-saturation-in-region "kurecolor" "\
Set the SATURATION of all hex colors found in region.
When region not active, act on the whole buffer.

(fn SATURATION)" t)
(autoload 'kurecolor-hex-set-brightness-in-region "kurecolor" "\
Set the BRIGHTNESS of all hex colors found in region.
When region not active, act on the whole buffer.

(fn BRIGHTNESS)" t)
(autoload 'kurecolor-hex-set-hue-in-region "kurecolor" "\
Set the HUE of all hex colors found in region (BEGIN END).
When region not active, act on the whole buffer.

(fn HUE)" t)
(autoload 'kurecolor-hex-adjust-saturation-in-region "kurecolor" "\
Adjust the SATURATION on all hex colors found in region.
When region not active, act on the whole buffer.

(fn SATURATION)" t)
(autoload 'kurecolor-hex-adjust-brightness-in-region "kurecolor" "\
Set the BRIGHTNESS of all hex colors found in region.
When region not active, act on the whole buffer.

(fn BRIGHTNESS)" t)
(autoload 'kurecolor-increase-brightness-by-step "kurecolor" "\
Increase brightness on hex color at point (or in region) by step.
Accepts universal argument X.

(fn X)" t)
(autoload 'kurecolor-decrease-brightness-by-step "kurecolor" "\
Decrease brightness on hex color at point (or in region) by step.
Accepts universal argument X.

(fn X)" t)
(autoload 'kurecolor-increase-saturation-by-step "kurecolor" "\
Increase saturation on hex color at point (or in region) by step.
Accepts universal argument X.

(fn X)" t)
(autoload 'kurecolor-decrease-saturation-by-step "kurecolor" "\
Decrease saturation on hex color at point (or in region) by step.
Accepts universal argument X.

(fn X)" t)
(autoload 'kurecolor-increase-hue-by-step "kurecolor" "\
Increase hue on hex color at point (or in region) by step.
Accepts universal argument X.

(fn X)" t)
(autoload 'kurecolor-decrease-hue-by-step "kurecolor" "\
Decrease hue on hex color at point (or in region) by step.
Accepts universal argument X.

(fn X)" t)
(autoload 'kurecolor-set-brightness "kurecolor" "\
Interactively change a COLOR's BRIGHTNESS.

(fn COLOR BRIGHTNESS)" t)
(autoload 'kurecolor-set-saturation "kurecolor" "\
Interactively change a COLOR's SATURATION.

(fn COLOR SATURATION)" t)
(autoload 'kurecolor-set-hue "kurecolor" "\
Interactively change a COLOR's HUE.

(fn COLOR HUE)" t)
(autoload 'kurecolor-hex-hue-group "kurecolor" "\
Given a HEX color.
Insert a list of hexcolors of different hue.

(fn HEX)" t)
(autoload 'kurecolor-hex-sat-group "kurecolor" "\
Given a HEX color.
Insert a list of hexcolors of different saturation (sat).

(fn HEX)" t)
(autoload 'kurecolor-hex-val-group "kurecolor" "\
Given a HEX color.
Insert a list of hexcolors of different brightness (val).

(fn HEX)" t)
(autoload 'kurecolor-cssrgb-at-point-or-region-to-hex "kurecolor" "\
CSS `rgb' color at point or region to hex `rgb'." t)
(autoload 'kurecolor-hexcolor-at-point-or-region-to-css-rgb "kurecolor" "\
Hex `rgb' color at point or region to css `rgb' color." t)
(autoload 'kurecolor-hexcolor-at-point-or-region-to-css-rgba "kurecolor" "\
Hex `rgb' color at point or region to css `rgba'.
Opacity is always set to `1.0'." t)
(autoload 'kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba "kurecolor" "\
XCode `colorLiteral' at point to hex `rgba'." t)
(autoload 'kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb "kurecolor" "\
XCode `colorLiteral' at point to hex `rgb'." t)
(autoload 'kurecolor-hex-rgb-at-point-or-region-to-xcode-color-literal "kurecolor" "\
Hex `rgb' to XCode `colorLiteral'." t)
(autoload 'kurecolor-hex-rgba-at-point-or-region-to-xcode-color-literal "kurecolor" "\
Hex `rgba' to XCode `colorLiteral'." t)
(register-definition-prefixes "kurecolor" '("kurecolor-"))

;;; End of scraped data

(provide 'kurecolor-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; kurecolor-autoloads.el ends here
