;;; svg-clock-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from svg-clock.el

(autoload 'svg-clock-insert "svg-clock" "\
Insert a self-updating image displaying an analog clock at point.
Optional argument SIZE the size of the clock in pixels.
Optional argument FOREGROUND the foreground color.
Optional argument BACKGROUND the background color.
Optional argument OFFSET the offset in seconds between current and displayed
time.
Optional argument NO-SECONDS says whether to do a seconds hand.
Optional argument NO-FACE says whether to decorate the face.

(fn &optional SIZE FOREGROUND BACKGROUND OFFSET NO-SECONDS NO-FACE)")
(autoload 'svg-clock "svg-clock" "\
Start/stop the svg clock.

(fn &key SIZE FOREGROUND BACKGROUND NO-SECONDS NO-FACE)" t)
(register-definition-prefixes "svg-clock" '("svg-clock-"))

;;; End of scraped data

(provide 'svg-clock-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; svg-clock-autoloads.el ends here