;;; restclient-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "restclient" "restclient.el" (0 0 0 0))
;;; Generated autoloads from restclient.el

(autoload 'restclient-http-send-current "restclient" "\
Sends current request.\nOptional argument RAW don't reformat response if t.\nOptional argument STAY-IN-WINDOW do not move focus to response buffer if t.\n\n(fn &optional RAW STAY-IN-WINDOW)" t nil)

(autoload 'restclient-http-send-current-raw "restclient" "\
Sends current request and get raw result (no reformatting or syntax highlight of XML, JSON or images).\n\n(fn)" t nil)

(autoload 'restclient-http-send-current-stay-in-window "restclient" "\
Send current request and keep focus in request window.\n\n(fn)" t nil)

(autoload 'restclient-mode "restclient" "\
Turn on restclient mode.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "restclient" '("restclient-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; restclient-autoloads.el ends here