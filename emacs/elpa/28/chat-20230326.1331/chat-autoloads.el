;;; chat-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "chat" "chat.el" (0 0 0 0))
;;; Generated autoloads from chat.el

(register-definition-prefixes "chat" '("chat"))

;;;### (autoloads nil "chat" "chat.el" (0 0 0 0))
;;; Generated autoloads from chat.el

(autoload 'chat-query-user "chat" "\
Insert ChatGPTs response to INPUT.
If INSERT is non-nil, the text is inserted into the current buffer.

\(fn INPUT &optional INSERT)" t nil)

(autoload 'chat-query-region "chat" "\
Apply INPUT to the region bounded by REG-BEG and REG-END.
MODE determines what is done with the result.

- If nil, a new buffer is created to hold the output.

- If mode is 4 (\\[universal-argument]), then `chat-query-region'
  inserts its contents after point.
- If mode is 16 (\\[universal-argument] \\[universal-argument]), then `chat-query-region'
  replaces the region with ChatGPT's response.

\(fn REG-BEG REG-END &optional MODE)" t nil)

(autoload 'chat-query-dwim "chat" "\
Query ChatGPT, getting input via a region or with the prompt.

This is not designed for programmatic use.  ARG is passed as the
mode controller to `chat-query-user' and `chat-query-region'.

\(fn &optional ARG)" t nil)

(autoload 'chat "chat" "\
Enter an interactive session with ChatGPT.
If ARG is non-nil, switch to a new buffer instead of popping to a new buffer.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "chat" '("chat-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; chat-autoloads.el ends here
