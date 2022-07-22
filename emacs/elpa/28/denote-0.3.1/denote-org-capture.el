;;; denote-org-capture.el --- Integration between Denote and org-capture -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote
;; Version: 0.3.1
;; Package-Requires: ((emacs "27.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Denote integration with org-capture.
;;
;; Samples of an `org-capture-templates' entry:
;;
;;     (setq org-capture-templates
;;           '(("n" "New note (with denote.el)" plain
;;              (file denote-last-path)
;;              #'denote-org-capture
;;              :no-save t
;;              :immediate-finish nil
;;              :kill-buffer t
;;              :jump-to-captured t)))
;;
;;     (with-eval-after-load 'org-capture
;;       (add-to-list 'org-capture-templates
;;                    '("n" "New note (with denote.el)" plain
;;                      (file denote-last-path)
;;                      #'denote-org-capture
;;                      :no-save t
;;                      :immediate-finish nil
;;                      :kill-buffer t
;;                      :jump-to-captured t)))
;;
;; Note that `denote-org-capture' ignores the `denote-file-type': it
;; always sets the Org file extension for the created note to ensure
;; that the capture process works as intended, especially for the
;; desired output of the `denote-org-capture-specifiers'.

;;; Code:

(require 'denote)

(defgroup denote-org-capture ()
  "Integration between Denote and Org Capture."
  :group 'denote)

(defcustom denote-org-capture-specifiers "%l\n%i\n%?"
  "String with format specifiers for `org-capture-templates'.
Check that variable's documentation for the details.

This string is appended to new notes in the `denote-org-capture'
function.  Every new note has the standard front matter we
define."
  :type 'string
  :group 'denote-org-capture)

;;;###autoload
(defun denote-org-capture ()
  "Create new note through `org-capture-templates'.
Use this as a function that returns the path to the new file.
The file is populated with Denote's front matter.  It can then be
expanded with the usual specifiers or strings that
`org-capture-templates' supports.

Note that this function ignores the `denote-file-type': it always
sets the Org file extension for the created note to ensure that
the capture process works as intended, especially for the desired
output of the `denote-org-capture-specifiers'.

Consult the manual for template samples."
  (let ((title (denote--title-prompt))
        (keywords (denote--keywords-prompt))
        (denote-file-type nil)) ; we enforce the .org extension for `org-capture'
    (denote--path title keywords)
    (denote--prepare-note denote-last-title denote-last-keywords denote-last-path)
    (denote--keywords-add-to-history denote-last-keywords)
    (concat denote-last-front-matter denote-org-capture-specifiers)))

(defun denote-org-capture-delete-empty-file ()
  "Delete file if capture with `denote-org-capture' is aborted."
  (when-let* ((file denote-last-path)
              ((denote--file-empty-p file)))
    (delete-file denote-last-path)))

(add-hook 'org-capture-after-finalize-hook #'denote-org-capture-delete-empty-file)

(provide 'denote-org-capture)
;;; denote-org-capture.el ends here
