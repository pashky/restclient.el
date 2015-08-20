;;; ob-restclient.el --- org-babel functions for restclient-mode

;; Copyright (C) Alf Lervåg

;; Author: Alf Lervåg
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


;;; Requirements:

;; restclient-mode

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'restclient)

(defvar org-babel-default-header-args:restclient
  `((:results . "raw"))
  "Default arguments for evaluating a restclient block.")

(defun org-babel-execute:restclient (body params)
  "Execute a block of Restclient code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Restclient source code block")
  (with-current-buffer (get-buffer-create "*ob-restclient*")
    (let ((results-buffer (current-buffer)))
      (with-temp-buffer
        (insert body)
        (restclient-http-parse-current-and-do 'restclient-http-do nil nil results-buffer))
      (sit-for 1)
      (goto-char (point-min))
      (insert "#+BEGIN_SRC html\n")
      (goto-char (point-max))
      (insert "#+END_SRC\n")
      (buffer-string))))

(provide 'ob-restclient)
;;; ob-restclient.el ends here
