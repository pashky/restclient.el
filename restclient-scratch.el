;;; restclient-scratch.el --- scratch buffer for restclient.el
;;
;; Public domain.

;; Author: Jens de Jong <jensdejong@protonmail.com>
;; Maintainer: Pavel Kurnosov <pashky@gmail.com>
;; Created: 17 Dec 2017
;; Keywords: http scratch

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a companion to restclient.el to have a scratch buffer to write some experimental requests.  Inspired by Emacs's and Cider's scratch buffers.

;;; Code:
;;
(defconst restclient-scratch-buffer-name "*restclient-scratch*")

;;;###autoload
(defun restclient-scratch ()
  "Go to the scratch buffer named `restclient-scratch-buffer-name'."
  (interactive)
  (pop-to-buffer (restclient-find-or-create-scratch-buffer)))

(defun restclient-find-or-create-scratch-buffer ()
  "Find or create the scratch buffer."
  (or (get-buffer restclient-scratch-buffer-name)
      (restclient-create-scratch-buffer)))

(defun restclient-create-scratch-buffer ()
  "Create a new scratch buffer."
  (with-current-buffer (get-buffer-create restclient-scratch-buffer-name)
    (restclient-mode)
    (insert "# -*- restclient -*-\n"
            "# This buffer is for experimental restclient calls.\n\n")
    (current-buffer)))

(provide 'restclient-scratch)

;;; restclient-scratch.el ends here
