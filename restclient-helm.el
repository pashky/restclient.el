;;; restclient-helm.el --- helm interface for restclient.el
;;
;; Public domain.

;; Author: Pavel Kurnosov <pashky@gmail.com>
;; Maintainer: Pavel Kurnosov <pashky@gmail.com>
;; Created: 01 Apr 2016
;; Keywords: http helm
;; Package-Requires: ((restclient "0") (helm "1.9.4"))

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a companion to restclient.el to add helm sources for requests and variables in current restclient-mode buffer.

;;; Code:
;;
(require 'helm)
(require 'helm-utils)
(require 'restclient)

(defun restclient-helm-find-candidates-matching (regexp process)
  (let ((result '()))
    (with-helm-current-buffer
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings
          (font-lock-fontify-buffer)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (setq result (cons (cons (funcall process) (line-number-at-pos)) result))))
      result)))

(defun restclient-helm-find-requests ()
  (restclient-helm-find-candidates-matching
   restclient-method-url-regexp
   '(lambda () (match-string 0))))

(defun restclient-helm-find-variables ()
  (restclient-helm-find-candidates-matching
   restclient-var-regexp
   '(lambda () (match-string 1))))

(defun restclient-helm-goto (candidate)
  (switch-to-buffer helm-current-buffer)
  (helm-goto-line candidate))

(defconst restclient-helm-requests-source
  (helm-build-sync-source "Variables"
    :action '(("Go to declaration" . restclient-helm-goto))
    :candidates 'restclient-helm-find-variables))

(defconst restclient-helm-variables-source
  (helm-build-sync-source "Requests"
    :action '(("Go to" . restclient-helm-goto))
    :candidates 'restclient-helm-find-requests))

;;;###autoload
(defun helm-restclient ()
  "Helm for Restclient."
  (interactive)
  (helm :sources '(restclient-helm-requests-source restclient-helm-variables-source)))

(provide 'restclient-helm)

(eval-after-load 'restclient
  '(progn
     (define-key restclient-mode-map (kbd "C-c C-g") #'helm-restclient)))

;;; restclient-helm.el ends here
