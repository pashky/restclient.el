;;; restclient-jq.el --- Support for setting restclient vars from jq expressions
;;
;; Public domain.

;; URL: https://github.com/pashky/restclient.el
;; Author: Cameron Dorrat <cdorrat@gmail.com>
;; Maintainer: Cameron Dorrat <cdorrat@gmail.com>
;; Created: 26 Apr 2020
;; Keywords: http jq
;; Package-Requires: ((restclient "20200502.831") (jq-mode "0.4.1") (emacs "24.4"))

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a companion to restclient.el to add support for setting variables from results using jq expressions

;;; Code:
;;
(require 'jq-mode)
(eval-when-compile (require 'cl-lib)) ;; lexical-let

;; --- jq support
(defun restclient-jq-result-end-point ()
  (save-excursion
    (goto-char (point-max))
    (or (and (re-search-backward "^[^/].*" nil t)
	     (line-end-position))
	(point-max))))

(defun restclient-jq-get-var (jq-pattern)
  (with-temp-buffer
    (let ((output (current-buffer)))
      (with-current-buffer restclient-same-buffer-response-name
        (call-process-region
         (point-min)
         (restclient-jq-result-end-point)
         shell-file-name
         nil
         output
         nil
         shell-command-switch
         (format "%s %s %s"
                 jq-interactive-command
		 "-r"
                 (shell-quote-argument jq-pattern))))
      (string-trim (buffer-string)))))

(defun restclient-jq-json-var-function (args args-offset)
  (save-match-data
   (and (string-match "\\(:[^: \n]+\\) \\(.*\\)$" args)
	(lexical-let ((var-name (match-string 1 args))
		      (jq-patt (match-string 2 args)))
	  (lambda ()
	    (let ((resp-val (restclient-jq-get-var jq-patt)))
	      (restclient-remove-var var-name)
	      (restclient-set-var var-name resp-val)
	      (message "restclient var [%s = \"%s\"] " var-name resp-val)))))))

(defun restclient-jq-interactive-result ()
  (interactive)
  (flush-lines "^//.*") ;; jq doesnt like comments
  (jq-interactively (point-min) (restclient-jq-result-end-point)))


(provide 'restclient-jq)

;; todo: eval-after-load should be used in configuration, not
;; packages. Replace with a better solution.
(eval-after-load 'restclient
  '(progn
     (restclient-register-result-func
      "jq-set-var" #'restclient-jq-json-var-function
      "Set a restclient variable with the value jq expression,
       takes var & jq expression as args.
       eg. -> jq-set-var :my-token .token")
     (define-key restclient-response-mode-map  (kbd "C-c C-j") #'restclient-jq-interactive-result)))

(provide 'restclient-jq)

;;; restclient-jq.el ends here
