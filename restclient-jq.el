;;; restclient-jq.el --- Support for setting restclient vars from jq expressions -*- lexical-binding: t; -*-
;;
;; Public domain.

;; URL: https://github.com/pashky/restclient.el
;; Author: Cameron Dorrat <cdorrat@gmail.com>
;; Maintainer: Cameron Dorrat <cdorrat@gmail.com>
;; Created: 26 Apr 2020
;; Keywords: tools comm http jq
;; Version: 0.1
;; Package-Requires: ((restclient "20200502.831") (jq-mode "0.4.1") (emacs "24.4"))

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a companion to restclient.el to add support for setting variables from results using jq expressions

;;; Code:
;;
(require 'restclient)
(require 'jq-mode)

;; --- jq support
(defun restclient-jq-result-end-point ()
  "Find the end of a restclient JSON response body."
  (save-excursion
    (goto-char (point-max))
    (or (and (re-search-backward "^[^/].*" nil t)
	     (line-end-position))
	(point-max))))

(defun restclient-jq-get-var (jq-pattern)
  "Find value matching the JQ-PATTERN in a restclient JSON response."

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

(defun restclient-jq-json-var-function (args _args-offset)
  "A restclient result func for setting variables from a JSON response.

ARGS contains the variable name and a jq pattern to use."
  (save-match-data
    (and (string-match "\\(:[^: \n]+\\) \\(.*\\)$" args)
         (let ((var-name (match-string 1 args))
               (jq-patt (match-string 2 args)))
           (lambda ()
             (let ((resp-val (restclient-jq-get-var jq-patt)))
               (restclient-remove-var var-name)
               (restclient-set-var var-name resp-val)
               (message "restclient var [%s = \"%s\"] " var-name resp-val)))))))

(defun restclient-jq-interactive-result ()
  "Run jq interactively on a restclient JSON response buffer."
  (interactive)
  (flush-lines "^//.*") ;; jq doesn't like comments
  (jq-interactively (point-min) (restclient-jq-result-end-point)))

(restclient-register-result-func
 "jq-set-var" #'restclient-jq-json-var-function
 "Set a restclient variable with the value jq expression,
takes var & jq expression as args.
eg. -> jq-set-var :my-token .token")
(define-key restclient-response-mode-map  (kbd "C-c C-j") #'restclient-jq-interactive-result)

(provide 'restclient-jq)

;;; restclient-jq.el ends here
