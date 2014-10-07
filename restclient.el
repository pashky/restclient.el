;;; restclient.el --- An interactive HTTP client for Emacs

;; Public domain.

;; Author: Pavel Kurnosov <pashky@gmail.com>
;; Maintainer: Pavel Kurnosov <pashky@gmail.com>
;; Created: 01 Apr 2012
;; Keywords: http

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

(require 'url)
(require 'json-reformat)

(defcustom restclient-same-buffer-response t
  "Re-use same buffer for responses or create a new one each time")

(defcustom restclient-same-buffer-response-name "*HTTP Response*"
  "Name for response buffer")

(defvar restclient-within-call nil)

(defvar restclient-request-time-start nil)
(defvar restclient-request-time-end nil)

;; The following disables the interactive request for user name and
;; password should an API call encounter a permission-denied response.
;; This API is meant to be usable without constant asking for username
;; and password.
(defadvice url-http-handle-authentication (around restclient-fix)
  (if restclient-within-call
      (setq success t)
    ad-do-it)
  (setq restclient-within-call nil))
(ad-activate 'url-http-handle-authentication)

(defadvice url-cache-extract (around restclient-fix-2)
  (if restclient-within-call
      (setq success t)
    ad-do-it)
  (setq restclient-within-call nil))
(ad-activate 'url-cache-extract)

(defun restclient-restore-header-variables ()
  (url-set-mime-charset-string)
  (setq url-mime-language-string nil)
  (setq url-mime-encoding-string nil)
  (setq url-mime-accept-string nil)
  (setq url-personal-mail-address nil))

(defun restclient-http-do (method url headers entity raw)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method method)
        (url-request-extra-headers '())
        (url-request-data entity))

    (restclient-restore-header-variables)
    
    (dolist (header headers)
      (let* ((mapped (assoc-string (downcase (car header))
                                   '(("from" . url-personal-mail-address)
                                     ("accept-encoding" . url-mime-encoding-string)
                                     ("accept-charset" . url-mime-charset-string)
                                     ("accept-language" . url-mime-language-string)
                                     ("accept" . url-mime-accept-string)))))
        
        (if mapped
            (set (cdr mapped) (cdr header))
          (setq url-request-extra-headers (cons header url-request-extra-headers)))
        ))

    (setq restclient-within-call t)
    (setq restclient-request-time-start (current-time))
    (url-retrieve url 'restclient-http-handle-response
                  (list (if restclient-same-buffer-response
                            restclient-same-buffer-response-name
                          (format "*HTTP %s %s*" method url)) raw))))


(defun restclient-prettify-response ()
  (save-excursion
    (let ((start (point)) (guessed-mode))
      (while (not (looking-at "^\\s-*$"))
        (when (looking-at "^Content-[Tt]ype: \\([^; \n]+\\).*$")
          (setq guessed-mode
                (cdr (assoc-string
                      (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                      '(("text/xml" . xml-mode)
                        ("application/xml" . xml-mode)
                        ("application/atom+xml" . xml-mode)
                        ("application/atomcat+xml" . xml-mode)
                        ("application/json" . js-mode)
                        ("image/png" . image-mode)
                        ("image/jpeg" . image-mode)
                        ("image/gif" . image-mode)
                        ("text/html" . html-mode))))))
        (forward-line))
      (let ((headers (buffer-substring-no-properties start (point))))
        (forward-line)
        (when guessed-mode
          (delete-region start (point))
          (unless (eq guessed-mode 'image-mode)
            (apply guessed-mode '())
            (font-lock-fontify-buffer))

          (cond
           ((eq guessed-mode 'xml-mode)
            (goto-char (point-min))
            (while (search-forward-regexp "\>[ \\t]*\<" nil t)
              (backward-char) (insert "\n"))
            (indent-region (point-min) (point-max)))

           ((eq guessed-mode 'image-mode)
            (let* ((img (buffer-string)))
              (delete-region (point-min) (point-max))
              (fundamental-mode)
              (insert-image (create-image img nil t))

              ))

           ((eq guessed-mode 'js-mode)
            (json-reformat-region (point-min) (point-max))))

          (goto-char (point-max))
          (let ((hstart (point)))
            (insert "\n" headers)
            (insert (format "Request duration: %fs\n" (float-time (time-subtract restclient-request-time-end restclient-request-time-start))))
            (unless (eq guessed-mode 'image-mode)
              (comment-region hstart (point))
              (indent-region hstart (point)))))))))

(defun restclient-http-handle-response (status bufname raw)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (setq restclient-request-time-end (current-time))
  (restclient-restore-header-variables)
  (if restclient-same-buffer-response
      (if (get-buffer restclient-same-buffer-response-name)
	  (kill-buffer restclient-same-buffer-response-name)))
  (restclient-decode-response (current-buffer) bufname)
  (unless raw
    (restclient-prettify-response))
  (buffer-enable-undo))

(defun restclient-decode-response (raw-http-response-buffer target-buffer-name)
  "Decode the HTTP response using the charset (encoding) specified in the
   Content-Type header. If no charset is specified, default to UTF-8."
  (let* ((charset-regexp "Content-Type.*charset=\\([-A-Za-z0-9]+\\)")
         (image? (save-excursion
                   (search-forward-regexp "Content-Type.*[Ii]mage" nil t)))
	 (encoding (if (save-excursion
			 (search-forward-regexp charset-regexp nil t))
		       (intern (downcase (match-string 1)))
		     'utf-8)))
    (if image?
        ;; Dont' attempt to decode. Instead, just switch to the raw HTTP response buffer and
        ;; rename it to target-buffer-name.
        (progn
          (switch-to-buffer-other-window raw-http-response-buffer)
          (rename-buffer target-buffer-name))
      ;; Else, switch to the new, empty buffer that will contain the decoded HTTP
      ;; response. Set its encoding, copy the content from the unencoded
      ;; HTTP response buffer and decode.
      (let ((decoded-http-response-buffer (get-buffer-create
                                           (generate-new-buffer-name target-buffer-name))))
        (switch-to-buffer-other-window decoded-http-response-buffer)
        (setq buffer-file-coding-system encoding)
        (save-excursion
          (insert-buffer-substring raw-http-response-buffer))
        (kill-buffer raw-http-response-buffer)
        (condition-case nil
            (decode-coding-region (point-min) (point-max) encoding)
          (error
           (message (concat "Error when trying to decode http response with encoding: "
                            (symbol-name encoding)))))))))

(defconst restclient-method-url-regexp
  "^\\(GET\\|POST\\|DELETE\\|PUT\\|HEAD\\|OPTIONS\\|PATCH\\) \\(.*\\)$")

(defconst restclient-header-regexp
  "^\\([^ :]+\\): \\(.*\\)$")

(defconst restclient-var-regexp
  "^\\(:[^: ]+\\) = \\(.+\\)$")

(defun restclient-current-min ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^#")
        (if (re-search-forward "^[^#]" (point-max) t)
            (point-at-bol))
      (if (re-search-backward "^#" (point-min) t)
          (point-at-bol 2)
        (point-min)))))

(defun restclient-current-max ()
  (save-excursion
    (if (re-search-forward "^#" (point-max) t)
        (point-at-bol)
      (point-max))))

(defun restclient-replace-all-in-string (replacements s)
  (if replacements 
      (replace-regexp-in-string (regexp-opt (mapcar 'car replacements))
                                (lambda (key) (cdr (assoc key replacements)))
                                s)
    s))

(defun restclient-replace-all-in-header (replacements header)
  (cons (car header)
        (restclient-replace-all-in-string replacements (cdr header))))

(defun restclient-replace-all-in-headers (replacements headers)
  (mapcar (apply-partially 'restclient-replace-all-in-header vars) headers))

(defun restclient-find-vars-before-point ()
  (let ((vars nil)
        (bound (point)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp restclient-var-regexp bound t)
        (let ((name (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
              (value (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
          (setq vars (cons (cons name value) vars))))
      vars)))

;;;###autoload
(defun restclient-http-send-current (&optional raw)
  (interactive)
  (save-excursion
    (goto-char (restclient-current-min))
    (when (re-search-forward restclient-method-url-regexp (point-max) t)
      (let ((method (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
            (url (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
            (headers '()))
        (forward-line)
        (while (re-search-forward restclient-header-regexp (point-at-eol) t)
          (setq headers (cons (cons (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                                    (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
                              headers))
          (forward-line))
        (when (looking-at "^\\s-*$")
          (forward-line))
        (let* ((entity (buffer-substring (point) (restclient-current-max)))
               (vars (restclient-find-vars-before-point))
               (url (restclient-replace-all-in-string vars url))
               (headers (restclient-replace-all-in-headers vars headers))
               (entity (restclient-replace-all-in-string vars entity)))
          (message "HTTP %s %s Headers:[%s] Body:[%s]" method url headers entity)
          (restclient-http-do method url headers entity raw))))))

;;;###autoload
(defun restclient-http-send-current-raw ()
  (interactive)
  (restclient-http-send-current t))

(setq restclient-mode-keywords
      (list (list restclient-method-url-regexp '(1 font-lock-keyword-face) '(2 font-lock-function-name-face))
            (list restclient-header-regexp '(1 font-lock-variable-name-face) '(2 font-lock-string-face))
            (list restclient-var-regexp '(1 font-lock-variable-name-face) '(2 font-lock-string-face))
            ))

(defvar restclient-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

;;;###autoload
(define-derived-mode restclient-mode fundamental-mode "REST Client"

  (local-set-key "\C-c\C-c" 'restclient-http-send-current)
  (local-set-key "\C-c\C-r" 'restclient-http-send-current-raw)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#\\W*")
  (set (make-local-variable 'comment-column) 48)

  (set (make-local-variable 'font-lock-defaults) '(restclient-mode-keywords)))


(provide 'restclient)
;;; restclient.el ends here
