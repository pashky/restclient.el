;;; restclient.el --- An interactive HTTP client for Emacs
;;
;; Public domain.

;; Author: Pavel Kurnosov <pashky@gmail.com>
;; Maintainer: Pavel Kurnosov <pashky@gmail.com>
;; Created: 01 Apr 2012
;; Keywords: http

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a tool to manually explore and test HTTP REST
;; webservices.  Runs queries from a plain-text query sheet, displays
;; results as a pretty-printed XML, JSON and even images.

;;; Code:
;;
(require 'url)
(require 'json)

(defgroup restclient nil
  "An interactive HTTP client for Emacs."
  :group 'tools)

(defcustom restclient-log-request t
  "Log restclient requests to *Messages*."
  :group 'restclient
  :type 'boolean)

(defcustom restclient-same-buffer-response t
  "Re-use same buffer for responses or create a new one each time."
  :group 'restclient
  :type 'boolean)

(defcustom restclient-same-buffer-response-name "*HTTP Response*"
  "Name for response buffer."
  :group 'restclient
  :type 'string)

(defcustom restclient-inhibit-cookies nil
  "Inhibit restclient from sending cookies implicitly."
  :group 'restclient
  :type 'boolean)

(defvar restclient-within-call nil)

(defvar restclient-request-time-start nil)
(defvar restclient-request-time-end nil)

(defvar restclient-response-loaded-hook nil
  "Hook run after response buffer created and data loaded.")

(defvar restclient-http-do-hook nil
  "Hook to run before making request.")

(defvar restclient-overlay nil
  "Overlay used to highlight the current request.")

;; The following disables the interactive request for user name and
;; password should an API call encounter a permission-denied response.
;; This API is meant to be usable without constant asking for username
;; and password.
(defadvice url-http-handle-authentication (around restclient-fix)
  (if restclient-within-call
      (setq success t ad-return-value t)
    ad-do-it))
(ad-activate 'url-http-handle-authentication)

(defadvice url-cache-extract (around restclient-fix-2)
  (if restclient-within-call
      (setq success t)
    ad-do-it))
(ad-activate 'url-cache-extract)

(defadvice url-http-user-agent-string (around restclient-fix-3)
  (if restclient-within-call
      (setq ad-return-value nil)
    ad-do-it))
(ad-activate 'url-http-user-agent-string)

(defun restclient-restore-header-variables ()
  (url-set-mime-charset-string)
  (setq url-mime-language-string nil)
  (setq url-mime-encoding-string nil)
  (setq url-mime-accept-string nil)
  (setq url-personal-mail-address nil))

(defun restclient-http-do (method url headers entity &rest handle-args)
  "Send ENTITY and HEADERS to URL as a METHOD request."
  (if restclient-log-request
      (message "HTTP %s %s Headers:[%s] Body:[%s]" method url headers entity))
  (let ((url-request-method method)
        (url-request-extra-headers '())
        (url-request-data (encode-coding-string entity 'utf-8)))

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
    (run-hooks 'restclient-http-do-hook)
    (url-retrieve url 'restclient-http-handle-response
                  (append (list method url (if restclient-same-buffer-response
                            restclient-same-buffer-response-name
                          (format "*HTTP %s %s*" method url))) handle-args) nil restclient-inhibit-cookies)))

(defvar restclient-content-type-regexp "^Content-[Tt]ype: \\(\\w+\\)/\\(?:[^\\+\r\n]*\\+\\)*\\([^;\r\n]+\\)")

(defun restclient-prettify-response (method url)
  (save-excursion
    (let ((start (point)) (guessed-mode) (end-of-headers))
      (while (and (not (looking-at "^\\s-*$"))
                  (eq (progn
                        (when (looking-at restclient-content-type-regexp)
                          (setq guessed-mode
                                (cdr (assoc-string (concat
                                                    (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                                                    "/"
                                                    (buffer-substring-no-properties (match-beginning 2) (match-end 2))
                                                    )
                                                   '(("text/xml" . xml-mode)
                                                     ("application/xml" . xml-mode)
                                                     ("application/json" . js-mode)
                                                     ("image/png" . image-mode)
                                                     ("image/jpeg" . image-mode)
                                                     ("image/jpg" . image-mode)
                                                     ("image/gif" . image-mode)
                        ("text/html" . html-mode))))))
                        (forward-line)) 0)))
      (setq end-of-headers (point))
      (while (and (looking-at "^\\s-*$")
                  (eq (forward-line) 0)))
      (unless guessed-mode
        (setq guessed-mode
              (or (assoc-default nil
                                 ;; magic mode matches
                                 '(("<\\?xml " . xml-mode)
                                   ("{\\s-*\"" . js-mode))
                                 (lambda (re _dummy)
                                   (looking-at re))) 'js-mode)))
      (let ((headers (buffer-substring-no-properties start end-of-headers)))
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
              (insert-image (create-image img nil t))))

           ((eq guessed-mode 'js-mode)
            (let ((json-special-chars (remq (assoc ?/ json-special-chars) json-special-chars)))
              (ignore-errors (json-pretty-print-buffer)))
            (restclient-prettify-json-unicode)))

          (goto-char (point-max))
          (or (eq (point) (point-min)) (insert "\n"))
          (let ((hstart (point)))
            (insert method " " url "\n" headers)
            (insert (format "Request duration: %fs\n" (float-time (time-subtract restclient-request-time-end restclient-request-time-start))))
            (unless (eq guessed-mode 'image-mode)
              (comment-region hstart (point))
              (indent-region hstart (point)))))))))

(defun restclient-prettify-json-unicode ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\[Uu]\\([0-9a-fA-F]\\{4\\}\\)" nil t)
      (replace-match (char-to-string (decode-char 'ucs (string-to-number (match-string 1) 16))) t nil))))

(defun restclient-http-handle-response (status method url bufname raw stay-in-window)
  "Switch to the buffer returned by `url-retreive'.
The buffer contains the raw HTTP response sent by the server."
  (setq restclient-within-call nil)
  (setq restclient-request-time-end (current-time))
  (if (= (point-min) (point-max))
      (signal (car (plist-get status :error)) (cdr (plist-get status :error)))
    (restclient-restore-header-variables)
    (when (buffer-live-p (current-buffer))
      (with-current-buffer (restclient-decode-response
                            (current-buffer)
                            bufname
                            restclient-same-buffer-response)
        (unless raw
          (restclient-prettify-response method url))
        (buffer-enable-undo)
        (run-hooks 'restclient-response-loaded-hook)
        (if stay-in-window
            (display-buffer (current-buffer) t)
          (switch-to-buffer-other-window (current-buffer)))))))

(defun restclient-decode-response (raw-http-response-buffer target-buffer-name same-name)
  "Decode the HTTP response using the charset (encoding) specified in the Content-Type header. If no charset is specified, default to UTF-8."
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
        (with-current-buffer raw-http-response-buffer
          (rename-buffer target-buffer-name)
          raw-http-response-buffer)
      ;; Else, switch to the new, empty buffer that will contain the decoded HTTP
      ;; response. Set its encoding, copy the content from the unencoded
      ;; HTTP response buffer and decode.
      (let ((decoded-http-response-buffer
             (get-buffer-create
              (if same-name target-buffer-name (generate-new-buffer-name target-buffer-name)))))
        (with-current-buffer decoded-http-response-buffer
          (setq buffer-file-coding-system encoding)
          (save-excursion
            (erase-buffer)
            (insert-buffer-substring raw-http-response-buffer))
          (kill-buffer raw-http-response-buffer)
          (condition-case nil
              (decode-coding-region (point-min) (point-max) encoding)
            (error
             (message (concat "Error when trying to decode http response with encoding: "
                              (symbol-name encoding)))))
          decoded-http-response-buffer)))))

(defconst restclient-method-url-regexp
  "\\(GET\\|POST\\|DELETE\\|PUT\\|HEAD\\|OPTIONS\\|PATCH\\) \\(.*\\)$")

(defconst restclient-header-regexp
  "\\([^ :]+\\): \\(.*\\)$")

(defconst restclient-var-regexp
  "\\(:[^: ]+\\)\\s-+\\(:?\\)=\\s-+\\(.+\\)$")

(defconst restclient-evar-regexp
  "\\(:[^: ]+\\)\\s-+:=\\s-+\\(.+\\)$")

(defvar restclient-common-prefix "^"
  "Prefix regexp used to identify `restclient' lines of code when used with another major mode.")

(put 'restclient-common-prefix 'safe-local-variable #'stringp)

(defun restclient-method-url-regexp()
  (concat restclient-common-prefix restclient-method-url-regexp))

(defun restclient-header-regexp()
  (concat restclient-common-prefix restclient-header-regexp))

(defun restclient-var-regexp()
  (concat restclient-common-prefix restclient-var-regexp))

(defun restclient-evar-regexp()
  (concat restclient-common-prefix restclient-evar-regexp))

(defun restclient-current-min ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at (concat restclient-common-prefix "#"))
        (if (re-search-forward (concat restclient-common-prefix "[^#]*?$") (point-max) t)
            (point-at-bol))
      (if (re-search-backward (concat restclient-common-prefix "#.*?$") (point-min) t)
          (point-at-bol 2)
        (if (eq restclient-common-prefix "^")
            (point-min)
          (progn
            (goto-char (point-min))
            (if (re-search-forward (concat restclient-common-prefix "#.*?\n") (point-max) t)
                (if (re-search-forward (concat restclient-common-prefix "[^#]*?$") (point-max) t)
                    (point-at-bol)
                  (error "Could not find current request"))
                ;;(progn
                ;;  (while (looking-at (concat restclient-common-prefix "#")) (forward-line 1))
                ;;  (point-at-bol))
              (error "Could not find current request"))))))))

(defun restclient-current-max ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at (concat restclient-common-prefix "#"))
        (forward-line 1))
    (if (re-search-forward (concat restclient-common-prefix "#.*?$") (point-max) t)
        (progn
          (re-search-backward (concat restclient-common-prefix "[^#]*?\n") (point-min) t)
          (point-at-bol 2))
      (progn
        (goto-char (point-max))
        (re-search-backward (concat restclient-common-prefix ".*$") (point-min) t)
        (point-at-bol 2))
      )))

(defun restclient-replace-all-in-string (replacements s)
  (if replacements
      (replace-regexp-in-string (regexp-opt (mapcar 'car replacements))
                                (lambda (key) (cdr (assoc key replacements)))
                                s nil t)
    s))

(defun restclient-replace-all-in-header (replacements header)
  (cons (car header)
        (restclient-replace-all-in-string replacements (cdr header))))

(defun restclient-replace-all-in-headers (replacements headers)
  (mapcar (apply-partially 'restclient-replace-all-in-header replacements) headers))

(defun restclient-find-vars-before-point ()
  (let ((vars nil)
        (bound (point)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp (restclient-var-regexp) bound t)
        (let ((name (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
              (should-eval (> (length (match-string 2)) 0))
              (value (buffer-substring-no-properties (match-beginning 3) (match-end 3))))
          (setq vars (cons (cons name (if should-eval (restclient-eval-var value) value)) vars))))
      vars)))

(defun restclient-eval-var (string)
  (with-output-to-string (princ (eval (read string)))))

(defun restclient-http-parse-current-and-do (func &rest args)
  (save-excursion
    (goto-char (restclient-current-min))
    (when (re-search-forward (restclient-method-url-regexp) (point-max) t)
      (let ((method (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
            (url (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
            (headers '()))
        (forward-line)
        (while (re-search-forward (restclient-header-regexp) (point-at-eol) t)
          (setq headers (cons (cons (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                                    (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
                              headers))
          (forward-line))
        (when (looking-at "^\\s-*$")
          (forward-line))
        (let* ((cmax (restclient-current-max))
               (entity (buffer-substring (min (point) cmax) cmax))
               (vars (restclient-find-vars-before-point))
               (url (restclient-replace-all-in-string vars url))
               (headers (restclient-replace-all-in-headers vars headers))
               (entity (restclient-replace-all-in-string vars entity)))
          (apply func method url headers entity args))))))

(defun restclient-copy-curl-command ()
  "Formats the request as a curl command and copies the command to the clipboard."
  (interactive)
  (restclient-http-parse-current-and-do
   '(lambda (method url headers entity)
      (kill-new (format "curl -i %s -X%s '%s' %s"
                        (mapconcat (lambda (header) (format "-H '%s: %s'" (car header) (cdr header))) headers " ")
                        method url
                        (if (> (string-width entity) 0)
                            (format "-d '%s'" entity) "")))
      (message "curl command copied to clipboard."))))

;;;###autoload
(defun restclient-http-send-current (&optional raw stay-in-window)
  "Sends current request.
Optional argument RAW don't reformat response if t.
Optional argument STAY-IN-WINDOW do not move focus to response buffer if t."
  (interactive)
  (restclient-http-parse-current-and-do 'restclient-http-do raw stay-in-window))

;;;###autoload
(defun restclient-http-send-current-raw ()
  "Sends current request and get raw result (no reformatting or syntax highlight of XML, JSON or images)."
  (interactive)
  (restclient-http-send-current t))

;;;###autoload
(defun restclient-http-send-current-stay-in-window ()
  "Send current request and keep focus in request window."
  (interactive)
  (restclient-http-send-current nil t))

(defun restclient-jump-next ()
  "Jump to next request in buffer."
  (interactive)
  (if (re-search-forward (concat restclient-common-prefix "#.*?\n") (point-max) t)
      (let ((curr-min (restclient-current-min)))
        (goto-char curr-min)
        (restclient-highlight-selection curr-min (restclient-current-max)))
    (message "No more requests found.")))

(defun restclient-jump-prev ()
  "Jump to previous request in buffer."
  (interactive)
  (let* ((curr-min (restclient-current-min))
         (prev-min (save-excursion
                     (goto-char curr-min)
                     (forward-line -1)
                     (while (and (looking-at (concat restclient-common-prefix "#"))
                                 (not (bobp)))
                       (forward-line -1))
                     (restclient-current-min))))
    (if (eq curr-min prev-min)
        (message "No previous request found.")
      (progn
        (goto-char prev-min)
        (restclient-highlight-selection prev-min (restclient-current-max))))))

(defun restclient-mark-current ()
  "Mark current request."
  (interactive)
  (goto-char (restclient-current-min))
  (restclient-highlight-selection
   (restclient-current-min)
   (restclient-current-max)))

(defun restclient-highlight-selection (begin end &optional face)
  "Highlight selection between BEGIN and END.
Optional FACE can be specified, otherwise `highlight' is
used by default."
  (let ((hiface (or face 'highlight)))
    (setq restclient-overlay (make-overlay begin end))
    (overlay-put restclient-overlay 'face hiface)
    (add-hook 'pre-command-hook 'restclient-remove-overlay)))

(defun restclient-remove-overlay ()
  "Remove overlay set by `restclient-highlight-selection'."
  (delete-overlay restclient-overlay)
  (setq restclient-overlay nil)
  (remove-hook 'pre-command-hook 'restclient-remove-overlay))


(defvar restclient-mode-keywords
  (list (list (restclient-method-url-regexp) '(1 font-lock-keyword-face) '(2 font-lock-function-name-face))
        (list (restclient-header-regexp) '(1 font-lock-variable-name-face) '(2 font-lock-string-face))
        (list (restclient-evar-regexp) '(1 font-lock-preprocessor-face) '(2 font-lock-function-name-face))
        (list (restclient-var-regexp) '(1 font-lock-preprocessor-face) '(3 font-lock-string-face))
        ))

(defvar restclient-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

;;;###autoload
(define-derived-mode restclient-mode fundamental-mode "REST Client"
  "Turn on restclient mode."
  (local-set-key (kbd "C-c C-c") 'restclient-http-send-current)
  (local-set-key (kbd "C-c C-r") 'restclient-http-send-current-raw)
  (local-set-key (kbd "C-c C-v") 'restclient-http-send-current-stay-in-window)
  (local-set-key (kbd "C-c C-n") 'restclient-jump-next)
  (local-set-key (kbd "C-c C-p") 'restclient-jump-prev)
  (local-set-key (kbd "C-c C-.") 'restclient-mark-current)
  (local-set-key (kbd "C-c C-u") 'restclient-copy-curl-command)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "# *")
  (set (make-local-variable 'comment-column) 48)

  (set (make-local-variable 'font-lock-defaults) '(restclient-mode-keywords)))


(provide 'restclient)
;;; restclient.el ends here
