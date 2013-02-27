;; JSON reformat snippet
;; from https://gist.github.com/1789605

(require 'json)
(require 'cl)

(defun json-reformat:indent (level)
  (make-string (* level 4) ? ))

(defun json-reformat:p-of-number (val)
  (number-to-string val))

(defun json-reformat:p-of-list (val level)
  (concat "{\n" (json:list-to-string val (1+ level)) (json-reformat:indent level) "}"))

(defun json-reformat:p-of-vector (val level)
  (if (= (length val) 0) "[]"
    (concat "[\n"
            (mapconcat
             'identity
             (loop for v across val
                   collect (concat
                            (json-reformat:indent (1+ level))
                            (json-reformat:print-value v (1+ level))
                            ))
             (concat ",\n"))
            "\n" (json-reformat:indent level) "]"
            )))

(defun json-reformat:p-of-symbol (val)
  (cond ((equal 't val) "true")
        ((equal json-false val) "false")
        (t (symbol-name val))))

(defun json-reformat:print-value (val level)
  (cond ((consp val) (json-reformat:p-of-list val level))
        ((numberp val) (json-reformat:p-of-number val))
        ((vectorp val) (json-reformat:p-of-vector val level))
        ((null val) "null")
        ((symbolp val) (json-reformat:p-of-symbol val))
        (t (json-encode-string val))))

(defun json:list-to-string (root level)
  (let (key val str)
    (while root
      (setq key (car root)
            val (cadr root)
            root (cddr root))
      (setq str
            (concat str (json-reformat:indent level)
                    "\"" key "\""
                    ": "
                    (json-reformat:print-value val level)
                    (when root ",")
                    "\n"
                    )))
    str))

(defun json-reformat-region (begin end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (let* ((json-key-type 'string)
             (json-object-type 'plist)
             (before (buffer-substring (point-min) (point-max)))
             (json-tree (json-read-from-string before))
             after)
        (setq after (json-reformat:print-value json-tree 0))
        (delete-region (point-min) (point-max))
        (insert after)))))

(provide 'json-reformat)
