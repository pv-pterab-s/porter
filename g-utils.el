(defun M (s) (message "%s" s) s)

(defun g--replace-list-of-pairs-in-string (string pairs)
  (if (not pairs)
      string
    (let* ((pair (car pairs))
           (regexp (car pair))
           (replacement (cdr pair)))
      (g--replace-list-of-pairs-in-string-helper
       (replace-regexp-in-string regexp replacement string t)
       (cdr pairs)))))

;; (defun g--replace-list-of-pairs-in-string (string pairs)
;;   (g--replace-list-of-pairs-in-string-helper
;;    string
;;    (seq-sort #'(lambda (pair-a pair-b)
;;                  (< (length (car pair-a)) (length (car pair-b)))
;;                  )
;;              pairs)))


;; dev-loop
(defun gdp--display-string-other-window (string)
  (with-current-buffer (find-file-noselect "/tmp/tmp-work-output")
    (erase-buffer)
    (insert (format "%s" string))
    (display-buffer (current-buffer))
    (set-window-point
     (get-buffer-window (current-buffer) 'visible)
     (point-min))
    string))


(defun g--split-params-string (string)
  (with-temp-buffer
    (c++-mode)
    (insert (g--replace-list-of-pairs-in-string string
                                                '(("\n" . "")
                                                  (" +" . " ")
                                                  ("^[^(]*(" . "")
                                                  (")[^)]*$" . "")
                                                  )))
    (goto-char (point-min))
    (let ((start-pt 1)
          (output-list))
      (while (re-search-forward "[(,<]" (point-max) t)
        (cond ((string-equal (string (char-before)) "(")
               (backward-char) (forward-sexp))
              ((string-equal (string (char-before)) "<")
               (backward-char) (forward-sexp))
              ((string-equal (string (char-before)) ",")
               (push (buffer-substring-no-properties start-pt (1- (point))) output-list)
               (setq start-pt (point)))
              )
        )  ;; while
      (push (buffer-substring-no-properties start-pt (point-max)) output-list)
      (nreverse output-list)
      ) ;; let
    )
  )

(defun g--c-defun-params ()
  (save-excursion
    (beginning-of-defun)
    (re-search-forward "(\\([^()]+\\))[^)]*{")
    (g--split-params-string (match-string 1))
    )
  )

(provide 'g-utils)
