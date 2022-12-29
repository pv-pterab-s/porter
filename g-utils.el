;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/test.el\")"; -*-
(defun M (s) (message "%s" s) s)

(defun gdp--display-string-other-window (string)
  (with-current-buffer (find-file-noselect "/tmp/tmp-work-output")
    (erase-buffer)
    (insert (format "%s" string))
    (display-buffer (current-buffer))
    (set-window-point
     (get-buffer-window (current-buffer) 'visible)
     (point-min))
    string))
(defun MM (string) (gdp--display-string-other-window string))


(defun g--replace-list-of-pairs (string pairs)
  (if (not pairs)
      string
    (let* ((pair (car pairs))
           (regexp (car pair))
           (replacement (cdr pair)))
      (g--replace-list-of-pairs-in-string-helper
       (replace-regexp-in-string regexp replacement string t)
       (cdr pairs)))))


;; (defun g--replace-list-of-pairs (string)
;;   (M pairs)
;;   (if (not pairs) (cl-return string))
;;   (let* ((pair (car pairs))
;;          (regexp (car pair))
;;          (replacement (cadr pair))
;;          ;; (new-string (replace-regexp-in-string regexp replacement string t)))
;;          )
;;     (g--replace-list-of-pairs string (cdr pairs))))

;; (M "===========")
;; (g--replace-list-of-pairs "doo" '("doo" "111"))



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


(defun g--c-defun-params-names ()
  (mapcar #'(lambda (param)
              (string-match "[^* ]+[ ]*$" param)  ;; only parameter name. not type
              (match-string 0 param))
          (g--c-defun-params)))


(defun g--c-defun-params-types ()
  (mapcar #'(lambda (param)
              (string-match "\\(.*[* ]+\\)\\([^* ]+[ ]*\\)$" param)  ;; only parameter name. not type
              (match-string 1 param))
          (g--c-defun-params)))


(defun g--c-defun-body ()
  (save-excursion
    (beginning-of-defun) (re-search-forward "{")
    (buffer-substring-no-properties (point)
                                    (progn (end-of-defun) (point)))))


(defun g--sort-strings-by-length (list-of-strings)
  (seq-sort #'(lambda (str-a str-b) (< (length str-a)) (length str-b))
            list-of-strings))


(defun g--zip (list-a list-b)
  (mapcar* 'cons list-a list-b))


(provide 'g-utils)
