;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/test.el\")"; -*-
(defun M (s) (message "%s" s) s)
(defun MM (string)
  (with-current-buffer (find-file-noselect "/tmp/tmp-work-output")
    (erase-buffer)
    (insert (format "%s" string))
    (display-buffer (current-buffer))
    (set-window-point
     (get-buffer-window (current-buffer) 'visible)
     (point-min))
    string))

(defun g--sort-strings-by-length (list-of-strings)
  (seq-sort #'(lambda (str-a str-b) (< (length str-a)) (length str-b))
            list-of-strings))
(defun g--zip (list-a list-b)
  (mapcar* 'cons list-a list-b))




(defun g--replace-list-of-pairs (string &optional pairs)
  (if (not pairs)
      string
    (let* ((pair (car pairs))
           (regexp (car pair))
           (replacement (cdr pair)))
      (g--replace-list-of-pairs-in-string-helper
       (replace-regexp-in-string regexp replacement string t)
       (cdr pairs)))))



(defun g--split-params-string (string)
  (with-temp-buffer
    (c++-mode)
    (insert string)
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
        )
      (push
       (buffer-substring-no-properties start-pt (point-max))
       output-list)
      (mapcar #'string-trim (nreverse output-list))
      )
    )
  )


(defun g--c-defun-params ()
  (save-excursion
    (beginning-of-defun)
    (let (beg-params end-params)
      (re-search-forward "(") (backward-char)
      (setq beg-params (point))
      (forward-sexp)
      (setq end-params (point))

      (g--split-params-string  ;; more robust params list split
       (g--replace-list-of-pairs
        (g--replace-list-of-pairs (buffer-substring-no-properties beg-params end-params)
                                  '(("\n" . "")
                                    (" +" . " ")))
        '(("^ *(" . "")
          (") *$" . ""))))
       )
      )
    )





      ;; (M (g--clean-string (buffer-substring-no-properties beg-params end-params)))

      ;; (M

      ;;   ))

      ;; (M "MADE IT HERE ^^")

      ;; (g--split-params-string
      ;;  (g--replace-list-of-pairs
      ;;   (buffer-substring-no-properties beg-params end-params)
      ;;   '(("\n" . "")
      ;;     (" +" . " "))
      ;;   ))


  ;;     (setq cleaned-args-string
  ;;           (g--replace-list-of-pairs
  ;;            (buffer-substring-no-properties beg-params end-params)
  ;;            '(("^ *(" . "")
  ;;              (")[^);]*; *$" . "")
  ;;              ("\n" . "")
  ;;              (" +" . " "))))

  ;;     (M "!!!!!!!!!")
  ;;     (M cleaned-args-string)

  ;;     (g--split-params-string cleaned-args-string)

  ;;     ;; (M (g--replace-list-of-pairs
  ;;     ;;  (buffer-substring-no-properties beg-params end-params)
  ;;     ;;  '(("^ *(" . "")
  ;;     ;;    (") *$" . ""))))
  ;;     )
  ;;   ;; (re-search-forward "(\\([^()]+\\))[^)]*{")
  ;;   ;; (re-search-forward "(\\([^()]+\\))  [^)]*{")

  ;;   )
  ;; )


(defun g--c-defun-params-names ()
  (mapcar #'(lambda (param)
              (string-match "[^* ]+[ ]*$" param)
              (match-string 0 param))
          (g--c-defun-params)))


(defun g--c-defun-params-types ()
  (mapcar #'(lambda (param)
              (string-match "\\(.*[* ]+\\)\\([^* ]+[ ]*\\)$" param)
              (string-trim (match-string 1 param)))
          (g--c-defun-params)))


(defun g--c-defun-body ()
  (save-excursion
    (beginning-of-defun) (re-search-forward "{")
    (buffer-substring-no-properties (point)
                                    (progn (end-of-defun) (point)))))


(defun g--c-invocation-params-string (point-at-invocation buffer)
  (interactive (list (point) (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char point-at-invocation)
      (g--replace-list-of-pairs
       (buffer-substring-no-properties
        (save-excursion (re-search-forward ";") (point))
        (save-excursion
          (re-search-forward ";")
          (re-search-backward ")")
          (forward-char)
          (backward-sexp)
          (point)
          ))
       '(("^ *(" . "")
         (")[^);]*; *$" . "")
         ("\n" . "")
         (" +" . " ")))
      )
    )
  )

(defun g--c-invocation-params (point-at-invocation buffer)
  (interactive (list (point) (current-buffer)))
  (g--split-params-string (g--c-invocation-params-string point-at-invocation buffer)))



;; (defun g--cc-class ()
;;   (let (begin-of-class end-of-class)
;;     (save-excursion  ;; expects to be within a class.
;;       (re-search-backward "class")
;;       (setq begin-of-class (point))
;;       (re-search-forward "{") (backward-char) (forward-sexp)
;;       (setq end-of-class (point))
;;       (buffer-substring-no-properties begin-of-class end-of-class))
;;     ))

;; (defun g--cc-class-name ()
;;   (let ((class (g--cc-class)))
;;     (string-match " +\\([_A-Za-z][_A-Za-z0-9]*\\) +" class)
;;     (match-string 1 class)))

;; (defun g--cc-constructor-params ()
;;   (let ((class (g--cc-class)))  ;; expect to run within class
;;     (with-temp-buffer

;;       (insert class)
;;       (re-search-backward "}")  ;; should be end of class
;;       (re-search-backward (concat "[ \\n]+" (g--cc-class-name)))  ;; should be constructor
;;       (let (begin-args end-args args-string)
;;         (re-search-forward "(") (backward-char)
;;         (setq begin-args (point))
;;         (forward-sexp)
;;         (setq end-args (point))

;;         ;; strip parens
;;         (g--split-params-string
;;          (buffer-substring-no-properties
;;           begin-args
;;           end-args))
;;         )
;;       )
;;     )
;;   )


;; (defun get-cpp-class-names ()
;;   "Return a list of all C++ class names in the current buffer."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let (class-names)
;;       (while (re-search-forward "^\\s-*class\\s-+\\([[:alnum:]]+\\)" nil t)
;;         (push (match-string 1) class-names))
;;       class-names)))

;; (defun get-cpp-constructor (class-name)
;;   "Return a constructor for the given C++ CLASS-NAME from the current buffer."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((regexp (format "\\s*-*%s\\s-*(\\s-*)" class-name)))
;;       (M regexp)
;;       (if (re-search-forward regexp nil t)
;;           (buffer-substring-no-properties
;;            (match-beginning 0) (line-end-position))
;;         (error "Constructor for class '%s' not found" class-name)))))


(defun g--cc-class ()
  (let (begin-of-class end-of-class)
    (save-excursion  ;; expects to be within a class.
      (cl-assert (looking-at "class"))
      (setq begin-of-class (point))
      (re-search-forward "{") (backward-char) (forward-sexp)
      (setq end-of-class (point))
      (buffer-substring-no-properties begin-of-class end-of-class))
    ))

(defun g--functor-constructor-params (buffer)   ;; assume only on of these in the buffer
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "class")  ;; find first class
      ;; (re-search-forward "{") (backward-char) (forward-sexp)
      (re-search-forward ":[^{]*{ *}")   ;; constructor has empty body
      (g--c-defun-params)
      )
    )
  )


(defun g--first-match (regexp string)
  (string-match regexp string)
  (match-string 1 string))


(provide 'g-utils)
