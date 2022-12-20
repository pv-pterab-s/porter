;;; g-utils.el --- personal utility functions        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Gallagher Pryor

;; Author: Gallagher Pryor <gpryor@a770>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(defun M (s) (message "%s") s)

(defun g--replace-list-of-pairs-in-string (string pairs)
  (if (not pairs)
      string
    (let* ((pair (car pairs))
           (regexp (car pair))
           (replacement (cdr pair)))
      (g--replace-list-of-pairs-in-string
       (replace-regexp-in-string regexp replacement string)
       (cdr pairs)))))

;; dev-loop
(defun gdp--display-string-other-window (string)
  (with-current-buffer (find-file-noselect "/tmp/tmp-work-output")
    (erase-buffer)
    (insert (format "%s" string))
    (display-buffer (current-buffer))
    (set-window-point
     (get-buffer-window (current-buffer) 'visible)
     (point-min))))

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
      (nreverse output-list)
      ) ;; let
    )
  )

(defun g--c-defun-params ()
  (save-excursion
    (beginning-of-defun)
    (re-search-forward "(\\([^()]+\\))[^)]*{")
    (M (g--split-params-string (match-string 1)))
    )
  )

(provide 'g-utils)
