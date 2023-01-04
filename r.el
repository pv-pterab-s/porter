;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/r.el\")"; -*-
(gallagher-eval-buffer "g-utils.el")
(gallagher-eval-buffer "porter.el")


(defun g--driver-point-on-invoke (point-in-opencl-driver buffer)
  (interactive (list (point) (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (let ((end (save-excursion (end-of-defun) (point))))
        (beginning-of-defun)
        (re-search-forward "EnqueueArgs(.*getQueue[^;]*;")
        (match-beginning 0)
        )))
  )

(defun g--driver (point-in-opencl-driver buffer)
  (interactive (list (point) (current-buffer)))
  (let ((driver-string
         (buffer-substring-no-properties
          (save-excursion (beginning-of-defun) (point))
          (save-excursion (end-of-defun) (point)))))
    (replace-regexp-in-string
     "[^;]*EnqueueArgs([^;]*;"
     (concat
      "\n\n"    ;; vvv cursor _must_ be on the opencl kernel dispatch
      (g--functor-dispatch (g--driver-point-on-invoke
                            point-in-opencl-driver buffer))
      "\n"
      )
     (g--driver-string-first-filter driver-string))
    )
  )


(with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
  (let (pt-in-opencl-driver)
    (setq pt-in-opencl-driver   ;; simulating user
          (with-current-buffer (find-file-noselect g--oneapi-driver-fn)
            (goto-char (point-min))
            (re-search-forward "void +tile")
            (forward-line 3) (point)))

    (M (g--driver-string (point) (current-buffer)))

    )
  )
