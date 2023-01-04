;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/r.el\")"; -*-
(gallagher-eval-buffer "g-utils.el")
(gallagher-eval-buffer "porter.el")

;; (MM (g--driver-string 3541 (find-file-noselect "unconverted-driver-but-with-functor.cpp")))

;; ;; TEST-1 GENERATE FUNCTOR CALL FROM KERNEL INVOKE
;; (with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
;;   (goto-char 3541) ;; expect the point to be on the kernel invocation


(with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
  (goto-char 3541)

  ;; has to have entire buffer
  ;; (g--functor-dispatch (point) (current-buffer)))

  (let ((driver-string
         (buffer-substring-no-properties
          (save-excursion (beginning-of-defun) (point))
          (save-excursion (end-of-defun) (point))))
        )
    (MM
     (replace-regexp-in-string
      "[^;]*EnqueueArgs([^;]*;"
      (concat
       "\n\n"
       (g--functor-dispatch (point) (current-buffer))
       "\n"
       )
      (g--driver-string-first-filter driver-string))

     )
    )
  )

       ;; (let* ((first-filtered (g--driver-string-first-filter (buffer-string)))
       ;;        (dispatch (g--functor-dispatch (point) (current-buffer))))

;;   ;; first-filtered
       ;;   )
