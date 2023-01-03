;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/r.el\")"; -*-
(gallagher-eval-buffer "g-utils.el")
(gallagher-eval-buffer "porter.el")

;; (MM (g--driver-string 3541 (find-file-noselect "unconverted-driver-but-with-functor.cpp")))

;; ;; TEST-1 GENERATE FUNCTOR CALL FROM KERNEL INVOKE
;; (with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
;;   (goto-char 3541) ;; expect the point to be on the kernel invocation

(defun g--driver-string-replace-ndrange (string)
  (replace-regexp-in-string
   "NDRange +\\([^ ]+\\) *(\\([^,]+\\),\\([^,]+\\),\\([^,]+\\))[;]*;"
   "auto \\1 = sycl::range(\\2, \\3);"
   string t)
  )

(with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
  (goto-char 3541)
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (MM
       (g--driver-string-first-filter (buffer-string))
       ;; (g--driver-string-replace-ndrange (buffer-string))
       )
      )
    )
  )
