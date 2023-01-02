;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/r.el\")"; -*-
(gallagher-eval-buffer "g-utils.el")
(gallagher-eval-buffer "porter.el")

;; (MM (g--driver-string 3541 (find-file-noselect "unconverted-driver-but-with-functor.cpp")))

;; ;; TEST-1 GENERATE FUNCTOR CALL FROM KERNEL INVOKE
;; (with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
;;   (goto-char 3541) ;; expect the point to be on the kernel invocation
