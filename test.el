;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/test.el\")"; -*-
(setq g--testing t)

(gallagher-eval-buffer "g-utils.el")
(gallagher-eval-buffer "porter.el")

;; (0) user harnesses arrayfire dir
(g-harness-arrayfire "~/tile-af")

;; reset the file system
(shell-command-to-string "cd ~/tile-af && git clean -dfx src && (yes | git checkout .)")
(shell-command-to-string "cd ~/tile-af && git checkout 23b7bb7f")

;; reset emacs buffers
(mapc #'(lambda (filename-symbol)
          (let* ((filename (eval filename-symbol))
                 (buffer (find-buffer-visiting filename)))
            (if (not buffer) return)
            (with-current-buffer buffer (revert-buffer t t t))
            ))
      '(g--opencl-kernel-fn g--opencl-driver-fn g--oneapi-driver-fn))


;; ;; TEST-0 EXTRACT C PARAMS FROM INVOCATIONS
;; (with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
;;   (goto-char 3541)
;;   (cl-assert
;;    (string-equal
;;     (g--c-invocation-params-string)
;;     "EnqueueArgs(getQueue(), global, local), *out.data, *in.data, out.info, in.info, blocksPerMatX, blocksPerMatY"))
;;   (cl-assert
;;    (equal
;;     (g--c-invocation-params)
;;     '("EnqueueArgs(getQueue(), global, local)" "*out.data" "*in.data" "out.info" "in.info" "blocksPerMatX" "blocksPerMatY")))
;;   )


;; UNFINISED!!!!!!!!
;; ;; TEST-1 GENERATE FUNCTOR CALL FROM KERNEL INVOKE
;; (with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
;;   ;; (goto-char 3541)   ;; assume point on kernel invoke

;;   ;; trying to get functor information
;;   (goto-char 1483)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward "[ \\n]*{ *}" nil t)
;;       ;; (beginning-of-defun)   ;; beginning of constructor
;;       ;; (M (point))
;;       (M (g--c-defun-params))
;;       )
;;     )

;;   ;; (M (get-cpp-constructor "tileCreateKernel"))

;;   ;; (M (g--cc-constructor-params))

;;   ;; functor constructors have a particular form. the function body is empty.


;; )


;; ;; TEST-1
;; (with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
;;   (goto-char 3541) ;; expect the point to be on the kernel invocation
;;   (M
;;    (concat
;;     g--function-to-port "CreateKernel<T>" "("
;;     (string-join (cdr (g--c-invocation-params)) ",") ")"))
;;   )


;; TEST-1 C PARAM PARSING
(with-current-buffer (find-file-noselect g--opencl-kernel-fn)
  (goto-char 720)
  (M "!!")

  ;; (M (prin1-to-string (g--c-defun-params)))

  (dolist (g--c-defun-params)
    )

  (M "VV")
  (cl-assert
   (equal (g--c-defun-params)
          '("global T *out" "global const T *in" "const KParam op" "const KParam ip" "const int blocksPerMatX" "const int blocksPerMatY")))
  ;; (cl-assert
  ;;  (equal (g--c-defun-params-types)
  ;;         '("global T *" "global const T *" "const KParam" "const KParam" "const int" "const int")))
  ;; (cl-assert
  ;;  (equal (g--c-defun-params-names)
  ;;         '("out" "in" "op" "ip" "blocksPerMatX" "blocksPerMatY")))
  )


;; ;; ;; TEST-2 FUNCTOR GENERATION IN ISOLATION
;; (cl-assert
;;  (string-equal
;;   (g--functor-string 720 (find-file-noselect g--opencl-kernel-fn))
;;   (with-temp-buffer
;;     (insert-file-contents "g--functor-string-gold.txt")
;;     (buffer-string))))


;; ;; ;; TEST-3 FUNCTOR INVOKE IN ISOLATION
;; ;; (MM (with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
;; ;;       (goto-char 3550)
;; ;;       (g--c-defun-invoke-params)
;; ;;       ))




;; ;; (0) user harnesses af
;; (g-harness-arrayfire "~/tile-af")



;; ;; ;; END TO END FINAL TEST is disabled while we sort out some subroutines
;; ;; (if t
;; ;;     (let (driver-shell functor driver pt-in-opencl-driver)
;; ;;       ;; (M "===========")

;; ;;       ;; (1) user generates oneapi driver shell and pastes into oneapi driver
;; ;;       (setq driver-shell (g--oneapi-driver-shell (find-file-noselect g--opencl-driver-fn)))  ;; AUTO
;; ;;       (kill-new driver-shell)

;; ;;       ;; (2) user pastes driver
;; ;;       (with-current-buffer (find-file-noselect g--oneapi-driver-fn)
;; ;;         (erase-buffer) (yank) (save-buffer))

;; ;;       ;; (1) user generates functor and copies it
;; ;;       (setq functor (g--functor-string 720 (find-file-noselect g--opencl-kernel-fn)))  ;; AUTO
;; ;;       (kill-new functor)

;; ;;       ;; (3) user pastes functor into driver below write_accessor
;; ;;       (with-current-buffer (find-file-noselect g--oneapi-driver-fn)
;; ;;         (goto-char (point-min)) (re-search-forward "write_accessor") (end-of-line)
;; ;;         (insert "\n\n") (yank) (insert "\n\n")
;; ;;         (save-buffer))
;; ;;       ;; (M "step 3 complete")

;; ;;       ;; (4) user positions point in old driver function
;; ;;       (setq pt-in-opencl-driver
;; ;;             (with-current-buffer (find-file-noselect g--oneapi-driver-fn)
;; ;;               (goto-char (point-min)) (re-search-forward "void +tile") (forward-line 3) (point)))
;; ;;       ;; (M "step 4 complete")

;; ;;       ;; (5) user generates driver and copies
;; ;;       (setq driver (g--driver-string pt-in-opencl-driver (find-file-noselect g--oneapi-driver-fn)))
;; ;;       (kill-new driver)
;; ;;       ;; (M "step 5 complete")

;; ;;       ;; (7) user replaces old driver with new driver (from kill ring)
;; ;;       (with-current-buffer (find-file-noselect g--oneapi-driver-fn)
;; ;;         (goto-char pt-in-opencl-driver)
;; ;;         (save-excursion
;; ;;           (save-restriction
;; ;;             (narrow-to-defun)
;; ;;             (delete-region (point-min) (point-max))
;; ;;             (yank)  ;; paste new driver
;; ;;             ))
;; ;;         (save-buffer))
;; ;;       (M "step 7 complete")

;; ;;   ;;     ;; (7) compile
;; ;;   ;;     (compile "cd ~/tile-af/build && make")
;; ;;   ;;     )
;; ;;   ;; )
;; ;; ))
