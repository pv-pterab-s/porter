;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/test.el\")"; -*-
(setq g--testing t)
;; (setq test-opencl-kernel-fn "~/tile-af/src/backend/opencl/kernel/tile.cl")
;; (setq test-opencl-driver-fn "~/tile-af/src/backend/oneapi/kernel/tile.hpp")
;; (setq test-oneapi-driver-fn "~/tile-af/src/backend/oneapi/kernel/tile.cpp")

(gallagher-eval-buffer "porter.el")

;; reset the user environment
(shell-command-to-string "cd ~/tile-af && git clean -dfx build && (yes | git checkout .)")
(shell-command-to-string "cd ~/tile-af && git checkout 23b7bb7f")
(mapc #'(lambda (x)  ;; start emacs buffers from scratch
          (if (boundp x)
              (let ((buffer (find-buffer-visiting (eval x))))
                (when buffer
                  (progn
                    (with-current-buffer buffer (set-buffer-modified-p nil))
                    )))))
      '(g--opencl-kernel-fn g--opencl-driver-fn g--oneapi-driver-fn))


;; (mapc #'(lambda (x)
;;           (with-current-buffer (find-buffer-visiting (eval x))
;;             (revert-buffer t t))
;;           )

;; (0) user harnesses arrayfire directory
(g-harness-arrayfire "~/tile-af")

;; (with-current-buffer (find-file-noselect test-oneapi-driver-fn)
;;   (gdp--display-string-other-window (buffer-string)))


(let (functor driver)
  ;; (1) user generates functor
  (setq functor (g--functor-string 720 test-opencl-kernel-fn))

;;   ;; (2) user copies
;;   (kill-new functor)

;;   ;; (3) user pastes functor into driver below write_accessor
;;   (with-current-buffer (find-file-noselect test-oneapi-driver-fn)
;;     (M "HERE")
;;     (M (buffer-string))
;;     (gdp--display-string-other-window (buffer-string))
;;     ;; (goto-char (point-min)) (re-search-forward "write_accessor") (end-of-line) (insert "\n\n")
;;     ;; (yank)
;;     ;; (insert "\n\n")
;;     )
;;   )

;;   ;; ;; (4) user jumps into driver and generates new one
;;   ;; (setq driver (g--driver-string
;;   )





;; ; insert functor into driver



;; ;; (progn
;; ;;   ;; STEP 2,3,4 gen functor and put in kill ring from opencl kernel
;; ;;   (save-excursion
;; ;;     (with-current-buffer (find-file-noselect (g--opencl-kernel-fn))
;; ;;       (goto-char 720)
;; ;;       (let ((doodle (g--functor-string)))
;; ;;         (kill-new doodle)   ;; this simulates the interactive command
;; ;;         )
;; ;;       )
;; ;;     )

;; ;;   ;; STEP 5,6 insert functor into driver
;; ;;   (save-excursion
;; ;;     (with-current-buffer (find-file-noselect (g--oneapi-driver-fn))
;; ;;       (goto-char (point-min))
;; ;;       (re-search-forward "write_accessor") ;; end of definitions
;; ;;       (end-of-line) (insert "\n\n")
;; ;;       (yank)  ;; insert functor
;; ;;       (insert "\n\n")
;; ;;       )
;; ;;     )

;; ;;   ;; STEP 7,8,9 make a driver function
;; ;;   (save-excursion
;; ;;     (with-current-buffer (find-file-noselect (g--oneapi-driver-fn))
;; ;;       (goto-char 2872)
;; ;;       (let (driver-string (g--driver-string)) ;; create the driver
;; ;;         (save-restriction   ;; blow away the original driver
;; ;;           (narrow-to-defun)
;; ;;           (erase-buffer)
;; ;;           (insert "sdsdsdsdsdsdsdsdsd")
;; ;;           )
;; ;;         )
;; ;;       )
;; ;;       (gdp--display-string-other-window
;; ;;        (with-current-buffer (find-file-noselect (g--oneapi-driver-fn))
;; ;;          (buffer-string)
;; ;;          )
;; ;;        )
;; ;;       )
;; ;;     )
;; ;;   )
