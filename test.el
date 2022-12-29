;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/test.el\")"; -*-
(setq g--testing t)
;; (setq test-opencl-kernel-fn "~/tile-af/src/backend/opencl/kernel/tile.cl")
;; (setq test-opencl-driver-fn "~/tile-af/src/backend/oneapi/kernel/tile.hpp")
;; (setq test-oneapi-driver-fn "~/tile-af/src/backend/oneapi/kernel/tile.cpp")

(gallagher-eval-buffer "porter.el")

;; reset the user environment
(shell-command-to-string "cd ~/tile-af && git clean -dfx src && (yes | git checkout .)")
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


(let (functor driver pt-in-opencl-driver)
  ;; (1) user generates functor
  (setq functor (g--functor-string 720 (find-file-noselect test-opencl-kernel-fn)))

  ;; (2) user copies the functor (they would do it manually)
  (kill-new functor)

  ;; (3) user pastes functor into driver below write_accessor
  (with-current-buffer (find-file-noselect test-oneapi-driver-fn)
    (goto-char (point-min)) (re-search-forward "write_accessor") (end-of-line)
    (insert "\n\n") (yank) (insert "\n\n")
    (save-buffer)
    )
  (M "step 3 complete")

  ;; (4) user positions point in old driver function
  (setq pt-in-opencl-driver
        (with-current-buffer (find-file-noselect test-oneapi-driver-fn)
          (goto-char (point-min)) (re-search-forward "void +tile") (forward-line 3) (point)))
  (M "step 4 complete")

  ;; (5) user generates driver
  (setq driver (g--driver-string pt-in-opencl-driver (find-file-noselect test-oneapi-driver-fn)))
  (M "step 5 complete")

  ;; (6) user copies the driver (they would do manually)
  (kill-new driver)

  ;; (7) user replaces old driver with new driver (from kill ring)
  (with-current-buffer (find-file-noselect test-oneapi-driver-fn)
    (goto-char pt-in-opencl-driver)
    (save-excursion
      (save-restriction
        (narrow-to-defun)
        (delete-region (point-min) (point-max))
        (yank)
        ))
    (save-buffer))
  (M "step 7 complete")

  ;; (7) compile
  (compile "cd ~/tile-af/build && make")
)
