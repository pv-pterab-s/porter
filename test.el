;; (add-to-list 'load-path (concat (getenv "HOME") "/porter"))
;; (load "porter.el")

;; (setq test-opencl-kernel-fn "~/tile-af/src/backend/opencl/kernel/tile.hpp")
;; (setq test-oneapi-kernel-fn "~/tile-af/src/backend/oneapi/kernel/tile.hpp")
;; (setq test-oneapi-driver-fn "~/tile-af/src/backend/oneapi/kernel/tile.cpp")

;; (g--functor-string 720 test-opencl-kernel-fn)

;; (progn
;;   ;; STEP 2,3,4 gen functor and put in kill ring from opencl kernel
;;   (save-excursion
;;     (with-current-buffer (find-file-noselect (g--opencl-kernel-fn))
;;       (goto-char 720)
;;       (let ((doodle (g--functor-string)))
;;         (kill-new doodle)   ;; this simulates the interactive command
;;         )
;;       )
;;     )

;;   ;; STEP 5,6 insert functor into driver
;;   (save-excursion
;;     (with-current-buffer (find-file-noselect (g--oneapi-driver-fn))
;;       (goto-char (point-min))
;;       (re-search-forward "write_accessor") ;; end of definitions
;;       (end-of-line) (insert "\n\n")
;;       (yank)  ;; insert functor
;;       (insert "\n\n")
;;       )
;;     )

;;   ;; STEP 7,8,9 make a driver function
;;   (save-excursion
;;     (with-current-buffer (find-file-noselect (g--oneapi-driver-fn))
;;       (goto-char 2872)
;;       (let (driver-string (g--driver-string)) ;; create the driver
;;         (save-restriction   ;; blow away the original driver
;;           (narrow-to-defun)
;;           (erase-buffer)
;;           (insert "sdsdsdsdsdsdsdsdsd")
;;           )
;;         )
;;       )
;;       (gdp--display-string-other-window
;;        (with-current-buffer (find-file-noselect (g--oneapi-driver-fn))
;;          (buffer-string)
;;          )
;;        )
;;       )
;;     )
;;   )
