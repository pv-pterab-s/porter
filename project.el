;; utility functions
;; (defun print-variable-values (variable-names)
;;   "Prints the names and string values of variables in `variable-names` list."
;;   (dolist (var variable-names)
;;     (let ((value (symbol-value var)))
;;       (when (stringp value)
;;         (message "    %s: %s" var value)))))

(defun print-variable-values (variable-names)
  "Prints the names and string values of variables in `variable-names` list with aligned values."
  (let ((max-name-length 0))
    ;; Find the maximum length of variable names
    (dolist (var variable-names)
      (when (symbol-value var)
        (setq max-name-length (max max-name-length (length (symbol-name var))))))

    ;; Print variable names and values with alignment
    (dolist (var variable-names)
      (let ((value (symbol-value var)))
        (when (and value (stringp value))
          (message "%s:%s%s" var (make-string (- 25 max-name-length) ? ) value))))))





;; there are three types of functions... CALL, DRIVER, KERNEL

;; 1 and 2 are the CALL files, and contain multiple call functions...
(global-set-key (kbd "M-1")
                #'(lambda () (interactive)
                    (find-file "~/arrayfire/src/backend/oneapi/sparse.cpp")))

(global-set-key (kbd "M-2")
                #'(lambda () (interactive)
                    (find-file "~/arrayfire/src/backend/opencl/sparse.cpp")))

(global-set-key (kbd "M-9")
                #'(lambda () (interactive)
                    (find-file "~/sparse/main.cpp")))

(defun switch-to-compilation-buffer ()
  "Switch to the *compilation* buffer."
  (interactive)
  (let ((buffer (get-buffer "*compilation*")))
    (if buffer
        (switch-to-buffer buffer)
      (message "No *compilation* buffer found."))))
(global-set-key (kbd "M-8") 'switch-to-compilation-buffer)

;; DONE copy CALL functions (one file) over to oneAPI
;; select a DRIVER function

;; configure the problem
(setq g--arrayfire-dir "~/arrayfire")
(setq g--function-to-port "swapIndex")
(setq g--opencl-kernel-fn (concat g--arrayfire-dir "/src/backend/opencl/kernel/" g--function-to-port ".cl"))
(setq g--opencl-driver-fn (concat g--arrayfire-dir "/src/backend/opencl/kernel/sparse.hpp"))
(setq g--oneapi-driver-fn (replace-regexp-in-string "opencl/" "oneapi/" g--opencl-driver-fn))

(message "problem setup is:")
(print-variable-values '(g--arrayfire-dir
                         g--function-to-port
                         g--opencl-driver-fn
                         g--oneapi-driver-fn))

;; C-c 2 is ok
;; (setq g--driver-shell (g--oneapi-driver-shell (find-file-noselect g--opencl-driver-fn)))
;; C-c 3 is ok (just pastes the beginnings of the driver function)

;; C-c 4, C-c h is ok. it just gens a functor and puts it in the kill
;;                     ring. it can handle multiple kernels in a file

;; C-c 5 is ok. just paste the functor into the driver file

;; -------------------------
;; procedure...
;;  C-c 2   will generate a simple driver string
;;  C-c 3   will open oneapi driver. paste and clear rest of file
;;  C-c 4   will open the opencl kernel file. place cursor in kernel and C-c h
;;  C-c 5   will open driver (again). paste functor above driver function

;;    vv this is the problem!

;;  C-c j generates a new driver function, but searches for a new
;;        functor in order to get the read/write attributes of the
;;        functor call correct. it is not smart enough to handle
;;        multiple kernel classes in one buffer.
