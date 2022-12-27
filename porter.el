;;; porter.el --- port an opencl function to oneapi within an arrayfire clone -*- lexical-binding: t; compile-command: "emacsclient -e '(load \"/home/gpryor/porter/test.el\")'"; -*-
(require 'g-utils)  ;; got to be one the load path


(defun g--opencl-kernel-fn () (concat g--arrayfire-dir "/src/backend/opencl/kernel/" g--function-to-port ".cl"))
(defun g--opencl-driver-fn () (concat g--arrayfire-dir "/src/backend/opencl/kernel/" g--function-to-port ".hpp"))
(defun g--oneapi-driver-fn () (replace-regexp-in-string "opencl/" "oneapi/" (g--opencl-driver-fn)))


(defun g-harness-arrayfire (arrayfire-dir)
  (interactive "Denter arrayfire clone directory: ")

  (unless (file-directory-p arrayfire-dir) (error "harnessed arrayfire must be a valid dir"))
  (setq g--arrayfire-dir arrayfire-dir)  ;; global defines target af clone (checking some things)

  ;; globally define target function to port (checking some things)
  (let ((this-dir-name (file-name-nondirectory
                        (directory-file-name
                         (expand-file-name arrayfire-dir)))))
    (if (not (string-match "-af$" this-dir-name))
        (error "must run in arrayfire dir named with trailing \"-af\""))
    (setq g--function-to-port (replace-regexp-in-string "-af$" "" this-dir-name)))

  (message (concat "harnessed arrayfire at " g--arrayfire-dir
                   " function " g--function-to-port))

  ;; generate new oneapi driver file if missing from harnessed clone
  (if (or t (not (file-exists-p (g--oneapi-driver))))ttes
      (progn
        ;; oneapi is copy of opencl driver
        (message "copying opencl driver into oneapi kernel")
        (with-current-buffer (find-file-noselect (g--oneapi-driver-fn))
          (erase-buffer)
          (insert
           (with-current-buffer (find-file-noselect (g--opencl-driver-fn))
             (buffer-string)))

          ;; make non-user-input changes to oneapi driver
          (replace-regexp-in-region "opencl" "oneapi" (point-min) (point-max))
          (goto-char (point-min))
          (re-search-forward "namespace kernel {")
          (insert "

template<typename T>
using local_accessor = sycl::accessor<T, 1, sycl::access::mode::read_write,
                                      sycl::access::target::local>;
template<typename T>
using read_accessor = sycl::accessor<T, 1, sycl::access::mode::read>;
template<typename T>
using write_accessor = sycl::accessor<T, 1, sycl::access::mode::write>;
")

          )  ;; with-current-buffer
        ) ;; progn
    )  ;; if

  ;; (find-file-other-window (g--oneapi-driver-fn))

  ) ;; defun

(defun g--functor-string-helper ()
  (with-current-buffer (find-file-noselect filename))
  (goto-char point)

  (if (not  ;; don't let this function run from a confusing place
       (and (string= (file-name-extension (buffer-file-name)) "cl")
            (save-excursion
              (beginning-of-defun)
              (looking-at "^kernel"))))
      (error "must run from within an opencl kernel"))

  (let* (
         ;; is each accessor read or write only?
         (read-or-write
          (mapcar #'(lambda (s)
                      (if (string-match "\\(global +const\\|global\\)" s) "read-only"
                          ;; (cadr
                          ;;  (read-multiple-choice
                          ;;   (format "is the argument \"%s\" read-only or write-only?" s)
                          ;;   '((?r "read-only" "The accessor should be read-only.")
                          ;;     (?w "write-only" "The accessor should be write-only."))))
                        "not-accessor")
                      )
                  (g--c-defun-params)
                  ))
         ;; functor-params is oneapi. globals replaced with accessors
         (functor-params
          (mapcar* #'(lambda (s read-or-write-flag)
                       (replace-regexp-in-string "\\(global +const\\|global\\)"
                                                 (if (string-match "read-only" read-or-write-flag)
                                                     "read_accesor<T>"
                                                   "write_accessor<T>")
                                                 s)
                       )
                   (g--c-defun-params) read-or-write)
          )
         ;; functor-params names with type filtered out
         (param-names
          (mapcar #'(lambda (s)
                             (string-match "[^ ]+[ ]*$" s)  ;; only parameter name. not type
                             (match-string 0 s))
                         (g--c-defun-params))
              ))

    (concat
     "template<typename T>\n"
     "class " (c-defun-name) "CreateKernel {\n"
     "public:\n"
     "    "
     (c-defun-name) "CreateKernel(" (string-join functor-params ",") ")\n"
     (mapconcat #'(lambda (s) (concat s "_(" s ")")) param-names ", ")
     " : {}\n\n"
     "    void operator()(sycl::nd_item<2> it) const {\n"
     "        sycl::group g = it.get_group();\n"

     (g--replace-list-of-pairs-in-string
      (save-excursion
        (beginning-of-defun) (re-search-forward "{")
        (buffer-substring-no-properties (point)
                                        (progn (end-of-defun) (point))))
      (append
       '(("get_group_id" . "g.get_group_id")
         ("get_local_id" . "it.get_local_id")
         ("barrier([^)]*)" . "it.barrier()")
         ("get_local_size" . "g.get_local_range")
         ("global\\(.*\\) \\([^=\\n]+\\)[^\\n]*=" . "\\1 \\2="))
       (seq-sort #'(lambda (pair-a pair-b)  ;; privates
                     (< (length (car pair-a)) (length (car pair-b)))
                     )
                 (mapcar #'(lambda (s)
                             (cons (concat "\\([^A-Za-z0-9_]\\)\\("
                                           s
                                           "\\)\\([^A-Za-z0-9_]\\)")
                                   (concat "\\1\\2_\\3")))
                         param-names))))
     "\n"
     "private:\n"
     (string-join functor-params ";\n")
     "\n};\n"
     ) ;; concat
    ) ;; let
  ) ;; defun

(defun g--functor-string-helper (point &optional filename)
  (interactive "d")
  (if boundp filename
    (with-current-buffer (find-file-noselect filename)
      (goto-char point)
      (g--functor-string))
  (progn
    (goto-char point)
    (g--functor-string))
  ))

(defun g--gen-functor-call (functor-decl-params kernel-invoke functor-name)
  (let* ((functor-decl-list (g--params-list functor-decl-params))
         (kernel-invoke-list (g--params-list kernel-invoke)))
    (concat
     "auto Q = getQueue();\n"
     "  Q.submit([&](auto &h) {\n"

     (mapconcat #'(lambda (x) x)
                (seq-filter #'(lambda (x) (string-match "accessor" x))
                            functor-decl-list)
                ";\n")
     ";\n"

     "  h.parallel_for(\n"
     "     sycl::nd_range{global, local},\n"

     functor-name "CreateKernel<T>("
     (string-join
      (seq-filter #'(lambda (x)
                      (not (string-match "EnqueueArgs" x))
                      )
                  kernel-invoke-list)
      ",")
     "));"

     "\n"
     "});"
     )
    ) ;; let
  ) ;; defun


(defun g--driver-string ()
  (let ()
    (save-excursion
      (save-restriction
        (narrow-to-defun)
        (g--replace-list-of-pairs-in-string  ;; from utils
         (buffer-string)
         (list '("^ *auto +.*=[^;]*;" . "")
               '("Param\\( +\\)" . "Param<T>\\1" )
               '("using +std::vector *;" . "")
               '(" *std::array[^;]*;" . "")
               '(" *std::vector[^;]*;" .  "")
               '(" *vector[^;]*;" .  "")
               ;; '(" *\\( +\\)vector[^;]*;" .  "\\1")
               '("cl::NDRange local" . "auto local = sycl::range" )
               '("cl::NDRange global" . "auto global = sycl::range")
               '("CL_DEBUG_FINISH" . "ONEAPI_DEBUG_FINISH")
               ;; (cons "^.*EnqueueArgs[^;]*;" (g--gen-functor-call functor-decl-params
               ;;                                                   kernel-invoke
               ;;                                                   functor-name))
               ))
        ) ;; save-restriction
      ) ;; save-excursion
    ) ;; let
  ) ;; defun


;; steps of port
(global-set-key (kbd "C-c 1") #'(lambda ()   ;; generate the right functor
                                  (interactive)
                                  (find-file (g--opencl-kernel-fn))))
(global-set-key (kbd "C-c 2") #'(lambda ()    ;; manually insert functor into oneapi driver
                                  (interactive)
                                  (kill-new (gdp--display-string-other-window (g--functor-string)))
                                  (find-file (g--oneapi-driver-fn))
                                  ))
(global-set-key (kbd "C-c 3") #'(lambda ()  ;; go to driver function and generate driver
                                  (interactive)
                                  (find-file (g--oneapi-driver-fn))))

(global-set-key (kbd "M-1") #'(lambda () (interactive) (find-file (g--opencl-driver-fn))))
(global-set-key (kbd "M-2") #'(lambda () (interactive) (find-file (g--opencl-kernel-fn))))
(global-set-key (kbd "M-3") #'(lambda () (interactive) (find-file (g--oneapi-driver-fn))))



(provide 'porter)
