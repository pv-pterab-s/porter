;;; porter.el --- port an opencl function to oneapi within an arrayfire clone -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/test.el\")"; -*-
(require 'g-utils)



(defun g-harness-arrayfire (arrayfire-dir)
  (interactive "Denter arrayfire clone directory: ")
  (setq g--arrayfire-dir arrayfire-dir)
  (setq g--function-to-port (replace-regexp-in-string "-af$" "" (file-name-nondirectory g--arrayfire-dir)))
  (setq g--opencl-kernel-fn (concat g--arrayfire-dir "/src/backend/opencl/kernel/" g--function-to-port ".cl"))
  (setq g--opencl-driver-fn (concat g--arrayfire-dir "/src/backend/opencl/kernel/" g--function-to-port ".hpp"))
  (setq g--oneapi-driver-fn (replace-regexp-in-string "opencl/" "oneapi/" (g--opencl-driver-fn)))

  (message (concat "harnessed arrayfire at " g--arrayfire-dir " function " g--function-to-port))

  (with-current-buffer (find-file-noselect g--oneapi-driver-fn)
    (erase-buffer)
    (insert-file-contents g--opencl-driver-fn)

    (replace-regexp-in-region "opencl" "oneapi" (point-min) (point-max))
    (delete-matching-lines ".*include.*kernel_headers.*" (point-min) (point-max))
    (goto-char (point-min))
    (re-search-forward "namespace kernel {")
    (insert "\n\ntemplate<typename T>
using local_accessor = sycl::accessor<T, 1, sycl::access::mode::read_write,
                                      sycl::access::target::local>;
template<typename T>
using read_accessor = sycl::accessor<T, 1, sycl::access::mode::read>;
template<typename T>
using write_accessor = sycl::accessor<T, 1, sycl::access::mode::write>;\n\n"
            )   ;; insert
    ) ;; with-current-buffer
  ) ;; defun


(defun g--ask-if-param-is-read-or-write (param)
  ;; (if (called-interactively-p) (cl-return "read-only"))
  (cadr
   (read-multiple-choice
    (format "is the argument \"%s\" read-only or write-only?" param)
    '((?r "read-only" "The accessor should be read-only.")
      (?w "write-only" "The accessor should be write-only.")))))


(defun g--functor-string-helper-new (should-ask-questions)
  (let (is-global-flags
        read-or-write-flags
        oneapi-param-types
        oneapi-params
        sorted-param-names
        search-regexps
        replacements)

    (setq is-global-flags (mapcar #'(lambda (param) (string-match "global" param)) (g--c-defun-params-types)))

    (setq read-or-write-flags
          (mapcar* #'(lambda (param-type is-global)
                       (cond ((not is-global) "not-accessor")
                             (should-ask-questions (g--ask-if-param-is-read-or-write param-type))
                             (t "read-only")
                             ))
                   (g--c-defun-params-types) is-global-flags))

    (setq oneapi-param-types
          (mapcar* #'(lambda (param-type read-or-write)
                       (cond
                        ((string-equal read-or-write "not-accessor") param-type)
                        ((string-equal read-or-write "read-only") "read_accessor<T>")
                        ((string-equal read-or-write "write-only") "write_accessor<T>")))
                   (g--c-defun-params-types) read-or-write-flags))

    (setq oneapi-params
          (mapcar* #'(lambda (type name) (concat (string-trim type) " " name))
                   oneapi-param-types (g--c-defun-params-names)))

    (setq sorted-param-names  ;; these three: for put underscores on private vars in functor body
          (g--sort-strings-by-length (g--c-defun-params-names)))
    (setq search-regexps
          (mapcar #'(lambda (s) (concat "\\([^A-Za-z0-9_]\\)\\(" s "\\)\\([^A-Za-z0-9_]\\)")) sorted-param-names))
    (setq replacements
          (mapcar #'(lambda (s) (concat "\\1\\2_\\3")) sorted-param-names))

    (concat
     "template<typename T>\n"
     "class " (c-defun-name) "CreateKernel {\n"
     "public:\n"
     "    "
     (c-defun-name) "CreateKernel(" (string-join oneapi-params ", ") ") : "
     (mapconcat #'(lambda (s) (concat s "_(" s ")")) (g--c-defun-params-names) ", ") " {}\n"
     "    void operator()(sycl::nd_item<2> it) const {\n"
     "        sycl::group g = it.get_group();\n"

     ;; (g--replace-list-of-pairs
      (g--replace-list-of-pairs (g--c-defun-body)
                                '(("get_group_id" . "g.get_group_id")
                                  ("get_local_id" . "it.get_local_id")
                                  ("barrier([^)]*)" . "it.barrier()")
                                  ("get_local_size" . "g.get_local_range")
                                  ("global\\(.*\\) \\([^=\\n]+\\)[^\\n]*=" . "\\1 \\2=")))
      ;; (g--zip search-regexps replacements))

     "\n"
     "private:\n"
     (string-join (mapcar #'(lambda (x) (concat x "_")) oneapi-params) ";\n") ";\n"
     "};\n"
     )
    )
  )
(defun g--functor-string (point buffer)
  (interactive (list (point) (current-buffer)))
  (with-current-buffer buffer
    (goto-char point);
    (g--functor-string-helper-new (called-interactively-p 'any))))


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


(defun g--driver-string-helper ()
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


(defun g--driver-string (point buffer)
  (interactive (list (point) (current-buffer)))
  (with-current-buffer buffer
    (goto-char point)
    (g--driver-string-helper)))


;; \TODO steps of port  e.g.  "C-c 1", "C-c 2", etc

(global-set-key (kbd "M-1") #'(lambda () (interactive) (find-file g--opencl-driver-fn)))
(global-set-key (kbd "M-2") #'(lambda () (interactive) (find-file g--opencl-kernel-fn)))
(global-set-key (kbd "M-3") #'(lambda () (interactive) (find-file g--oneapi-driver-fn)))
