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
  )


(defun g--oneapi-driver-shell (buffer)
  (interactive (list (buffer)))
  (with-temp-buffer
    (insert (with-current-buffer buffer (buffer-string)))
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
using write_accessor = sycl::accessor<T, 1, sycl::access::mode::write>;\n\n")
    (buffer-string)))


(defun g--ask-if-param-is-read-or-write (param)
  ;; (if (called-interactively-p) (cl-return "read-only"))
  (cadr
   (read-multiple-choice
    (format "is the argument \"%s\" read-only or write-only?" param)
    '((?r "read-only" "The accessor should be read-only.")
      (?w "write-only" "The accessor should be write-only.")))))


(defun g--functor-string-helper (should-ask-questions)
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
                             (t "write-only")
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

     (g--replace-list-of-pairs
      (g--replace-list-of-pairs (g--c-defun-body)
                                '(("get_group_id" . "g.get_group_id")
                                  ("get_local_id" . "it.get_local_id")
                                  ("barrier([^)]*)" . "it.barrier()")
                                  ("get_local_size" . "g.get_local_range")
                                  ("global\\(.*\\) \\([^=\\n]+\\)[^\\n]*=" . "\\1 \\2=")
                                  ("NDRange" . "AAA")))
      (g--zip search-regexps replacements))

     "\n"
     "private:\n"
     (string-join (mapcar #'(lambda (x) (concat x "_")) oneapi-params) ";\n") ";\n"
     "};\n"
     )
    )
  )






(defun g--driver-accessor-decls (point-on-invocation buffer)
  (interactive (list (point) (current-buffer)))

  (let ((functor-params (g--functor-constructor-params buffer))
        (invocation-params (cdr (g--c-invocation-params point-on-invocation buffer))))

    (let (accessor-ro-decls accessor-wo-decls)
      (setq accessor-ro-decls
            (mapconcat #'(lambda (pair)
                           (concat "sycl::accessor d_"
                                   (let ((str (cdr pair)))
                                     (string-match "^[ *]*\\([^ .]*\\)" str)
                                     (match-string 1 str))  ;; base struct name w/o field or ptr
                                   "{" (cdr pair) ", h, sycl::read_only};"))
                       (seq-filter #'(lambda (pair)
                                       (string-match "read" (car pair)))
                                   (g--zip functor-params invocation-params))
                       "\n")
            )

      (setq accessor-wo-decls
            (mapconcat #'(lambda (pair)
                           (concat "sycl::accessor d_"
                                   (let ((str (cdr pair)))
                                     (string-match "^[ *]*\\([^ .]*\\)" str)
                                     (match-string 1 str))
                                   "{" (cdr pair) ", h, sycl::write_only, sycl::no_init};"))
                       (seq-filter #'(lambda (pair)
                                       (string-match "write" (car pair)))
                                   (g--zip functor-params invocation-params)) "\n"))

      (concat accessor-ro-decls "\n" accessor-wo-decls "\n")
      )
    )
  )


(defun g--driver-string-first-filter (string)
  (g--replace-list-of-pairs
   string
   (list '("using +cl::[^;]*;" . "")
         '("^ *auto +.*=[^;]*;" . "")
         '("[^;]*emplace_back[^;]*;" . "")
         '("Param\\( +\\)" . "Param<T>\\1")
         '("using +std::vector *;" . "")
         '(" *std::array[^;]*;" . "")
         '(" *std::vector[^;]*;" .  "")
         '(" *vector[^;]*;" .  "")
         '("cl::NDRange local" . "auto local = sycl::range" )
         '("cl::NDRange global" . "auto global = sycl::range")
         '("NDRange local" . "auto local = sycl::range" )
         '("NDRange global" . "auto global = sycl::range")
         '("CL_DEBUG_FINISH" . "ONEAPI_DEBUG_FINISH"))))


(defun g--functor-invoke-param-string (point-on-invoke buffer)  ;; conversion from opencl. not the dispatch
  (interactive (list (point) (current-buffer)))
  (let* ((invoke-params (g--c-invocation-params point-on-invoke buffer))
         (functor-params (g--functor-constructor-params buffer))
         (param-kinds (mapcar #'(lambda (param)
                                  (cond ((string-match "read.*accessor" param) "READ")
                                        ((string-match "write.*accessor" param) "WRITE")
                                        (t "normal"))) functor-params)))

    (string-join
     (mapcar* #'(lambda (invoke-param functor-param kind)
                  (if (not (string-equal kind "normal"))
                      (concat "d_" (g--first-match "^[ *]*\\([^ .]*\\)" invoke-param))
                    invoke-param)
                  )
              (cdr invoke-params) functor-params param-kinds)
     ", "
     )
    )
  )


(defun g--functor-dispatch (point-on-invoke buffer)
  (interactive (list (point) (current-buffer)))
  (save-excursion
    (with-current-buffer buffer
      (concat
       "getQueue().submit([&](auto &h) {\n"
       (g--driver-accessor-decls point-on-invoke buffer)
       "h.parallel_for(\n"
       "  sycl::nd_range{global, local},\n"
       "  " (concat g--function-to-port "CreateKernel<T>(")
       (g--functor-invoke-param-string point-on-invoke buffer)
       "));\n"
       "});"))))


;; \TODO steps of port  e.g.  "C-c 1", "C-c 2", etc
(global-set-key (kbd "M-1") #'(lambda () (interactive) (find-file g--opencl-driver-fn)))
(global-set-key (kbd "M-2") #'(lambda () (interactive) (find-file g--opencl-kernel-fn)))
(global-set-key (kbd "M-3") #'(lambda () (interactive) (find-file g--oneapi-driver-fn)))
