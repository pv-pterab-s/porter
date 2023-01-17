;;; porter.el --- port an opencl function to oneapi within an arrayfire clone -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/test.el\")"; -*-
(gallagher-eval-buffer "g-utils.el")


(defun g-harness-arrayfire (arrayfire-dir)
  (interactive "Denter arrayfire clone directory: ")
  (setq g--arrayfire-dir arrayfire-dir)
  ;; (setq g--function-to-port (replace-regexp-in-string
  ;;                            "-af$" ""
  ;;                            (file-name-nondirectory (directory-file-name
  ;;                                                     g--arrayfire-dir))))

  (setq g--function-to-port (read-string "function name to port? "))
  (M (concat "!!! "  g--function-to-port))

  (setq g--opencl-kernel-fn (concat g--arrayfire-dir "/src/backend/opencl/kernel/" g--function-to-port ".cl"))
  (setq g--opencl-driver-fn (concat g--arrayfire-dir "/src/backend/opencl/kernel/" g--function-to-port ".hpp"))
  (setq g--oneapi-driver-fn (replace-regexp-in-string "opencl/" "oneapi/" g--opencl-driver-fn))

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
          (mapcar* #'(lambda (param-type is-global param-name)
                       (cond ((not is-global) "not-accessor")
                             (should-ask-questions (g--ask-if-param-is-read-or-write
                                                    (concat param-type " " param-name)))
                             (t "write-only")
                             ))
                   (g--c-defun-params-types) is-global-flags (g--c-defun-params-names)))

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
                                  ("global\\(.*\\) \\([^=\\n]+\\)[^\\n]*=" . "\\1 \\2=")))
      (g--zip search-regexps replacements))

     "\n"
     "private:\n"
     (string-join (mapcar #'(lambda (x) (concat x "_")) oneapi-params) ";\n") ";\n"
     "};\n"
     )
    )
  )


(defun g--functor-string (point-in-kernel buffer &optional should-ask-questions)
  (interactive (list (point) (current-buffer)))
  (with-current-buffer buffer
    (goto-char point-in-kernel)
    (g--functor-string-helper should-ask-questions)))


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
  (replace-regexp-in-string
   " *$" ""
   (g--replace-list-of-pairs
    string
    (list '("using +cl::[^;]*;" . "")
          '("^ *auto +.*=[^;]*;" . "")
          '("[^ .]+\\.emplace_back[^;]*;" . "")
          '("Param\\( +\\)" . "Param<T>\\1")
          '("using +std::vector *;" . "")
          '(" *std::array[^;]*;" . "")
          '(" *std::vector[^;]*;" .  "")
          '(" *vector[^;]*;" .  "")
          '("CL_DEBUG_FINISH" . "ONEAPI_DEBUG_FINISH")
          '(
            "\\(cl::\\)?NDRange +\\([^ ]+\\) *(\\([^,]+\\),\\([^,]+\\),\\([^,]+\\))[;]*;" .
            "auto \\2 = sycl::range(\\3,\\4);"
            )
          '(
            "\\(cl::\\)?NDRange +\\([^ ]+\\) *(\\([^,]+\\),\\([^,]+\\))[;]*;" .
            "auto \\2 = sycl::range(\\3,\\4);"
            )
          )))
  )


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


(defun g--driver-point-on-invoke (point-in-opencl-driver buffer)
  (interactive (list (point) (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (let ((end (save-excursion (end-of-defun) (point))))
        (beginning-of-defun)
        (re-search-forward "EnqueueArgs(.*getQueue[^;]*;")
        (match-beginning 0)
        ))))


(defun g--driver-string (point-in-opencl-driver buffer)
  (interactive (list (point) (current-buffer)))
  (with-current-buffer buffer
    (let ((driver-string
           (buffer-substring-no-properties
            (save-excursion (beginning-of-defun) (point))
            (save-excursion (end-of-defun) (point)))))
      (replace-regexp-in-string
       "[^;]*EnqueueArgs([^;]*;"
       (concat
        "\n\n"    ;; vvv cursor _must_ be on the opencl kernel dispatch
        (g--functor-dispatch (g--driver-point-on-invoke
                              point-in-opencl-driver buffer)
                             buffer)
        "\n"
        )
       (g--driver-string-first-filter driver-string))
      )))


(defun gdp-add-functor-param (param-type-and-name)   ;; just assumes in functor params
  ;; TODO no error checking and not generalized
  (interactive "s")
  (save-excursion
    (goto-char (search-forward-regexp ")"))
    (backward-char)
    (insert (concat ", " param-type-and-name))
    (goto-char (search-forward-regexp "{"))
    (backward-char)
    (let ((param-name (replace-regexp-in-string ".* \\([^ ]+ *\\)$" "\\1" param-type-and-name)))
      (insert (format ", %s_(%s) " param-name param-name))  ;; constructor set private shorthand
      (goto-char (search-forward-regexp "const {"))   ;; should be beginning of operator
      (mark-defun)
      (message "%s" (point)) (message "%s" (mark))
      (replace-regexp-in-region param-name
                                (concat param-name "_")
                                (point) (mark))
      (goto-char (search-forward-regexp "^ *};"))   ;; TODO regexp might not work
      (forward-line -1) (end-of-line) (newline)
      (insert (concat param-type-and-name "_;")))))



(global-set-key (kbd "M-1") #'(lambda () (interactive) (find-file g--opencl-driver-fn)))
(global-set-key (kbd "M-2") #'(lambda () (interactive) (find-file g--opencl-kernel-fn)))
(global-set-key (kbd "M-3") #'(lambda () (interactive) (find-file g--oneapi-driver-fn)))
(global-set-key (kbd "M-4") #'(lambda () (interactive)
                                (find-file
                                 (concat "~/porter/out/" g--function-to-port "/"))))

;; TODO
;; (global-set-key (kbd "M-0") #'(lambda () (interactive)
;;                                 (find-file
;;                                  (concat g--arrayfire-dir "/src/backend/opencl/kernel/" g--function-to-port ".cl")
;;                                  (concat "~/porter/out/" g--function-to-port "/"))))




(global-set-key (kbd "C-c 1")
                #'(lambda (arrayfire-dir)
                    (interactive "Denter arrayfire clone directory: ")
                    (g-harness-arrayfire arrayfire-dir)
                    (message (concat "harnessed " g--arrayfire-dir " function " g--function-to-port
                                     " goto step C-c 2"))
                    )
                )

(global-set-key (kbd "C-c 2")
                #'(lambda () (interactive)
                    (find-file g--opencl-driver-fn)
                    (let ((driver-shell (g--oneapi-driver-shell (find-file-noselect g--opencl-driver-fn))))
                      (kill-new (MM driver-shell))
                      )
                    (setq header-line-format "(2) generated driver shell")
                    (set-face-attribute 'header-line nil
                                        :background "DarkGreen"
                                        :foreground "White")
                    ))

(global-set-key (kbd "C-c 3")
                #'(lambda () (interactive)
                    (find-file g--oneapi-driver-fn)
                    (setq header-line-format "(3) paste driver-shell into oneapi file (use C-y)")
                    (set-face-attribute 'header-line nil
                                        :background "DarkGreen"
                                        :foreground "White")
                    )
                )

(global-set-key (kbd "C-c h") #'(lambda (pt buffer)
                                  (interactive (list (point) (current-buffer)))
                                  (kill-new (MM (g--functor-string pt buffer t)))
                                  ))
(global-set-key (kbd "C-c 4")
                #'(lambda () (interactive)
                    (find-file g--opencl-kernel-fn)
                    (setq header-line-format "(4) gen functor from point in opencl kernel (use C-c h)")
                    (set-face-attribute 'header-line nil
                                        :background "DarkGreen"
                                        :foreground "White")
                    )
                )

(global-set-key (kbd "C-c 5")
                #'(lambda () (interactive)
                    (find-file g--oneapi-driver-fn)
                    (setq header-line-format "(5) paste functor. (use C-y) ")
                    (set-face-attribute 'header-line nil
                                        :background "DarkGreen"
                                        :foreground "White")
                    )
                )

(global-set-key (kbd "C-c j") #'(lambda (pt buffer)
                                  (interactive (list (point) (current-buffer)))
                                  (kill-new (MM (g--driver-string pt buffer)))
                                  ))
(global-set-key (kbd "C-c 6")
                #'(lambda () (interactive)
                    (setq header-line-format "(6) create new driver from inside old (use C-c j) and insert")
                    (set-face-attribute 'header-line nil
                                        :background "DarkGreen"
                                        :foreground "White")
                    )
                )

;; remember to wrap call in
;;   if constexpr (!(std::is_same_v<T, double> || std::is_same_v<T, cdouble>)) {
;; remember to uncomment <kernel/???.hpp>
