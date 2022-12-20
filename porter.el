;;; porter.el --- port an opencl function to oneapi within an arrayfire clone -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Gallagher Pryor

;; Author: Gallagher Pryor <gpryor@a770>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'g-utils)  ;; got to be one the load path


(defun g--opencl-kernel-fn () (concat g--arrayfire-dir "/src/backend/opencl/kernel/" g--function-to-port ".cl"))
(defun g--opencl-driver-fn () (concat g--arrayfire-dir "/src/backend/opencl/kernel/" g--function-to-port ".hpp"))
(defun g--oneapi-driver-fn () (replace-regexp-in-string "opencl/" "oneapi/" (g--opencl-driver-fn)))


;; C-c 1,2,3 switch between files
(global-set-key (kbd "C-c 1") #'(lambda () (interactive) (find-file (g--oneapi-driver-fn))))
(global-set-key (kbd "C-c 2") #'(lambda () (interactive) (find-file (g--opencl-kernel-fn))))
(global-set-key (kbd "C-c 3") #'(lambda () (interactive) (find-file (g--opencl-driver-fn))))


(defun g--functor ()   ;; generate functor from kernel at point
  (let ((kernel-name "")
        (params ""))
  (format "
template<typename T>
class " kernel-name "CreateKernel {
public:
  " kernel-name "CreateKernel(" kernel-params ") : %s {}
  void operator()(sycl::nd_item<2> it) const {
    sycl::group g = it.get_group();
    %s
  }
;; private:
;;   %s
;; };
;; "
  )))


(defun M (s) (message "%s" s) s)
(with-current-buffer (find-file-noselect "/home/gpryor/resize-af/src/backend/opencl/kernel/resize.cl")
  (goto-char 949)  ;; assume in a kernel

  ) ;; with-current-buffer


(defun g--gen-oneapi-driver ()
  ;; (interactive)
  (if (not (boundp 'g--arrayfire-dir))
      (error "you must harness an arrayfire directory first"))

  ;; copy "opencl driver" into "oneapi kernel"
  (message "copy opencl driver into oneapi kernel and display")
  (with-current-buffer (find-file-noselect (g--oneapi-driver-fn))
    (erase-buffer)
    (insert
     (with-current-buffer (find-file-noselect (g--opencl-driver-fn))
       (buffer-string))))

  ;; generate a functor for each kernel in the opencl kernel

  ) ;; defun


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
  (if (or t (not (file-exists-p (g--oneapi-driver))))
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

          )  ;; with-current-buffer
        ) ;; progn
    )  ;; if
  ) ;; defun


(defun gdp--functor-string-old ()
  (with-output-to-string
    (princ (format "
template<typename T>
class %sCreateKernel {
public:
  %sCreateKernel(%s) : %s {}
  void operator()(sycl::nd_item<2> it) const {
    sycl::group g = it.get_group();
    %s
  }
private:
  %s
};
"
                   (gdp--name-of-kernel-at-point)
                   (gdp--name-of-kernel-at-point)
                   (gdp--oneapi-params-from-kernel-params (gdp--params-of-kernel-at-point))
                   (gdp--constructor-assigns-from-kernel-params (gdp--params-of-kernel-at-point))
                   (gdp--underscore-privates
                    (gdp--oneapi-body-from-opencl-body
                     (gdp--oneapi-params-from-kernel-params (gdp--params-of-kernel-at-point))
                     (gdp--body-of-kernel-at-point))
                    (gdp--names-from-params (gdp--params-of-kernel-at-point)))
                   (gdp--privates-from-kernel-params (gdp--params-of-kernel-at-point))
                   ))))

(defun g--kernel-name () )
(defun g--kernel-params () )
(defun g--kernel-params () )
(defun g--functor-body () )

(defun g--functor-string ()
  (if (not  ;; don't let this function run from a confusing place
       (and (string= (file-name-extension (buffer-file-name)) "cl")
            (save-excursion
              (beginning-of-defun)
              (looking-at "^kernel"))))
      (error "must run from within an opencl kernel"))

  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (let ((kernel-name )))))

  (concat
   "template<typename T>\n"
   "class " (c-defun-name) "CreateKernel {\n"
   "public:\n"
   "    " (c-defun-name) "CreateKernel("
   (mapconcat #'(lambda (s)
                  (replace-regexp-in-string "\\(global +const\\|global\\)" "sycl::accessor<T, 1>" s))
              (g--c-defun-params) ",")
   ") :"
   (mapconcat #'(lambda (s)

                  )
              (g--c-defun-params) "!!") " {}\n"
   "    void operator()(sycl::nd_item<2> it) const {\n"
   "        sycl::group g = it.get_group();\n\n"
   ;; "      "  (g--functor-body) "\n"
   "}\n"
   "private:\n"
   ;; (g--privates)
   "}\n"
   ) ;; concat
  ) ;; defun


;; do testing by starting repo from scratch
(M (shell-command-to-string "cd ~/resize-af && git clean -dfx"))
(g-harness-arrayfire "~/resize-af")
(with-current-buffer (find-file-noselect "/home/gpryor/resize-af/src/backend/opencl/kernel/resize.cl")
  (goto-char 3970)

  (gdp--display-string-other-window     ;; main
   (progn
     (g--functor-string)
     ;; (g-oneapi-port-defun)
     ;;   ^^ this is a string.
     )
   ) ;; display other window
  )



(provide 'porter)
