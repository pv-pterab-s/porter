;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/r.el\")"; -*-
(setq g--testing t)

(gallagher-eval-buffer "g-utils.el")
(gallagher-eval-buffer "porter.el")

;; TEST-1 GENERATE FUNCTOR CALL FROM KERNEL INVOKE
(with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
  (goto-char 3541) ;; expect the point to be on the kernel invocation

  ;; (MM (buffer-substring-no-properties (- (point) 20) (+ (point) 20)))

  (let ((invoke-params (g--c-invocation-params (point) (current-buffer)))
        (functor-params (g--functor-constructor-params (current-buffer)))
        )
    ;; (let ((is-param-ro-accessor
    ;;        (mapcar #'(lambda (param) (string-match "read" param)) functor-params))
    ;;       (is-param-wo-accessor
    ;;        (mapcar #'(lambda (param) (string-match "write" param)) functor-params
    ;;       (is-param-not-accessor
    ;;        (mapcar #'(lambda (param) (not (string-match "accessor" param)))))
    ;;        )
    (let ((param-kinds (mapcar #'(lambda (param)
                                   (cond ((string-match "read.*accessor" param) "READ")
                                         ((string-match "write.*accessor" param) "WRITE")
                                         (t "normal"))) functor-params))
          )
      )
    )
  )
