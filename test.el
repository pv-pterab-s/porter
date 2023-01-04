;;; -*- lexical-binding: t; my-lisp-to-evaluate: "(gallagher-eval-buffer \"/home/gpryor/porter/test.el\")"; -*-
(gallagher-eval-buffer "g-utils.el")
(gallagher-eval-buffer "porter.el")

;; (0) user harnesses arrayfire dir
(g-harness-arrayfire "~/tile-af")

; reset the file system
(shell-command-to-string "cd ~/tile-af && git clean -dfx src && (yes | git checkout .)")
(shell-command-to-string "cd ~/tile-af && git checkout 23b7bb7f")

;; reset emacs buffers
(mapc #'(lambda (filename-symbol)
          (let* ((filename (eval filename-symbol))
                 (buffer (find-buffer-visiting filename)))
            (if buffer
                (with-current-buffer buffer (revert-buffer t t t)))
            ))
      '(g--opencl-kernel-fn g--opencl-driver-fn g--oneapi-driver-fn))



;; TEST-1 G--ZIP
(let ()
  (cl-assert
   (equal
    (g--zip '("doo" "da") '("flip" "fly"))
    '(("doo" . "flip") ("da" . "fly"))))
  )



;; TEST-1 C PARAM PARSING
(with-current-buffer (find-file-noselect g--opencl-kernel-fn)
  (goto-char 720)
  (cl-assert
   (equal (g--c-defun-params)
          '("global T *out" "global const T *in" "const KParam op" "const KParam ip" "const int blocksPerMatX" "const int blocksPerMatY")))
  (cl-assert
   (equal (g--c-defun-params-types)
          '("global T *" "global const T *" "const KParam" "const KParam" "const int" "const int")))
  (cl-assert
   (equal (g--c-defun-params-names)
          '("out" "in" "op" "ip" "blocksPerMatX" "blocksPerMatY")))
  )



;; TEST-0 EXTRACT C PARAMS FROM INVOCATIONS
(with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
  (goto-char 3541)
  (cl-assert
   (string-equal
    (g--c-invocation-params-string (point) (current-buffer))
    "EnqueueArgs(getQueue(), global, local), *out.data, *in.data, out.info, in.info, blocksPerMatX, blocksPerMatY"))
  (cl-assert
   (equal
    (g--c-invocation-params (point) (current-buffer))
    '("EnqueueArgs(getQueue(), global, local)" "*out.data" "*in.data" "out.info" "in.info" "blocksPerMatX" "blocksPerMatY")))
  )



;; TEST-1 PARSING FUNCTOR IN CURRENT FILE
(with-current-buffer (find-file-noselect "unconverted-driver-but-with-functor.cpp")
  (goto-char 3541) ;; expect the point to be on the kernel invocation
  (cl-assert
   (equal (g--functor-constructor-params (current-buffer))
          '("write_accessor<T> out" "read_accessor<T> in" "const KParam op" "const KParam ip" "const int blocksPerMatX" "const int blocksPerMatY")))
  )



;; TEST-1 ACCESSOR DECLS WORK
(cl-assert
 (string=
  (g--driver-accessor-decls 3541 (find-file-noselect "unconverted-driver-but-with-functor.cpp"))
  "sycl::accessor d_in{*in.data, h, sycl::read_only};
sycl::accessor d_out{*out.data, h, sycl::write_only, sycl::no_init};
"
  ))



;; TEST-1 FUNCTOR PARAMS GENERATION
(cl-assert
 (string=
  (g--functor-invoke-param-string 3541 (find-file-noselect "unconverted-driver-but-with-functor.cpp"))
  "d_out, d_in, out.info, in.info, blocksPerMatX, blocksPerMatY"
  )
 )



;; TEST-1 FUNCTOR DISPATCH GENERATION
(cl-assert
 (string=
  (g--functor-dispatch 3541 (find-file-noselect "unconverted-driver-but-with-functor.cpp"))
"getQueue().submit([&](auto &h) {
sycl::accessor d_in{*in.data, h, sycl::read_only};
sycl::accessor d_out{*out.data, h, sycl::write_only, sycl::no_init};
h.parallel_for(
  sycl::nd_range{global, local},
  tileCreateKernel<T>(d_out, d_in, out.info, in.info, blocksPerMatX, blocksPerMatY));
});"))



;; TEST-1 DRIVER FUNCTION GEN
(cl-assert
 (string=
  (MM (g--driver-string 3541 (find-file-noselect "unconverted-driver-but-with-functor.cpp")))
"template<typename T>
void tile(Param<T> out, const Param<T> in) {


    using std::string;


    constexpr int TX    = 32;
    constexpr int TY    = 8;
    constexpr int TILEX = 512;
    constexpr int TILEY = 32;






    auto local = sycl::range(TX, TY);

    int blocksPerMatX = divup(out.info.dims[0], TILEX);
    int blocksPerMatY = divup(out.info.dims[1], TILEY);
    auto global = sycl::range(local[0] * blocksPerMatX * out.info.dims[2],
                   local[1] * blocksPerMatY * out.info.dims[3]);

getQueue().submit([&](auto &h) {
sycl::accessor d_in{*in.data, h, sycl::read_only};
sycl::accessor d_out{*out.data, h, sycl::write_only, sycl::no_init};
h.parallel_for(
  sycl::nd_range{global, local},
  tileCreateKernel<T>(d_out, d_in, out.info, in.info, blocksPerMatX, blocksPerMatY));
});

    ONEAPI_DEBUG_FINISH(getQueue());
}
"))



;; (0) user harnesses af
(g-harness-arrayfire "~/tile-af")



;; END TO END FINAL TEST is disabled while we sort out some subroutines
(if t
    (let (driver-shell functor driver pt-in-opencl-driver)
      (M "===========")

      ;; (1) user generates oneapi driver shell and pastes into oneapi driver
      (setq driver-shell       ;; USER MUST put cursor anywhere in opencl driver buffer
            (g--oneapi-driver-shell (find-file-noselect g--opencl-driver-fn)))
      (kill-new driver-shell)

      ;; (2) user pastes driver
      (with-current-buffer (find-file-noselect g--oneapi-driver-fn)
        (erase-buffer) (yank) (save-buffer))

      ;; (1) user generates functor and copies it
      (setq functor    ;; USER MUST cursor inside of kernel
            (g--functor-string 720 (find-file-noselect g--opencl-kernel-fn)))
      (kill-new functor)

      ;; (3) user pastes functor into driver below write_accessor
      (with-current-buffer (find-file-noselect g--oneapi-driver-fn)
        (goto-char (point-min)) (re-search-forward "write_accessor") (end-of-line)
        (insert "\n\n") (yank) (insert "\n\n")
        (save-buffer))

      ;; (4) user positions point in old driver function
      (M g--oneapi-driver-fn)
      (setq pt-in-opencl-driver
            (with-current-buffer (find-file-noselect g--oneapi-driver-fn)
              (goto-char (point-min))
              (re-search-forward "void +tile")
              (forward-line 3) (point)))
      (M pt-in-opencl-driver)
      (M "step 4 complete")

      ;; (5) user generates driver and copies
      (M pt-in-opencl-driver)
      (setq driver   ;; USER MUST cursor inside of opencl driver function
            (g--driver-string
             pt-in-opencl-driver
             (find-file-noselect g--oneapi-driver-fn)))

      ;; (kill-new driver)

      ;; ;; (6) user replaces old driver with new driver (from kill ring)
      ;; (with-current-buffer (find-file-noselect g--oneapi-driver-fn)
      ;;   (goto-char pt-in-opencl-driver)   ;; emulate user
      ;;   (save-excursion
      ;;     (save-restriction
      ;;       (narrow-to-defun)
      ;;       (delete-region (point-min) (point-max))  ;; kill old driver
      ;;       (yank)  ;; paste new driver
      ;;       ))
      ;;   (save-buffer)
      ;;   )

      ;; ;; (7) compile
      ;; ;; (compile "cd ~/tile-af/build && make VERBOSE=1")
      ;; (compile "cd /home/gpryor/tile-af/build/src/backend/oneapi && /opt/intel/oneapi/compiler/2022.2.1/linux/bin/icpx -DAFDLL -DAF_CACHE_KERNELS_TO_DISK -DAF_MKL_INTERFACE_SIZE=4 -DAF_MKL_THREAD_LAYER=2 -DAF_ONEAPI -DAF_WITH_LOGGING -DBOOST_ALL_NO_LIB -DBOOST_CHRONO_HEADER_ONLY -DBOOST_COMPUTE_HAVE_THREAD_LOCAL -DBOOST_COMPUTE_THREAD_SAFE -DOS_LNX -Dafoneapi_EXPORTS -I/home/gpryor/tile-af/include -I/home/gpryor/tile-af/build/include -I/home/gpryor/tile-af/src/backend/oneapi -I/home/gpryor/tile-af/src/api/c -I/home/gpryor/tile-af/src/backend -I/home/gpryor/tile-af/build/src/backend -isystem /home/gpryor/tile-af/build/extern/af_forge-src/include -isystem /home/gpryor/tile-af/build/extern/af_forge-build/include -isystem /home/gpryor/tile-af/build/extern/spdlog-src/include -isystem /home/gpryor/tile-af/build/extern/span-lite-src/include -isystem /home/gpryor/tile-af/build/extern/af_glad-src/include -isystem /home/gpryor/tile-af/extern/half/include -w -g -fPIC -fvisibility=hidden -Wno-unqualified-std-cast-call -Werror=reorder-ctor -fp-model precise -fsycl -Wno-ignored-attributes -Wall -std=c++17 -MD -MT src/backend/oneapi/CMakeFiles/afoneapi.dir/tile.cpp.o -MF CMakeFiles/afoneapi.dir/tile.cpp.o.d -o CMakeFiles/afoneapi.dir/tile.cpp.o -c /home/gpryor/tile-af/src/backend/oneapi/tile.cpp")
      )
  )
