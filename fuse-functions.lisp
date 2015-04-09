; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(in-package :cl-fuse)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro file-here (name &optional type)
            (make-pathname :name name
			   :type type
                           :directory (pathname-directory
                                       (or
                                        *compile-file-pathname*
                                        *load-pathname*
                                        ))
                           :device (pathname-device
                                    (or
                                     *compile-file-pathname*
                                     *load-pathname*
                                     ))
                           :host (pathname-host
                                  (or
                                   *compile-file-pathname*
                                   *load-pathname*
                                   ))
                           ))
  )

(cffi:define-foreign-type cffi-string-latin-1 (cffi::foreign-string-type)
  () (:default-initargs :encoding :latin-1)
  (:simple-parser cffi-string-latin-1))

(define-foreign-library libfuse
  (:darwin (:or "libosxfuse.dylib" "libosxfuse"))
  (:linux (:or "libfuse.so.2" "libfuse.so"))
  (:unix (:or "libfuse.so.2" "libfuse.so"))
  (t (:default "libfuse")))

(define-foreign-library
  fuse-launcher
  (t (:or
       #.(namestring (file-here "libfuse-launcher" "so"))
       #+ccl #.(ccl:native-translated-namestring (file-here "libfuse-launcher"
                                                            #+darwin "dylib"
                                                            #+linux "so"))
       "libfuse-launcher"
       #.(file-here "libfuse-launcher")
       #.(file-here "libfuse-launcher" #+darwin "dylib" #+linux "so")
       #.(namestring (file-here "libfuse-launcher"))
       )))

(use-foreign-library libfuse)
(use-foreign-library fuse-launcher)

; fuse_common.h : struct fuse_chan *fuse_mount(const char *mountpoint, struct fuse_args *args)
(defcfun "fuse_mount" :pointer
  (mountpoint :pointer)
  (args (:pointer (:struct fuse-args))))

; fuse_common.h : int fuse_parse_cmdline(
;                                   struct fuse_args *args,
;                                   char **	mountpoint,
;                                   int *  	multithreaded,
;                                   int *  	foreground
;                                   )
(defcfun "fuse_parse_cmdline" :int
  (args (:pointer (:struct fuse-args)))
  (mountpoint :pointer)
  (multithreaded :pointer)
  (foreground :pointer))

; fuse_opt.h : int fuse_opt_add_arg(struct fuse_args *args, const char *arg)
(defcfun "fuse_opt_add_arg" :int
  (args (:pointer (:struct fuse-args)))
  (arg :pointer))

; fuse.h : struct fuse* fuse_new (
;                                 struct fuse_chan *ch,
;                                 struct fuse_args *args,
;                                 const struct fuse_operations *op,
;                                 size_t  	op_size,
;                                 void *  	user_data
;                                )
(defcfun "fuse_new" (:pointer (:struct fuse-data))
  (ch :pointer)
  (args (:pointer (:struct fuse-args)))
  (op (:pointer (:struct fuse-ops)))
  (op_size size)
  (user_data :pointer))

; [fuse-launcher.c]
; struct fuse *fuse_new_proxy(struct fuse_chan *ch, struct fuse_args *args,
;                      const struct fuse_operations *op, size_t op_size,
;                      void *user_data)
(defcfun "fuse_new_proxy" (:pointer (:struct fuse-data))
  (ch :pointer)
  (args (:pointer (:struct fuse-args)))
  (op (:pointer (:struct fuse-ops)))
  (op_size size)
  (user_data :pointer))

; fuse.h : void fuse_destroy(struct fuse *f)
(defcfun "fuse_destroy" :void
  (f (:pointer (:struct fuse-data))))

; fuse_common.h : void fuse_unmount(const char *mountpoint, struct fuse_chan *ch)
(defcfun "fuse_unmount" :void
  (mountpoint :pointer)
  (ch :pointer))

; fuse.h : struct fuse_session *fuse_get_session(struct fuse *f)
(defcfun "fuse_get_session" (:pointer (:struct fuse-session))
  (f (:pointer (:struct fuse-data))))

; fuse_lowlevel.h : size_t fuse_chan_bufsize(struct fuse_chan *ch)
(defcfun "fuse_chan_bufsize" size
  (ch :pointer))

; fuse_lowlevel.h : int fuse_session_exited(struct fuse_session *se)
(defcfun "fuse_session_exited" :int
  (se (:pointer (:struct fuse-session))))

; fuse_lowlevel.h : void fuse_session_exit(struct fuse_session *se)
(defcfun "fuse_session_exit" :void
  (se :pointer))

; fuse_lowlevel.h : int fuse_chan_recv(struct fuse_chan **ch, char *buf, size_t size)
(defcfun "fuse_chan_recv" :int
  (ch :pointer)
  (buf :pointer)
  (size size))

; fuse_lowlevel.h : void fuse_session_process(struct fuse_session *se,
;                                             const char *buf, size_t len,
;                                             struct fuse_chan *ch)
(defcfun "fuse_session_process" :void
  (se (:pointer (:struct fuse-session)))
  (buf :pointer)
  (len size)
  (ch :pointer))

; fuse_lowlevel.h : struct fuse_session *fuse_lowlevel_new(struct fuse_args *args,
;                                                          const struct fuse_lowlevel_ops *op,
;                                                          size_t op_size, void *userdata)
(defcfun "fuse_lowlevel_new" :pointer
  (args (:pointer (:struct fuse-args)))
  (op :pointer)
  (op-size size)
  (userdata :pointer)
  )

; fuse_lowlevel.h : void fuse_session_add_chan(struct fuse_session *se, struct fuse_chan *ch)
(defcfun "fuse_session_add_chan" :void
  (se (:pointer (:struct fuse-session)))
  (ch :pointer))

; fuse.h : struct fuse *fuse_setup(int argc, char *argv[],
;                                  const struct fuse_operations *op, size_t op_size,
;                                  char **mountpoint, int *multithreaded,
;                                  void *user_data)
(defcfun "fuse_setup" (:pointer (:struct fuse-data))
  (argc :int)
  (argv (:pointer :string))
  (op (:pointer (:struct fuse-ops)))
  (op-size size)
  (mountpoint (:pointer (:pointer)))
  (multithreaded :pointer)
  (user-data :pointer))

; fuse_lowlevel.h : struct fuse_chan *fuse_session_next_chan(struct fuse_session *se,
;                                                            struct fuse_chan *ch)
(defcfun "fuse_session_next_chan" :pointer
  (se (:pointer (:struct fuse-session)))
  (ch :pointer))

; fuse_opt.h : void fuse_opt_free_args(struct fuse_args *args)
(defcfun "fuse_opt_free_args" :void
  (args (:pointer (:struct fuse-args))))


; fuse.h : struct fuse_context *fuse_get_context(void);
(defcfun "fuse_get_context" (:pointer (:struct fuse-ops)))

(defun fuse-main-lisp (operations args &optional
  (call-manager (lambda (f &rest x)
                        (declare (ignore x))
                        (funcall f))))
  (let* (
         (operation-list (foreign-alloc '(:struct fuse-ops)))
         (args-fuse (foreign-alloc '(:struct fuse-args)))
         (chan-fuse nil)
         (data-fuse nil)
         (session nil)
         (bufsize nil)
         (buf-fuse nil)
         (foreground (foreign-alloc :int))
         (multithreaded (foreign-alloc :int))
         (mountpoint (foreign-alloc :pointer))
         (fuse-ops-size (foreign-type-size '(:struct fuse-ops)))
         (argc (length args))
         (argv (foreign-alloc :pointer :count (+ 1 argc)))
         (failure nil)
         (dummy nil)
         )
        (loop for i from 0 upto (- fuse-ops-size 1) do
              (setf (mem-aref operation-list :uint8 i) 0))
        (loop for i in operations do
              (setf (foreign-slot-value operation-list '(:struct fuse-ops) (car i))
                    (get-callback (cadr i))))
        (loop for i from 0 upto (- argc 1) do
              (setf (mem-aref argv :pointer i)
                    (foreign-string-alloc (nth i args))))
        (setf (mem-aref argv :pointer argc) (null-pointer))
        (setf (foreign-slot-value args-fuse '(:struct fuse-args) 'argc) argc
              (foreign-slot-value args-fuse '(:struct fuse-args) 'argv) argv
              (foreign-slot-value args-fuse '(:struct fuse-args) 'allocated) 1
              )
        (setf dummy (fuse-parse-cmdline args-fuse
                                        mountpoint
                                        multithreaded
                                        foreground))
        (setf chan-fuse (fuse-mount (mem-ref mountpoint :pointer)
                                    args-fuse))
        (when (= (pointer-address chan-fuse) 0)
              (return-from fuse-main-lisp nil)
              )
        (setf data-fuse
              (fuse-new-proxy chan-fuse
                               args-fuse
                               operation-list
                               (foreign-type-size '(:struct fuse-ops))
                               (null-pointer)
                               ))
        (when (= (pointer-address data-fuse) 0)
              (fuse-unmount (mem-ref mountpoint :pointer) chan-fuse)
              (return-from fuse-main-lisp nil)
              )

        (fuse-opt-free-args args-fuse)

        (setf session (fuse-get-session data-fuse))
        (setf chan-fuse (fuse-session-next-chan session (null-pointer)))
        (setf bufsize (fuse-chan-bufsize chan-fuse))

        (loop until (or failure (/= (fuse-session-exited session) 0))
              do (let* (
                        (tmp-chan (foreign-alloc :pointer))
                        (dummy (setf (mem-ref tmp-chan :pointer)
                                     chan-fuse))
                        (buf-fuse (foreign-alloc :char :count bufsize))
                        (read-success
                         (fuse-chan-recv tmp-chan buf-fuse bufsize))
                        )
                       (declare (ignore dummy))
                       (cond
                        ((= read-success (- error-EINTR)) nil)
                        ((= read-success 0) nil)
                        ((< read-success 0) (setf failure t))
                        (t (funcall
                            call-manager
                            (lambda ()
                                    (fuse-session-process
                                     session buf-fuse read-success
                                     (mem-ref tmp-chan :pointer))
                                    (when buf-fuse (foreign-free buf-fuse))
                                    (when tmp-chan (foreign-free tmp-chan))
                                    )
                            ))
                        )
                       ))

        (fuse-unmount (mem-ref mountpoint :pointer) chan-fuse)
        (unless (= (pointer-address data-fuse) 0)
                (fuse-destroy data-fuse))
        (mapcar 'foreign-free
                (list
                 operation-list args-fuse foreground
                 multithreaded mountpoint))
        ))
