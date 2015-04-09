; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

; example FS
; Does nothing of interest
; Supports trivial directory listings and
; symlink generation

(require 'cl-fuse)
(require 'cl-utilities)
(require 'bordeaux-threads)

(use-package 'cl-fuse)
;(use-package 'cl-utilities)

(defun is-root (split-path)
  (or (null split-path) (equalp split-path "")) )

(defun is-directory (split-path)
  (or
   (is-root split-path)
   (and
    (or
     (equalp (car split-path) "symlinks")
     (equalp (car split-path) "same-name")
     (equalp (car split-path) "many-files")
     )
    (null (cdr split-path))
  )
))

(defun symlink-target (split-path)
  (cond
    ((equalp (car split-path) "symlinks") (cdr split-path))
    ((equalp (car split-path) "many-files") (cdr split-path))
    (t nil)
  )
)

(defun directory-content (split-path)
  (cond
    ((or
      (null (car split-path))
      (equalp "" (car split-path)))
     '("symlinks" "same-name" "many-files"))
    ((and
      (equalp "many-files" (car split-path))
      (null (cdr split-path)))
     (loop for i from 0 upto 100 collect (format nil "~a" i)))
    (t nil)
    )
)

(defun file-size (split-path)
  (cond
    ((and (equalp (car split-path) "same-name") (null (cddr split-path)))
     (+ (length (cadr split-path))
        (length "same-name/")))
    (t nil)))

(defun file-read (split-path size offset fh)
  (declare (ignore fh))
  (declare (ignore size))
  (declare (ignore offset))
  (let* (
         (name (cadr split-path))
         )
    #+nil `(:offset 1 ,name)
    (concatenate 'string (car split-path) "/" name)))

(defun fuse-test ()
  (fuse-run '("none" "/tmp/test" "-d"
                     )
            :directoryp 'is-directory
            :directory-content 'directory-content
            :symlink-target 'symlink-target
            :file-size 'file-size
            :file-read 'file-read
            ))

(defun fuse-test-par ()
  (fuse-run '("none" "/tmp/test"
                     )
            :directoryp 'is-directory
            :directory-content 'directory-content
            :symlink-target 'symlink-target
            :file-size 'file-size
            :file-read 'file-read
            :call-manager
            (lambda (f &rest x)
              (declare (ignore x))
              (bordeaux-threads:make-thread f))
            ))

;(if (boundp 'common-lisp-user::*parallel-fuse-test*) (fuse-test-par) (fuse-test))
