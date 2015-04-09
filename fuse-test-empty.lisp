; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(require 'cl-fuse)

(defun is-directory (split-path)
  t)

(cl-fuse:fuse-run '("none" "-d" "/tmp/test")
                  :directoryp 'is-directory
                  )
