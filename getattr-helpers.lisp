; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(in-package :cl-fuse)

(defcfun "getuid" :int)
(defcfun "getgid" :int)

(defun generic-file (content)
  ;(fuse-complain "mode was ~s" (foreign-slot-value content '(:struct stat-data) :mode))
  ;(fuse-complain "inode was ~s" (foreign-slot-value content '(:struct stat-data) :inode))
  (setf
   (foreign-slot-value content '(:struct stat-data) :on-dev) 0
   ;(foreign-slot-value content '(:struct stat-data) :inode) 0
   (foreign-slot-value content '(:struct stat-data) :mode) #o0555
   (foreign-slot-value content '(:struct stat-data) :link-count) 1
   (foreign-slot-value content '(:struct stat-data) :uid) (getuid)
   (foreign-slot-value content '(:struct stat-data) :gid) (getgid)
   (foreign-slot-value content '(:struct stat-data) :device-id) 0
   (foreign-slot-value content '(:struct stat-data) :size) 0
   (foreign-slot-value content '(:struct stat-data) :blocksize) 0
   (foreign-slot-value content '(:struct stat-data) :blockcount) 0
   (foreign-slot-value content '(:struct stat-data) :access-time) 0
   (foreign-slot-value content '(:struct stat-data) :modification-time) 0
   (foreign-slot-value content '(:struct stat-data) :change-time) 0
   )
  )

(defun writeable-file (content)
  (setf
   (foreign-slot-value content '(:struct stat-data) :mode)
   (boole
    boole-ior
    #o200
    (foreign-slot-value content '(:struct stat-data) :mode)
    )))

(defun executable-file (content)
  (setf
   (foreign-slot-value content '(:struct stat-data) :mode)
   (boole
    boole-ior
    #o100
    (foreign-slot-value content '(:struct stat-data) :mode)
    )))

(defun generic-directory (content)
  (generic-file content)
  (setf
   (foreign-slot-value content '(:struct stat-data) :mode) (logior mode-directory #o0555)))

(defun generic-symlink (content)
  (generic-file content)
  (setf
   (foreign-slot-value content '(:struct stat-data) :mode) (logior mode-link #o0555)))

(defun generic-plain-file (content size)
  (generic-file content)
  (setf (foreign-slot-value content '(:struct stat-data) :mode)
        (logior mode-regular #o0444))
  (setf (foreign-slot-value content '(:struct stat-data) :size) size))
