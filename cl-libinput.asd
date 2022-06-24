;;;; cl-libinput.asd

(asdf:defsystem #:cl-libinput
  :description "Common Lisp wrapper for libinput"
  :author "Malcolm Still <malcolm.still@gmail.com>"
  :license "BSD 3-Clause"
  :depends-on (#:cffi #:osicat)
  :serial t
  :components ((:file "package")
               (:file "cl-libinput")))

