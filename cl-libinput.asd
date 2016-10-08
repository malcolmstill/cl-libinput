;;;; cl-libinput.asd

(asdf:defsystem #:cl-libinput
  :description "Describe cl-libinput here"
  :author "Malcolm Still <malcolm.still@gmail.com>"
  :license "Specify license here"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "cl-libinput")))

