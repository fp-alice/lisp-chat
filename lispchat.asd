;;;; lispchat.asd

(asdf:defsystem #:lispchat
  :description "Describe lispchat here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:usocket #:lisp-binary)
  :components ((:file "package")
               (:file "lispchat")))
