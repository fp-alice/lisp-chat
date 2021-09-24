;;;; lispchat.asd

(asdf:defsystem #:lispchat
  :description "Describe lispchat here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:usocket #:bit-smasher)
  :components ((:file "package")
               (:file "lispchat")
               (:file "utils")
               (:file "bits")
               (:file "server")
               (:file "packets")
               (:file "client")))
