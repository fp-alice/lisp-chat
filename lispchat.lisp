;;;; lispchat.lisp

(in-package #:lispchat)

(defconstant bit-queue (make-instance 'bit-queue))


(defun send-client-packet ())

(defun send-connect (stream)
  (send-packet stream :connect '((:username "calamity"))))

(defun send-disconnect (stream)
  (send-packet stream :disconnect '()))

(defun send-message (stream message)
  (let ((body (list :message message)))
   (send-packet stream :message (list body))))

(defun test-server (port)
  (usocket:with-client-socket (socket stream "localhost" port :element-type '(unsigned-byte 8))
    (initiate-connection stream)
    (loop
      (let ((input (read-line)))
        (if (string= input ":quit")
          (send-disconnect stream)
          (send-message stream input))))))
