;;;; lispchat.lisp

(in-package #:lispchat)

(defun test-server (port)
  (let ((bq (make-instance 'bit-queue)))
    (encode-packet client-packet-registry (create-packet :connect '((:username "calamity"))) bq)
    (usocket:with-client-socket (socket stream "localhost" port :element-type '(unsigned-byte 8))
      (loop
        (loop for b in (queue->bytes bq)
            do (write-byte b stream))
        (force-output stream)))))
