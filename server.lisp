;;;; server.lisp

(in-package #:lispchat)

(defvar *connections* (make-hash-table))

(defun for-client (func)
  (loop for client being the hash-keys of *connections*
        do (funcall func client)))

(defun send-server-packet ())

(defconstant output-queue (make-instance 'bit-queue))

(defun free-connection (client username)
  (format t "Disconnecting ~S~%" username)
  (usocket:socket-close client)
  (remhash client *connections*))

(defun accept-incoming-connections (socket)
  (loop for ready in (usocket:wait-for-input socket :ready-only t :timeout 0)
        when ready
        do (let ((new-connection (usocket:socket-accept ready :element-type '(unsigned-byte 8))))
             (setf (gethash new-connection *connections*) "new user"))))

(defun handle-new-connection (socket packet-data)
  (let ((username (getf packet-data :username)))
    (format t "User connected with username ~S~%" username)
    (setf (gethash socket *connections*) username)))

(defun handle-message (username packet-data)
  (let ((message (getf packet-data :message)))
    (format t "User ~S sent message ~S~%" username message)
    (for-client (lambda (client) ))))

(defun accept-packet (client username)
  (let* ((packet      (read-packet client-packet-registry (usocket:socket-stream client)))
         (packet-id   (getf packet :packet-id))
         (packet-data (getf packet :data)))
    (format t "Accepted :~a from user ~S~%" packet-id username)
    (case packet-id
      (:connect    (handle-new-connection client packet-data))
      (:message    (handle-message username packet-data))
      (:disconnect (free-connection client username)))))

(defun process-input (client username)
  (handler-case (accept-packet client username)
    (end-of-file ()
      (format t "Caught EOF condition for user ~S~%" username)
      (free-connection client username))))

(defun create-server (port)
  (usocket:with-socket-listener (socket "localhost" port :reuse-address t)
    (unwind-protect
      (loop
        (accept-incoming-connections socket)
        (maphash #'process-input *connections*))
      (maphash #'free-connection *connections*))))
