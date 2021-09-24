;;;; server.lisp

(in-package #:lispchat)

;; (defun create-server (port)
;;   (usocket:with-socket-listener (socket "localhost" port)
;;     (usocket:wait-for-input socket)
;;     (usocket:with-connected-socket (conn (usocket:socket-accept socket :element-type '(unsigned-byte 8)))
;;       (let ((stream (usocket:socket-stream conn)))
;;         (read-packet client-packet-registry stream))

(defvar *connections* (make-hash-table))

(defun get-incoming-connections (socket)
  (loop for ready in (usocket:wait-for-input socket :ready-only t :timeout 0)
        when ready
        do (let ((new-connection (usocket:socket-accept ready :element-type '(unsigned-byte 8))))
             (setf (gethash new-connection *connections*) "new-user"))))

(defun handle-new-connection (socket packet-data)
  (let ((username (getf packet-data :username)))
    (format t "User connected with username %s" username)
    (setf (gethash socket *connections*) username)))

(defun process-input (client username)
  (let ((socket (first (usocket:wait-for-input client :ready-only t :timeout 0))))
    (when socket
      (format t "Accepting packet from %s" username)
      (let* ((packet      (read-packet client-packet-registry (usocket:socket-stream socket)))
             (packet-id   (getf packet :packet-id))
             (packet-data (getf packet :data)))
        (case packet-id
          (:connect (handle-new-connection socket packet-data)))))))


(defun free-connection (client username)
  (format t "Disconnecting %s" username)
  (usocket:socket-close client)
  (remhash client *connections*))

(defun create-server (port)
  (usocket:with-socket-listener (socket "localhost" port :reuse-address t)
    (unwind-protect
      (loop
        (get-incoming-connections socket)
        (maphash #'process-input *connections*))
      (maphash #'free-connection *connections*))))
