;;;; lispchat.lisp

(in-package #:lispchat)

;;;; Packet registries

(defconstant client-packet-registry
  (list
   :connect        '(:fields (:username)
                     :schema (:username :string))
   :direct-message '(:fields (:username :message)
                     :schema (:username :string
                              :message  :string))
   :message        '(:fields (:message)
                     :schema (:message :string))
   :disconnect     '()))

(defconstant server-packet-registry
  (list
   :message
   :terminate))

;;;; Packet functions

;; EXAMPLE (create-packet :connect '((:username "calamity")))
;; NEEDS TO BE A LIST OF LISTS OF FIELDS! CANNOT BE 1D

(defun create-packet (packet-type packet-data)
  (list :packet-id packet-type :data packet-data))

(defun encode-packet-field (packet-schema field-data bit-queue)
  (let ((field-name  (first field-data))
        (field-value (second field-data)))
    (push-generic-queue bit-queue (getf packet-schema field-name) field-value)))

(defun field-encoder (packet-schema bit-queue)
  (lambda (pair) (encode-packet-field packet-schema pair bit-queue)))

(defun encode-packet (registry packet bit-queue)
  (let* ((packet-id            (getf packet :packet-id))
         (packet-data          (getf packet :data))
         (packet-discriminator (position packet-id registry))
         (packet-schema        (getf (getf registry packet-id) :schema)))
    (push-int-queue bit-queue packet-discriminator)
    (print bit-queue)
    (loop for pair in packet-data
          do (funcall (field-encoder packet-schema bit-queue) pair))))

(defun decode-fields (bit-queue packet-schema packet-fields)
  (loop for field in packet-fields
        collect (list field (pop-generic-queue bit-queue (getf packet-schema field)))))

(defun decode-packet (registry bit-queue)
  (let* ((packet-id     (pop-int-queue bit-queue))
         (packet-key    (getf registry (nth packet-id registry)))
         (packet-fields (getf packet-key :fields))
         (packet-schema (getf packet-key :schema))
         (packet-data   (decode-fields bit-queue packet-schema packet-fields)))
    (create-packet (nth packet-id registry) packet-data)))

(defun test2 ()
  (let ((packet    (create-packet :disconnect '()))
        (bit-queue (make-instance 'bit-queue)))
    (encode-packet client-packet-registry packet bit-queue)
    (decode-packet client-packet-registry bit-queue)))

(defun test ()
  (let* ((packet    (create-packet :disconnect '()))
         (bit-queue (make-instance 'bit-queue)))
    (encode-packet client-packet-registry packet bit-queue)
    bit-queue))

(defun create-server (port)
  (let* ((socket (usocket:socket-listen "localhost" port))
         (conn   (usocket:socket-accept socket :element-type 'byte)))
    (unwind-protect
      (progn
        (write-byte 1 (usocket:socket-stream conn))
        (format (usocket:socket-stream conn) "Hello World~%")
        (force-output (usocket:socket-stream conn))
        (progn
          (format t "Closing sockets~%")
          (usocket:socket-close conn)
          (usocket:socket-close socket))))))

(defun bytes->int (stream)
  (let ((bit-queue (make-instance 'bit-queue))
        (sign      (read-byte stream))
        (size      (read-byte stream)))
    (push-queue bit-queue (bit-smasher:bits<- sign))
    (push-queue bit-queue (bit-smasher:bits<- size))
    (dotimes (_ size)  (push-queue bit-queue (bit-smasher:bits<- (read-byte stream))))
    (pop-int-queue bit-queue)))

(defun bytes->string (stream)
  (let ((size (bytes->int stream)))
    (with-output-to-string (str)
      (dotimes (_ size)
        (princ (code-char (bytes->int stream)) str)))))

(defun read-type-from-stream (type-key stream)
  (case type-key
    (:int    (bytes->int stream))
    (:string (bytes->string stream))))

(defun read-packet-field (stream schema)
  (lambda (field)
    (let ((field-type (getf schema field)))
      (list field (read-type-from-stream field-type stream)))))

(defun read-packet (registry stream)
  (let* ((packet-id   (read-type-from-stream :int stream))
         (packet-name (nth packet-id registry))
         (packet-info (getf registry packet-name))
         (fields      (getf packet-info :fields))
         (schema      (getf packet-info :schema))
         (packet-data (map 'list (read-packet-field stream schema) fields)))
    (create-packet packet-name packet-data)))

(defun create-echo-server (port)
  (usocket:with-socket-listener (socket "localhost" port)
    (usocket:wait-for-input socket)
    (usocket:with-connected-socket (conn (usocket:socket-accept socket :element-type '(unsigned-byte 8)))
      (let ((stream (usocket:socket-stream conn)))
        (read-packet client-packet-registry stream)))))

(defun test-server (port)
  (let ((bq (make-instance 'bit-queue)))
    (encode-packet client-packet-registry (create-packet :connect '((:username "calamity"))) bq)
    (usocket:with-client-socket (socket stream "localhost" port :element-type '(unsigned-byte 8))
      (loop for b in (queue->bytes bq)
            do (write-byte b stream)))))
