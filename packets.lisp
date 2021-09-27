;;;; packets.lisp

(in-package #:lispchat)

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
   :message   '(:fields (:username :message)
                :schema (:username :string
                         :message  :string))
   :terminate '()))

;; EXAMPLE (create-packet :connect '((:username "calamity")))
;; NEEDS TO BE A LIST OF LISTS OF FIELDS! CANNOT BE 1D
;; Creates a well-formed packet from an ID and a body
(defun create-packet (packet-type packet-data)
  (list :packet-id packet-type :data packet-data))

;; Encodes a single field of a packet into a bit queue
(defun encode-packet-field (packet-schema field-data bit-queue)
  (let ((field-name  (first field-data))
        (field-value (second field-data)))
    (push-generic-queue bit-queue (getf packet-schema field-name) field-value)))

;; Returns a function that encodes a field in a schema
(defun field-encoder (packet-schema bit-queue)
  (lambda (pair) (encode-packet-field packet-schema pair bit-queue)))

;; Converts a packet into bits and adds them to a bit queue
(defun encode-packet (registry packet bit-queue)
  (let* ((packet-id            (getf packet :packet-id))
         (packet-data          (getf packet :data))
         (packet-discriminator (position packet-id registry))
         (packet-schema        (getf (getf registry packet-id) :schema)))
    (push-int-queue bit-queue packet-discriminator)
    (loop for pair in packet-data
          do (funcall (field-encoder packet-schema bit-queue) pair))))

;; Consumes bytes from a stream and converts them into an integer
(defun bytes->int (stream)
  (let ((bit-queue (make-instance 'bit-queue))                                         ;; Create a new bit-queue (reusing my encoding for decoding)
        (sign      (read-byte stream))                                                 ;; Sign byte
        (size      (read-byte stream)))                                                ;; Size byte
    (push-queue bit-queue (bit-smasher:bits<- sign))                                   ;; Push sign bits into queue
    (push-queue bit-queue (bit-smasher:bits<- size))                                   ;; Push size bits into queue
    (dotimes (_ size)  (push-queue bit-queue (bit-smasher:bits<- (read-byte stream)))) ;; Push remainder of integer into queue
    (pop-int-queue bit-queue)))                                                        ;; Pop and return integer value

;; Consumes bytes from a stream and converts them into a string
(defun bytes->string (stream)
  (let ((size (bytes->int stream)))
    (with-output-to-string (str)
      (dotimes (_ size)
        (princ (code-char (bytes->int stream)) str)))))

;; Consumes data from the stream using different functions depending on the value of type-key
(defun read-type-from-stream (type-key stream)
  (case type-key
    (:int    (bytes->int stream))
    (:string (bytes->string stream))))

;; Returns a function that reads fields from a stream referring to a provided schema
(defun read-packet-field (stream schema)
  (lambda (field)                                                    ;; Target field
    (let ((field-type (getf schema field)))                     ;; Type of target field
      (list field (read-type-from-stream field-type stream))))) ;; Field name and value as a pair

;; Read packet from a stream using a registry
(defun read-packet (registry stream)
  (let* ((packet-id   (read-type-from-stream :int stream))                   ;; Read the packet ID
         (packet-name (nth packet-id registry))                              ;; Get packet name from registry
         (packet-info (getf registry packet-name))                           ;; Get the packet information from registry
         (fields      (getf packet-info :fields))                            ;; Fields in packet
         (schema      (getf packet-info :schema))                            ;; Field types
         (packet-data (mapcan (read-packet-field stream schema) fields)))    ;; Read each field and associate it with its name
    (create-packet packet-name packet-data)))                                ;; Assemble a fully formed packet to return

(defun write-queue-to-stream (queue stream)
  (loop for b in (queue->bytes queue)
        do (write-byte b stream)
        finally (force-output stream)))

(defun write-packet-to-queue (queue registry packet-id fields)
  (encode-packet registry (create-packet packet-id fields) queue))

(defun send-packet (queue stream registry packet-id fields)
  (write-packet-to-queue queue registry packet-id fields)
  (write-queue-to-stream queue stream))
