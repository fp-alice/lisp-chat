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
  (let ((packet    (create-packet :connect '((:username "calamity"))))
        (bit-queue (make-instance 'bit-queue)))
    (encode-packet client-packet-registry packet bit-queue)
    (decode-packet client-packet-registry bit-queue)))

(defun test ()
  (let* ((packet    (create-packet :connect '((:username "calamity"))))
         (bit-queue (make-instance 'bit-queue)))
    (encode-packet client-packet-registry packet bit-queue)
    bit-queue))
