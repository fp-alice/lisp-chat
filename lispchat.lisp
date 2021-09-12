;;;; lispchat.lisp

(in-package #:lispchat)

;;;; Packet registries

(defconstant client-packet-registry
  (list
   :connect        '(:username :string)
   :direct-message '(:username :string
                     :message :string)
   :message        '(:message :string)
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

(defun encode-packet-field (packet-schema field-data)
  (let* ((field-name  (first field-data))
         (field-value (second field-data))
         (field-type  (getf packet-schema field-name)))
    (generic-encode field-type field-value)))

(defun field-encoder (packet-schema)
  (lambda (fields)
    (encode-packet-field packet-schema fields)))

(defun encode-packet (packet registry)
  (let* ((packet-id            (getf packet :packet-id))
         (packet-data          (getf packet :data))
         (packet-discriminator (position packet-id registry))
         (packet-schema        (getf registry packet-id))
         (encoded-packet       (mapcan (field-encoder packet-schema) packet-data)))
   (concatenate 'list (pad-list-left 8 0 (int->bits packet-discriminator)) encoded-packet)))

(defun decode-packet (bits registry)
  (let* ((packet-id  (decode-arbitrary-int (subseq bits 0 9)))
         (packet-key (getf registry (nth packet-id registry))))
    (print packet-key)))


(defun test ()
  (let* ((packet  (create-packet :connect '((:username "calamity"))))
         (encoded (encode-packet packet client-packet-registry))
         (bytes   (pad-bits-and-make-bytes encoded)))
    (print encoded)
    (print (bytes->bits bytes))))
