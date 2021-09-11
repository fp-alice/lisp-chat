;;;; lispchat.lisp

(in-package #:lispchat)

;;;; Encoding and decoding

;; Recursive function to collect the bits of the integer one-by-one
(defun recurse-collect-bits (integer carry)
    (if (equalp integer 0)
      carry                               ;; Return our collected list if we've read all bits
      (recurse-collect-bits               ;; Otherwise call the function again
       (ash integer -1)                   ;; Right-shift the integer by one
       (cons (logand integer 1) carry)))) ;; Cons this bit to the carry list

;; Convert an arbitrary integer into a sequence of bytes
(defun int-to-bits (i)
  (let ((sign (if (> i 0) 1 0))                    ;; Reserve one bit to denote positive/negative
        (bits (recurse-collect-bits (abs i) '()))) ;; Get bits from the absolute value of input
    (cons sign bits)))                             ;; Prepend sign to bits

;; Recursively pad a list to a desired length with a desired pad item
(defun pad-list (desired-length pad collector)
  (if (>= desired-length (length collector))             ;; If the desired length is >= actual length
    collector                                            ;; Return the collector
    (pad-list desired-length pad (cons pad collector)))) ;; Else pad and recurse

;; Encode an integer of arbitrary length (up to 255 bits)
(defun encode-arbitrary-int (integer)
  (let* ((bit-length-as-bits (int-to-bits (integer-length integer))) ;; Get the bit length
         (padded-bit-length  (pad-list 8 0 bit-length-as-bits))      ;; Pad it to 8 to standardize size
         (integer-bits       (int-to-bits integer)))                 ;; Get the bits of the number
    (concatenate 'list padded-bit-length integer-bits)))             ;; Concatenate length as bits and real bits

;; Recursively turn a list of bits into an integer
(defun recurse-decode-int (len bits collector)
  (if (equalp len 0)                             ;; If length is zero
    collector                                    ;; Return collector, else
    (recurse-decode-int                          ;; Recurse
     (- len 1)                                   ;; Subtracting len by 1
     (rest bits)                                 ;; With only the tail of the bit list
     (logior (ash collector 1) (first bits)))))  ;; And the result of (collector << 1) | (first bits)

;; One-argument wrapper for recurse-decode-int
(defun decode-int (bits)
  (recurse-decode-int (length bits) bits 0))

;; Decode an encoded integer of arbitrary length
(defun decode-arbitrary-int (bits)
  (if (equalp (decode-int (subseq bits 0 8)) 0)                 ;; If the length of the encoded int is zero
    0                                                           ;; Return early
    (let ((sign (if (equalp 0 (nth 9 bits)) -1 1))              ;; Otherwise, extract the sign bit
          (integer (decode-int (subseq bits 9 (length bits))))) ;; Extract the integer bits and decode
      (* sign integer))))                                       ;; Multiply the decoded int by 1 or -1

;; Encode a string as a list of integers
(defun encode-string (string)
  (map
   'list
   (lambda (c) (encode-arbitrary-int (char-code c)))
   string))

;; Decode a string
(defun decode-string (string-bits)
  (map
   'string
   (lambda (bits) (code-char (decode-arbitrary-int bits)))
   string-bits))

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

;;;; Generic encoding & decoding

(defun generic-encode (type-key data)
  (case type-key
    (:string (encode-string data))
    (:int    (encode-arbitrary-int data))))

(defun generic-decode (type-key data)
  (case type-key
    (:string (decode-string data))
    (:int    (decode-arbitrary-int data))))

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
   (concatenate 'list (encode-arbitrary-int packet-discriminator) encoded-packet)))
