;;;; lispchat.lisp

(in-package #:lispchat)

;;;; Encoding and decoding

;; Recursive function to collect the bits of the integer one-by-one
(defun recurse-collect-bits (integer carry)
    (if (zerop integer)
      (if carry
          carry                           ;; Return carry if int has been consumed and carry is not nil
          '(0))                           ;; Return 0 if empty carry
      (recurse-collect-bits               ;; Otherwise call the function again
       (ash integer -1)                   ;; Right-shift the integer by one
       (cons (logand integer 1) carry)))) ;; Cons this bit to the carry list

;; Convert an arbitrary integer into a sequence of bytes
(defun int-to-bits (i)
  (recurse-collect-bits (abs i) '())) ;; Get bits from the absolute value of input

;; Fills a list with an item using the difference between target-length and current-length
(defun create-pad (target-length current-length pad)
  (make-list (- target-length current-length) :initial-element pad))

;; Pad a list to a desired length with a desired pad item on the left
(defun pad-list-left (desired-length pad input)
  (let ((input-length (length input)))
    (if (<= desired-length input-length)    ;; If the desired length is >= actual length
      input                                 ;; Return the input
      (concatenate                          ;; Otherwise, create a pad list and concatenate on the left
       'list
       (create-pad desired-length input-length pad)
       input))))

;; Like pad-list-left, but pads on the right instead
(defun pad-list-right (desired-length pad input)
  (let ((input-length (length input)))
    (if (<= desired-length input-length)
      input
      (concatenate
       'list
       input
       (create-pad desired-length input-length pad)))))

;; Get the next number after current divisible by n
(defun next-divisible (current n)
  (+ current (- n (rem current n))))

;; Encode an integer of arbitrary length (up to 255 bits)
(defun encode-arbitrary-int (integer)
  (let* ((bit-length-as-bits (int-to-bits (integer-length integer))) ;; Get the bit length
         (padded-bit-length  (pad-list-left 8 0 bit-length-as-bits)) ;; Pad it to 8 to standardize size
         (sign               (if (< 0 integer) 1 0))                 ;; Determine sign-bit
         (integer-bits       (int-to-bits integer)))                 ;; Get the bits of the number
    (concatenate 'list padded-bit-length (list sign) integer-bits))) ;; Concatenate length as bits and real bits

;; Recursively turn a list of bits into an integer
(defun recurse-decode-int (len bits collector)
  (if (zerop len)                               ;; If length is zero
    collector                                   ;; Return collector, else
    (recurse-decode-int                         ;; Recurse
     (- len 1)                                  ;; Subtracting len by 1
     (rest bits)                                ;; With only the tail of the bit list
     (logior (ash collector 1) (first bits))))) ;; And the result of (collector << 1) | (first bits)

;; One-argument wrapper for recurse-decode-int
(defun decode-int (bits)
  (recurse-decode-int (length bits) bits 0))

;; Decode an encoded integer of arbitrary length
(defun decode-arbitrary-int (bits)
  (let ((int-length (decode-int (subseq bits 0 8))))                 ;; Extract length of encoded int
    (if (zerop int-length)                                           ;; If the length of the encoded int is zero
      0                                                              ;; Return early
      (let ((sign (if (equalp 0 (nth 9 bits)) -1 1))                 ;; Otherwise, extract the sign bit
            (integer (decode-int (subseq bits 9 (+ 9 int-length))))) ;; Extract the integer bits and decode
        (* sign integer)))))                                         ;; Multiply the decoded int by 1 or -1

;; Encode a string as a list of integers
(defun encode-string (string)
  (mapcan (lambda (c) (encode-arbitrary-int (char-code c))) (coerce string 'list)))

;; Recursively consume integers from bits and convert them to characters collected in coll
(defun recurse-decode-string (bits coll)
  (if bits
    (let* ((first-int-length (decode-int (subseq bits 0 8)))                         ;; Length of the first int
           (first-int-bits   (+ 9 first-int-length))                                 ;; Total bit length of the first int
           (first-int        (decode-arbitrary-int (subseq bits 0 first-int-bits)))  ;; Decoded first int
           (remaining-bits   (subseq bits first-int-bits (length bits))))            ;; Bits remaining after decoding
      (decs remaining-bits (concatenate 'string coll (list (code-char first-int))))) ;; Recurse, collecting char
    coll))                                                                           ;; Return collected string

;; Decode string from list of bits
(defun decode-string (bits)
  (recurse-decode-string bits ""))

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
   (concatenate 'list (pad-list-left 8 0 (int-to-bits packet-discriminator)) encoded-packet)))

(defun decode-packet (bits registry)
  (let* ((packet-id  (decode-arbitrary-int (subseq bits 0 9)))
         (packet-key (getf registry (nth packet-id registry))))
    (print packet-key)))
  
(defun recurse-partition (list len coll)
  (if list
    (recurse-partition
     (nthcdr len list)
     len
     (cons (subseq list 0 len) coll))
    (reverse coll)))

(defun partition (list len)
  (recurse-partition list len '()))

(defun bits->bytes (bits)
  (let ((byte-bit-lists (partition bits 8)))
    (map 'list #'decode-int byte-bit-lists)))

(defun pad-bits-and-make-bytes (bits)
  (let* ((pad-length  (next-divisible (length bits) 8))
         (padded-bits (pad-list-right pad-length 0 bits))
         (partitioned (partition padded-bits 8)))
    (mapcan #'bits->bytes partitioned)))

(defun bytes->bits (bytes)
  (let ((bits-list (map 'list #'int-to-bits bytes)))
    (mapcan (lambda (bits) (pad-list-left 8 0 bits)) bits-list)))

(defun test ()
  (let* ((packet  (create-packet :connect '((:username "calamity"))))
         (encoded (encode-packet packet client-packet-registry))
         (bytes   (pad-bits-and-make-bytes encoded)))
    (print encoded)
    (print (bytes->bits bytes))))
