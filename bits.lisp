;;;; bits.lisp

(in-package #:lispchat)

;;;; Encoding and decoding integers

;; This encoding probably makes some people shudder...
;; But it was my best effort and I referred to almost no examples in coming up with it!

;; Recursive function to collect the bits of the integer one-by-one
(defun recurse-collect-bits (integer carry)
    (if (zerop integer)                   ;; If integer is zero it's time to return
      (if carry                           ;; But carry is sometimes nil! so,
          carry                           ;; Return carry if int has been consumed and carry is not nil
          '(0))                           ;; Othwerise, return '(0) if carry is nil
      (recurse-collect-bits               ;; If it isn't time to return, call the function again
       (ash integer -1)                   ;; Right-shift the integer by one
       (cons (logand integer 1) carry)))) ;; And cons this bit to the carry list

;; Convert an arbitrary integer into a sequence of bytes without padding to byte-width
(defun int->bits (i)
  (recurse-collect-bits (abs i) '())) ;; Get bits from the absolute value of input

;; Recursively turn a list of bits into an integer
(defun recurse-decode-int (len bits collector)
  (if (zerop len)                               ;; If length is zero
    collector                                   ;; Return collector, else
    (recurse-decode-int                         ;; Recurse
     (- len 1)                                  ;; Subtracting len by 1
     (rest bits)                                ;; With only the tail of the bit list
     (logior (ash collector 1) (first bits))))) ;; And the result of (collector << 1) | (first bits)

;; One-argument wrapper for recurse-decode-int
(defun bits->int (bits)
  (recurse-decode-int (length bits) bits 0))

;; Encode an integer of arbitrary length (up to 255 bits)
(defun encode-arbitrary-int (integer)
  (let* ((bit-length-as-bits (int->bits (integer-length integer)))   ;; Get the bit length
         (padded-bit-length  (pad-left 8 0 bit-length-as-bits)) ;; Pad it to 8 to standardize size
         (sign               (if (< 0 integer) 1 0))                 ;; Determine sign-bit
         (integer-bits       (int->bits integer)))                   ;; Get the bits of the number
    (concatenate 'list padded-bit-length (list sign) integer-bits))) ;; Concatenate length as bits and real bits

;; Decode an encoded integer of arbitrary length
(defun decode-arbitrary-int (bits)
  (let ((int-length (bits->int (subseq bits 0 8))))                  ;; Extract length of encoded int
    (if (zerop int-length)                                           ;; If the length of the encoded int is zero
      0                                                              ;; Return early
      (let ((sign (if (equalp 0 (nth 9 bits)) -1 1))                 ;; Otherwise, extract the sign bit
            (integer (bits->int (subseq bits 9 (+ 9 int-length)))))  ;; Extract the integer bits and decode
        (* sign integer)))))                                         ;; Multiply the decoded int by 1 or -1

;;;; Encoding and decoding strings

;; I am using my integer encoding functions to do string encoding,
;; I am 1000% sure that this wastes a massive amount of space
;; Probably inflates the actual size of the string by 3 or more times
;; But it was fun!

;; Encode a string as a list of integers
(defun encode-string (string)
  (mapcan                                       ;; Map this lambda, concatenating results
   (lambda (c) (encode-arbitrary-int (char-code c))) ;; Encode the char-code of each character
   (coerce string 'list)))                      ;; After coercing the string into a list (so that it can be mapped)

;; Recursively consume integers from bits and convert them to characters collected in coll
(defun recurse-decode-string (bits coll)
  (if bits                                                                             ;; If the input is not nil
    (let* ((first-int-length (bits->int (subseq bits 0 8)))                            ;; Length of the first int
           (first-int-bits   (+ 9 first-int-length))                                   ;; Total bit length of the first int
           (first-int        (decode-arbitrary-int (subseq bits 0 first-int-bits)))    ;; Decoded first int
           (remaining-bits   (subseq bits first-int-bits (length bits)))               ;; Bits remaining after decoding one
           (next             (concatenate 'string coll (list (code-char first-int))))) ;; The next collector value
      (recurse-decode-string remaining-bits next))                                     ;; Recurse collecting char
    coll))                                                                             ;; If input is nil, return collected string

;; Decode string from list of bits
(defun decode-string (bits)
  (recurse-decode-string bits ""))

;;;; TODO: Add various other types.. int and string alone is a little lacking

;;;; Bit and byte conversion & utilities

;; Convert a list of bits into a list of bytes padding to correct length if necessary
(defun bits->bytes (bits)
  (let* ((pad-length  (next-divisible (length bits) 8))   ;; Determine the necessary pad length
         (padded-bits (pad-right pad-length 0 bits)) ;; Pad the bits to fit into sections of 8
         (partitioned (partition padded-bits 8)))         ;; Partition the bits out
    (mapcan #'bits->int partitioned)))                    ;; map and concatenate the converter on each byte

;; Convert a list of bytes into a contiguous list of bits
(defun bytes->bits (bytes)
  (let ((bits-list (map 'list #'int->bits bytes)))           ;; Convert each byte into bits
    (mapcan (lambda (bits) (pad-left 8 0 bits)) bits-list))) ;; Map to pad each set of bits to 8 and concatenate

;;;; Generic encoding & decoding

(defun generic-encode (type-key data)
  (case type-key
    (:string (encode-string data))
    (:int    (encode-arbitrary-int data))))

(defun generic-decode (type-key data)
  (case type-key
    (:string (decode-string data))
    (:int    (decode-arbitrary-int data))))
