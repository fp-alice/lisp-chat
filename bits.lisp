;;;; bits.lisp

(in-package #:lispchat)

(defclass bit-queue ()
    ((queue
      :initform #*
      :initarg  :queue
      :type     'bit-vector
      :accessor queue)))

(defmethod print-object ((bq bit-queue) stream)
  (print-unreadable-object (bq stream :type t :identity t)
    (with-slots (queue) bq
      (format stream "~a" queue))))

(defmethod peek-queue ((bq bit-queue) len)
  (subseq (queue bq) 0 len))

(defmethod pop-queue ((bq bit-queue) len)
  (let* ((bitvec (queue bq))
         (head   (subseq bitvec 0 len))
         (tail   (subseq bitvec len (length bitvec))))
    (setf (queue bq) tail)
    head))

(defmethod push-queue ((bq bit-queue) bits)
  (setf (queue bq) (concatenate 'bit-vector (queue bq) bits)))

(defmethod push-int-queue ((bq bit-queue) int)
  (let ((sign (bit-smasher:bits<- (if (<= 0 int) 1)))
        (size (bit-smasher:bits<- (bit-smasher:byte-length (abs int))))
        (bits (bit-smasher:bits<- (abs int))))
    (push-queue bq sign)
    (push-queue bq size)
    (push-queue bq bits)))

(defmethod push-string-queue ((bq bit-queue) str)
  (let ((strlen (length str)))
    (push-int-queue bq strlen)
    (loop for c across str do
      (push-int-queue bq (char-code c)))))

(defmethod pop-byte-queue ((bq bit-queue))
  (bit-smasher:int<- (pop-queue bq 8)))

(defmethod pop-int-queue ((bq bit-queue))
  (let ((sign (bit-smasher:int<- (pop-queue bq 8)))
        (size (bit-smasher:int<- (pop-queue bq 8))))
    (* (if (zerop sign) -1 1) (bit-smasher:int<- (pop-queue bq (* 8 size))))))

(defmethod pop-string-queue ((bq bit-queue))
  (with-output-to-string (stream)
    (dotimes (_ (pop-int-queue bq))
      (princ (code-char (pop-int-queue bq)) stream))))

(defmethod push-generic-queue ((bq bit-queue) type-key value)
  (case type-key
    (:string (push-string-queue bq value))
    (:int    (push-int-queue bq value))))

(defmethod pop-generic-queue ((bq bit-queue) type-key)
  (case type-key
    (:string (pop-string-queue bq))
    (:int    (pop-int-queue bq))))

(defmethod queue->bytes ((bq bit-queue))
  (let ((byte-length (/ (length (queue bq)) 8)))
    (loop for x from 1 to byte-length
          collect (pop-byte-queue bq))))
