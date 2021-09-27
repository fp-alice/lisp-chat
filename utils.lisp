;;;; utils.lisp

(in-package #:lispchat)

;;;; Functions for working with lists

;; Fills a list with an item using the difference between target-length and current-length
(defun create-pad (target-length current-length pad)
  (make-list (- target-length current-length) :initial-element pad))

;; Pad a list to a desired length with a desired pad item on the left
(defun pad-left (desired-length pad input)
  (let ((input-length (length input)))
    (if (<= desired-length input-length)    ;; If the desired length is >= actual length
      input                                 ;; Return the input
      (concatenate                          ;; Otherwise, create a pad list and concatenate on the left
       'list
       (create-pad desired-length input-length pad)
       input))))

;; Like pad-left, but pads on the right instead
(defun pad-right (desired-length pad input)
  (let ((input-length (length input)))
    (if (<= desired-length input-length)
      input
      (concatenate
       'list
       input
       (create-pad desired-length input-length pad)))))

;; Recursively partition a list into sublists
(defun recurse-partition (list len coll)
  (if list
    (recurse-partition
     (nthcdr len list)
     len
     (cons (subseq list 0 len) coll))
    (reverse coll)))

;; Wraps recurse-partition
(defun partition (list len)
  (recurse-partition list len '()))

;;;; Misc

;; Get the next number after current divisible by n
(defun next-divisible (current n)
  (+ current (- n (rem current n))))
