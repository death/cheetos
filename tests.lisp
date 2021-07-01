;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/tests
  (:use #:cl #:cheetos)
  (:export
   #:run-tests))

(in-package #:cheetos/tests)

(defun ackermann (m n)
  (declare (optimize (speed 3)))
  (declare (type fixnum m n))
  (cond ((zerop m) (1+ n))
        ((zerop n) (ackermann (1- m) 1))
        (t (ackermann (1- m) (ackermann m (1- n))))))

(define-benchmark (ackermann tiny)
  (measure
   (ackermann 3 10)))

(define-benchmark (ackermann small)
  (measure
   (ackermann 3 12)))

(defun run-tests ()
  (run-benchmark nil))
