;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/tests
  (:use #:cl #:cheetos)
  (:export
   #:run-tests))

(in-package #:cheetos/tests)

;; (setf *benchmark-suite*
;;       (make-instance 'standard-benchmark-suite
;;                      :name 'cheetos-tests))
;;
;; Should it be (in-benchmark-suite cheetos-tests) ?
;;
;; Should suites be flat?
;;
;; Should they be a kind of benchmark, so that a run would constitute
;; the totals of benchmarks in it?
;;
;; Should benchmark names indicate the suites they belong to, so for
;; example (ackermann foo) belongs in the ackermann suite?

(defun ackermann (m n)
  (declare (optimize (speed 3)))
  (declare (type fixnum m n))
  (cond ((zerop m) (1+ n))
        ((zerop n) (ackermann (1- m) 1))
        (t (ackermann (1- m) (ackermann m (1- n))))))

(define-benchmark (ackermann tiny)
  (ackermann 3 10))

(define-benchmark (ackermann small)
  (ackermann 3 12))

(defun run-tests ()
  (run-all-benchmarks))
