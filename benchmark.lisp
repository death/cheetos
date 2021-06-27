;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/benchmark
  (:use #:cl #:cheetos/protocols)
  (:export
   #:standard-benchmark))

(in-package #:cheetos/benchmark)

(defclass standard-benchmark (benchmark)
  ((suite :initarg :suite :reader benchmark-suite)
   (name :initarg :name :reader benchmark-name)
   (tag :initform nil :accessor benchmark-tag)
   (function :initarg :function :accessor benchmark-function)
   (runs :initform (make-array 0 :adjustable t) :reader benchmark-runs)))

(defmethod create-benchmark-run ((benchmark benchmark))
  (let ((start-time (get-universal-time))
        end-time
        (tag (benchmark-tag benchmark))
        plist)
    (sb-impl::call-with-timing
     (lambda (&rest timing-info)
       (setf end-time (get-universal-time))
       (setf plist (copy-list timing-info)))
     (benchmark-function benchmark))
    (make-instance 'benchmark-run
                   :benchmark benchmark
                   :start-time start-time
                   :end-time end-time
                   :tag tag
                   :plist plist)))

(defmethod add-benchmark-run ((benchmark benchmark) run)
  (assert (eq (benchmark-run-benchmark run) benchmark))
  (vector-push-extend run (benchmark-runs benchmark))
  run)
