;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/benchmark
  (:use
   #:cl
   #:cheetos/protocols
   #:cheetos/run)
  (:export
   #:standard-benchmark))

(in-package #:cheetos/benchmark)

(defclass standard-benchmark (benchmark)
  ((suite :initarg :suite :reader benchmark-suite)
   (name :initarg :name :reader benchmark-name)
   (tag :initform nil :accessor benchmark-tag)
   (function :initarg :function :accessor benchmark-function)
   (runs :initform (make-array 0 :adjustable t :fill-pointer 0)
         :reader benchmark-runs)))

(defmethod print-object ((benchmark standard-benchmark) stream)
  (print-unreadable-object (benchmark stream :type t)
    (with-slots (name tag runs) benchmark
      (format stream "~S~@[ tag ~S~]~[~; ~:*~D run~:P~]" name tag (length runs)))))

(defmethod create-benchmark-run ((benchmark standard-benchmark))
  (let ((start-time (get-universal-time))
        end-time
        (tag (benchmark-tag benchmark))
        plist)
    ;; FIXME: SBCL-specific code here, for now...
    (sb-impl::call-with-timing
     (lambda (&rest timing-info)
       (setf end-time (get-universal-time))
       (setf plist (copy-list timing-info)))
     (benchmark-function benchmark))
    (make-instance 'standard-benchmark-run
                   :benchmark benchmark
                   :start-time start-time
                   :end-time end-time
                   :tag tag
                   :plist plist)))

(defmethod add-benchmark-run ((benchmark standard-benchmark) run)
  (assert (eq (benchmark-run-benchmark run) benchmark))
  (vector-push-extend run (benchmark-runs benchmark))
  run)

(defmethod list-session-runs ((benchmark standard-benchmark))
  (map 'list #'identity (benchmark-runs benchmark)))
