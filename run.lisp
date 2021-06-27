;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/run
  (:use
   #:cl
   #:cheetos/protocols
   #:cheetos/utils)
  (:export
   #:standard-benchmark-run))

(in-package #:cheetos/run)

(defclass standard-benchmark-run (benchmark-run)
  ((benchmark :initarg :benchmark :reader benchmark-run-benchmark)
   (start-time :initarg :start-time :reader benchmark-run-start-time)
   (end-time :initarg :end-time :reader benchmark-run-end-time)
   (tag :initarg :tag :reader benchmark-run-tag)
   (plist :initarg :plist :reader benchmark-run-plist)))

(defmethod print-object ((run benchmark-run) stream)
  (print-unreadable-object (run stream :type t)
    (with-slots (tag start-time end-time plist) run
      (format stream "~@[~S ~]~A ~D μs, ~D b"
              tag
              (time-range-string start-time end-time)
              (getf plist :user-run-time-us)
              (getf plist :bytes-consed)))))
