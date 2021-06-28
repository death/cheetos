;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/run
  (:use
   #:cl
   #:cheetos/protocols
   #:cheetos/utils)
  (:export
   #:standard-run))

(in-package #:cheetos/run)

(defclass standard-run (run)
  ((benchmark :initarg :benchmark :reader benchmark)
   (start-time :initarg :start-time :reader start-time)
   (end-time :initarg :end-time :reader end-time)
   (tag :initarg :tag :reader tag)
   (user-run-time-us :initarg :user-run-time-us :reader user-run-time-us)
   (bytes-consed :initarg :bytes-consed :reader bytes-consed)))

(defmethod print-object ((run standard-run) stream)
  (print-unreadable-object (run stream :type t)
    (with-slots (tag start-time end-time user-run-time-us bytes-consed)
        run
      (format stream "~@[~S ~]~A ~D μs, ~D b"
              tag
              (time-range-string start-time end-time)
              user-run-time-us
              bytes-consed))))
