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
   (plist :initarg :plist :reader plist)))

(defmethod print-object ((run standard-run) stream)
  (print-unreadable-object (run stream :type t)
    (with-slots (tag start-time end-time plist) run
      (format stream "~@[~S ~]~A ~D μs, ~D b"
              tag
              (time-range-string start-time end-time)
              (getf plist :user-run-time-us)
              (getf plist :bytes-consed)))))
