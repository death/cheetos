;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/run
  (:use #:cl #:cheetos/protocols)
  (:export
   #:standard-benchmark-run))

(in-package #:cheetos/run)

(defclass standard-benchmark-run (benchmark-run)
  ((benchmark :initarg :benchmark :reader benchmark-run-benchmark)
   (start-time :initarg :start-time :reader benchmark-run-start-time)
   (end-time :initarg :end-time :reader benchmark-run-end-time)
   (tag :initarg :tag :reader benchmark-run-tag)
   (plist :initarg :plist :reader benchmark-run-plist)))
