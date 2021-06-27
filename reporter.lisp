;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/reporter
  (:use
   #:cl
   #:cheetos/protocols
   #:cheetos/utils)
  (:import-from
   #:alexandria)
  (:export
   #:standard-benchmark-reporter))

(in-package #:cheetos/reporter)

(defclass standard-benchmark-reporter (benchmark-reporter)
  ())

(defmethod report-start-schedule ((reporter standard-benchmark-reporter) benchmarks)
  (format t "~&Running ~D benchmark~:P~2%" (length benchmarks)))

(defmethod report-end-schedule ((reporter standard-benchmark-reporter) runs)
  (declare (ignore benchmark-runs)))

(defmethod report-start-benchmark ((reporter standard-benchmark-reporter) benchmark)
  (declare (ignore benchmark)))

;; We could attempt to get a previous run for the same benchmark to
;; compare to.

(defmethod report-end-benchmark ((reporter standard-benchmark-reporter) run)
  (let* ((name (benchmark-name (benchmark-run-benchmark run)))
         (tag (benchmark-run-tag run))
         (start-time (benchmark-run-start-time run))
         (plist (benchmark-run-plist run))
         (time-string (time-point-string start-time))
         (perf-string
           (format nil "~D μs, ~D b"
                   (getf plist :user-run-time-us)
                   (getf plist :bytes-consed))))
    (format t "[~A] <~:(~{~A~^ :: ~}~)>~@[ [~A]~] ~A~%"
            time-string
            (alexandria:ensure-list name)
            tag
            perf-string)))
