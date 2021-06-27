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
   #:standard-benchmark-reporter
   #:report-benchmark-run))

(in-package #:cheetos/reporter)

(defclass standard-benchmark-reporter (benchmark-reporter)
  ())

(defgeneric report-benchmark-run (standard-benchmark-reporter benchmark-run)
  (:documentation
   "Report performance information gathered in BENCHMARK-RUN."))

(defmethod report-benchmark-runs ((reporter standard-benchmark-reporter) runs)
  (setf runs (stable-sort (copy-list runs) #'< :key #'benchmark-run-start-time))
  (dolist (run runs)
    (report-benchmark-run reporter run)))

;; We could attempt to get a previous run for the same benchmark to
;; compare to.

(defmethod report-benchmark-run ((reporter standard-benchmark-reporter) run)
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
