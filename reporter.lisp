;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/reporter
  (:use #:cl #:cheetos/protocols)
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
  (dolist (run runs)
    (report-benchmark-run reporter run)))

(defmethod report-benchmark-run ((reporter standard-benchmark-reporter) run)
  (let* ((name
           (benchmark-name (benchmark-run-benchmark run)))
         (tag
           (benchmark-run-tag run))
         (time-string
           (format nil "~D ~D"
                   (benchmark-run-start-time run)
                   (benchmark-run-end-time run)))
         (perf-string
           (format nil "~S"
                   (benchmark-run-plist run))))
    (format t "~S ~@[[~A]~] ~A ~S~%"
            name
            tag
            time-string
            perf-string)))
