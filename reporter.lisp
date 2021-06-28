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
   #:standard-reporter))

(in-package #:cheetos/reporter)

(defclass standard-reporter (reporter)
  ())

(defmethod report-start-schedule ((reporter standard-reporter) benchmarks)
  (format t "~&Running ~D benchmark~:P~2%"
          (count-if #'thunk benchmarks)))

(defmethod report-end-schedule ((reporter standard-reporter) runs)
  (declare (ignore runs)))

(defmethod report-start-benchmark ((reporter standard-reporter) benchmark)
  (declare (ignore benchmark)))

;; We could attempt to get a previous run for the same benchmark to
;; compare to.

(defmethod report-end-benchmark ((reporter standard-reporter) run)
  (let* ((name (name (benchmark run)))
         (tag (tag run))
         (start-time (start-time run))
         (user-run-time-us (user-run-time-us run))
         (bytes-consed (bytes-consed run))
         (time-string (time-point-string start-time))
         (perf-string (format nil "~D μs, ~D b" user-run-time-us bytes-consed)))
    (when (and user-run-time-us bytes-consed)
      (format t "[~A] <~:(~{~A~^ :: ~}~)>~@[ [~A]~] ~A~%"
              time-string
              (alexandria:ensure-list name)
              tag
              perf-string))))
