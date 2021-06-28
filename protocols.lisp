;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/protocols
  (:use #:cl)
  (:export
   #:benchmark
   #:list-session-runs
   #:create-benchmark-run
   #:add-benchmark-run
   #:benchmark-function
   #:benchmark-tag
   #:benchmark-name
   #:benchmark-parent
   #:benchmark-children
   #:benchmark-add-child
   #:benchmark-lookup-child
   #:benchmark-remove-child
   #:benchmark-run
   #:benchmark-run-benchmark
   #:benchmark-run-start-time
   #:benchmark-run-end-time
   #:benchmark-run-tag
   #:benchmark-run-plist
   #:benchmark-reporter
   #:report-start-schedule
   #:report-end-schedule
   #:report-start-benchmark
   #:report-end-benchmark))

(in-package #:cheetos/protocols)

(defclass benchmark ()
  ())

(defgeneric list-session-runs (benchmark)
  (:documentation
   "Return a fresh list of all runs, for the current session, of
BENCHMARK."))

(defgeneric create-benchmark-run (benchmark tag)
  (:documentation
   "Create new benchmark run for BENCHMARK.

The run will be tagged with TAG."))

(defgeneric add-benchmark-run (benchmark benchmark-run)
  (:documentation
   "Add BENCHMARK-RUN to the runs of BENCHMARK.

Returns the benchmark run."))

(defgeneric benchmark-function (benchmark))

(defgeneric (setf benchmark-function) (new-function benchmark))

(defgeneric benchmark-tag (benchmark))

(defgeneric (setf benchmark-tag) (new-tag benchmark))

(defgeneric benchmark-name (benchmark))

(defgeneric benchmark-parent (benchmark))

(defgeneric (setf benchmark-parent) (new-parent benchmark))

(defgeneric benchmark-children (benchmark))

(defgeneric benchmark-add-child (parent child))

(defgeneric benchmark-lookup-child (benchmark child-name-relative))

(defgeneric benchmark-remove-child (benchmark child))

(defclass benchmark-run ()
  ())

(defgeneric benchmark-run-benchmark (benchmark-run))

(defgeneric benchmark-run-start-time (benchmark-run))

(defgeneric benchmark-run-end-time (benchmark-run))

(defgeneric benchmark-run-tag (benchmark-run))

(defgeneric benchmark-run-plist (benchmark-run))

(defclass benchmark-reporter ()
  ())

(defgeneric report-start-schedule (benchmark-reporter benchmarks))

(defgeneric report-end-schedule (benchmark-reporter benchmark-runs))

(defgeneric report-start-benchmark (benchmark-reporter benchmark))

(defgeneric report-end-benchmark (benchmark-reporter benchmark-run))
