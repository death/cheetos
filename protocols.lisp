;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/protocols
  (:use #:cl)
  (:export
   #:benchmark
   #:list-session-runs
   #:create-run
   #:add-run
   #:thunk
   #:tag
   #:name
   #:parent
   #:children
   #:add-child
   #:lookup-child
   #:remove-child
   #:run
   #:benchmark
   #:start-time
   #:end-time
   #:tag
   #:plist
   #:reporter
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

(defgeneric create-run (benchmark tag)
  (:documentation
   "Create new benchmark run for BENCHMARK.

The run will be tagged with TAG."))

(defgeneric add-run (benchmark run)
  (:documentation
   "Add RUN to the runs of BENCHMARK.

Returns the benchmark run."))

(defgeneric thunk (benchmark))

(defgeneric (setf thunk) (new-thunk benchmark))

(defgeneric tag (benchmark))

(defgeneric (setf tag) (new-tag benchmark))

(defgeneric name (benchmark))

(defgeneric parent (benchmark))

(defgeneric (setf parent) (new-parent benchmark))

(defgeneric children (benchmark))

(defgeneric add-child (parent child))

(defgeneric lookup-child (benchmark child-name-relative))

(defgeneric remove-child (benchmark child))

(defclass run ()
  ())

(defgeneric benchmark (run))

(defgeneric start-time (run))

(defgeneric end-time (run))

(defgeneric tag (run))

(defgeneric plist (run))

(defclass reporter ()
  ())

(defgeneric report-start-schedule (reporter benchmarks))

(defgeneric report-end-schedule (reporter runs))

(defgeneric report-start-benchmark (reporter benchmark))

(defgeneric report-end-benchmark (reporter run))
