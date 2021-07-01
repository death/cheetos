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
   #:body-function
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
   #:user-run-time-us
   #:bytes-consed
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
   "Create new benchmark run for BENCHMARK, or return NIL if it's a
filler benchmark.

A filler benchmark is a benchmark that does not hold a reference to a
body function.  Such a benchmark usually exists to serve as a
container of child benchmarks.

The run will be tagged with TAG."))

(defgeneric add-run (benchmark run)
  (:documentation
   "Add RUN to the runs of BENCHMARK.

Returns the benchmark run."))

(defgeneric body-function (benchmark))

(defgeneric (setf body-function) (new-function benchmark))

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

(defgeneric user-run-time-us (run))

(defgeneric bytes-consed (run))

(defclass reporter ()
  ())

(defgeneric report-start-schedule (reporter benchmarks)
  (:documentation
   "Called before BENCHMARKS are run."))

(defgeneric report-end-schedule (reporter runs)
  (:documentation
   "Calls after benchmarks are run, with a list of the RUNS they
created.

Not all benchmarks create benchmark runs.  In particular, filler
benchmarks return NIL instead.  Therefore, the number of benchmark
runs in the list may be lower than the number of benchmarks at the
start of the schedule."))

(defgeneric report-start-benchmark (reporter benchmark)
  (:documentation
   "Called before BENCHMARK is run."))

(defgeneric report-end-benchmark (reporter run)
  (:documentation
   "Called after a benchmark created RUN, or returned NIL.

RUN may be either NIL or a benchmark run."))
