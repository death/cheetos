;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/protocols
  (:use #:cl)
  (:export
   #:benchmark-suite
   #:list-all-benchmarks
   #:ensure-benchmark
   #:create-benchmark
   #:lookup-benchmark
   #:remove-benchmark
   #:benchmark
   #:list-session-runs
   #:create-benchmark-run
   #:add-benchmark-run
   #:benchmark-function
   #:benchmark-tag
   #:benchmark-name
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

(defclass benchmark-suite ()
  ())

(defgeneric list-all-benchmarks (benchmark-suite)
  (:documentation
   "Return a fresh list of all benchmark in BENCHMARK-SUITE."))

(defgeneric ensure-benchmark (benchmark-suite benchmake-name &key function tag &allow-other-keys)
  (:documentation
   "Create or update a benchmark associated with NAME, and return the
benchmark.

FUNCTION serves as an entry point for the code to instrument.

TAG will be used to tag future benchmark runs."))

(defgeneric create-benchmark (benchmark-suite benchmark-name benchmark-function)
  (:documentation
   "Associate a new benchmark in BENCHMARK-SUITE with BENCHMARK-NAME,
and return the benchmark.

BENCHMARK-FUNCTION serves as an entry point for the code to
instrument.

Signals an error if a benchmark is already associated with
BENCHMARK-NAME."))

(defgeneric lookup-benchmark (benchmark-suite benchmark-name)
  (:documentation
   "Return the benchmark associated with BENCHMARK-NAME in
BENCHMARK-SUITE, or NIL if there is no such association."))

(defgeneric remove-benchmark (benchmark-suite benchmark)
  (:documentation
   "Remove BENCHMARK from BENCHMARK-SUITE.

Returns true if the benchmark existed and was removed, and false
otherwise."))

(defclass benchmark ()
  ())

(defgeneric list-session-runs (benchmark)
  (:documentation
   "Return a fresh list of all runs, for the current session, of
BENCHMARK."))

(defgeneric create-benchmark-run (benchmark tag)
  (:documentation
   "Create a new benchmark run for BENCHMARK.

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
