;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/cheetos
  (:use #:cl))

(in-package #:cheetos/cheetos)

(defun coerce-to-function (x)
  (etypecase x
    (null
     (error "NIL cannot be used as a benchmark function designator."))
    (symbol
     (if (fboundp x)
         (fdefinition x)
         (error "There is no function with name ~S." x)))
    (function
     x)))

(defclass benchmark-suite ()
  ((table :initform (make-hash-table :test 'equal)
          :reader benchmark-suite-table)))

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
  ((suite :initarg :suite :reader benchmark-suite)
   (name :initarg :name :reader benchmark-name)
   (tag :initform nil :accessor benchmark-tag)
   (function :initarg :function :accessor benchmark-function)
   (runs :initform (make-array 0 :adjustable t) :reader benchmark-runs)))

(defgeneric create-benchmark-run (benchmark)
  (:documentation
   "Create a new benchmark run."))

(defgeneric add-benchmark-run (benchmark benchmark-run)
  (:documentation
   "Add BENCHMARK-RUN to the runs of BENCHMARK.

Returns the benchmark run."))

(defclass benchmark-run ()
  ((benchmark :initarg :benchmark :reader benchmark-run-benchmark)
   (start-time :initarg :start-time :reader benchmark-run-start-time)
   (end-time :initarg :end-time :reader benchmark-run-end-time)
   (tag :initarg :tag :reader benchmark-run-tag)
   (plist :initarg :plist :reader benchmark-run-plist)))

(defclass benchmark-reporter ()
  ())

(defgeneric report-benchmark-runs (benchmark-reporter benchmark-runs)
  (:documentation
   "Report performance information gathered in BENCHMARK-RUNS.

BENCHMARK-RUNS is a list of benchmark runs."))

(defclass simple-benchmark-reporter (benchmark-reporter)
  ())

(defgeneric report-benchmark-run (simple-benchmark-reporter benchmark-run)
  (:documentation
   "Report performance information gathered in BENCHMARK-RUN."))

(defmethod list-all-benchmarks ((suite benchmark-suite))
  (loop for benchmark being each hash-value of (benchmark-suite-table suite)
        collect benchmark))

(defmethod ensure-benchmark ((suite benchmark-suite) name &key function (tag nil tag-supplied))
  (let ((benchmark (or (lookup-benchmark suite name)
                       (create-benchmark suite name function))))
    (when (and function
               (not (eql function (benchmark-function benchmark))))
      (setf (benchmark-function benchmark) function))
    (when (and tag-supplied
               (not (equal tag (benchmark-tag benchmark))))
      (setf (benchmark-tag benchmark) tag))
    benchmark))

(defmethod create-benchmark ((suite benchmark-suite) name function)
  (let ((benchmark (lookup-benchmark suite name)))
    (unless (null benchmark)
      (error "Benchmark with name ~S already exists in suite ~S." name suite)))
  (setf function (coerce-to-function function))
  (setf (gethash name (benchmark-suite-table suite))
        (make-instance 'benchmark
                       :suite suite
                       :name name
                       :function function)))

(defmethod lookup-benchmark ((suite benchmark-suite) name)
  (gethash name (benchmark-suite-table suite)))

(defmethod remove-benchmark ((suite benchmark-suite) benchmark)
  (let ((name (benchmark-name benchmark)))
    (remhash name (benchmark-suite-table suite))))

(defmethod report-benchmark-runs ((reporter simple-benchmark-reporter) runs)
  (dolist (run runs)
    (report-benchmark-run reporter run)))

(defmethod report-benchmark-run ((reporter simple-benchmark-reporter) run)
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

(defmethod create-benchmark-run ((benchmark benchmark))
  (let ((start-time (get-universal-time))
        end-time
        (tag (benchmark-tag benchmark))
        plist)
    (sb-impl::call-with-timing
     (lambda (&rest timing-info)
       (setf end-time (get-universal-time))
       (setf plist (copy-list timing-info)))
     (benchmark-function benchmark))
    (make-instance 'benchmark-run
                   :benchmark benchmark
                   :start-time start-time
                   :end-time end-time
                   :tag tag
                   :plist plist)))

(defmethod add-benchmark-run ((benchmark benchmark) run)
  (assert (eq (benchmark-run-benchmark run) benchmark))
  (vector-push-extend run (benchmark-runs benchmark))
  run)

(defvar *benchmark-suite*
  (make-instance 'benchmark-suite))

(defvar *benchmark-reporter*
  (make-instance 'simple-benchmark-reporter))

(defun run-all-benchmarks (&key (benchmark-suite *benchmark-suite*)
                                (reporter *benchmark-reporter*))
  "Run all benchmarks in BENCHMARK-SUITE and report performance
information."
  (let ((benchmarks (list-all-benchmarks benchmark-suite))
        (new-runs '()))
    (dolist (benchmark benchmarks)
      (let ((new-run (create-benchmark-run benchmark)))
        (add-benchmark-run benchmark new-run)
        (push new-run new-runs)))
    (report-benchmark-runs reporter (nreverse new-runs))))

(defun run-benchmark (name &key (benchmark-suite *benchmark-suite*)
                                (reporter *benchmark-reporter*))
  "Run benchmark associated with NAME and report performance information."
  (let* ((benchmark
           (find-benchmark name
                           :benchmark-suite benchmark-suite
                           :if-does-not-exist :error))
         (new-run
           (create-benchmark-run benchmark)))
    (add-benchmark-run benchmark new-run)
    (report-benchmark-runs reporter (list new-run))))

(defun find-benchmark (name &key (benchmark-suite *benchmark-suite*)
                                 (if-does-not-exist nil))
  "Return the benchmark associated with NAME.

If no benchmark is associated with NAME, act according to
IF-DOES-NOT-EXIST:

 NIL - return NIL;

 :ERROR - signal an error."
  (let ((benchmark (lookup-benchmark benchmark-suite name)))
    (or benchmark
        (ecase if-does-not-exist
          ((nil)
           nil)
          (:error
           (error "Benchmark with name ~S does not exist in suite ~S."
                  name benchmark-suite))))))
