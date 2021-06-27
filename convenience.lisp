;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/convenience
  (:use
   #:cl
   #:cheetos/protocols
   #:cheetos/suite
   #:cheetos/reporter)
  (:export
   #:*benchmark-suite*
   #:*benchmark-reporter*
   #:run-all-benchmarks
   #:run-benchmark
   #:find-benchmark))

(in-package #:cheetos/convenience)

(defvar *benchmark-suite*
  (make-instance 'standard-benchmark-suite))

(defvar *benchmark-reporter*
  (make-instance 'standard-benchmark-reporter))

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
