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
   #:find-benchmark
   #:define-benchmark))

(in-package #:cheetos/convenience)

(defvar *benchmark-suite*
  (make-instance 'standard-benchmark-suite)
  "The current benchmark suite.")

(defvar *benchmark-reporter*
  (make-instance 'standard-benchmark-reporter)
  "The current benchmark run reporter.")

(defun run-all-benchmarks (&key (benchmark-suite *benchmark-suite*)
                                (reporter *benchmark-reporter*)
                                (tag nil tag-supplied))
  "Run all benchmarks in BENCHMARK-SUITE and report performance
information."
  (let ((benchmarks (list-all-benchmarks benchmark-suite))
        (new-runs '()))
    (dolist (benchmark benchmarks)
      (let ((new-run (create-benchmark-run benchmark
                                           (if tag-supplied
                                               tag
                                               (benchmark-tag benchmark)))))
        (add-benchmark-run benchmark new-run)
        (push new-run new-runs)))
    (report-benchmark-runs reporter (nreverse new-runs))))

(defun run-benchmark (name &key (benchmark-suite *benchmark-suite*)
                                (reporter *benchmark-reporter*)
                                (tag nil tag-supplied))
  "Run benchmark associated with NAME and report performance information."
  (let* ((benchmark
           (find-benchmark name
                           :benchmark-suite benchmark-suite
                           :if-does-not-exist :error))
         (new-run
           (create-benchmark-run benchmark
                                 (if tag-supplied
                                     tag
                                     (benchmark-tag benchmark)))))
    (add-benchmark-run benchmark new-run)
    (report-benchmark-runs reporter (list new-run))))

(defun find-benchmark (name &key (benchmark-suite *benchmark-suite*)
                                 (if-does-not-exist nil))
  "Return the benchmark associated with NAME.

If no benchmark is associated with NAME, act according to
IF-DOES-NOT-EXIST:

  NIL

    Return NIL.

  :ERROR

    Signal an error."
  (let ((benchmark (lookup-benchmark benchmark-suite name)))
    (or benchmark
        (ecase if-does-not-exist
          ((nil)
           nil)
          (:error
           (error "Benchmark with name ~S does not exist in suite ~S."
                  name benchmark-suite))))))

(defmacro define-benchmark (name &body body)
  "Define a benchmark associated with NAME.

BODY will be evaluated when the benchmark is run.  The following
properties may be specified prior to the actual forms:

  :TAG <form>

    The value of <form> will be used as the benchmark's tag.  By
    default, the benchmark tag is NIL."
  (let ((tag nil))
    (loop
     (cond ((eq (car body) :tag)
            (setf tag (cadr body))
            (setf body (cddr body)))
           (t
            (return))))
  `(ensure-benchmark *benchmark-suite*
                     ',name
                     :function (lambda () ,@body)
                     :tag ,tag)))
