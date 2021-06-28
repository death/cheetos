;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/convenience
  (:use
   #:cl
   #:cheetos/protocols
   #:cheetos/benchmark
   #:cheetos/reporter
   #:cheetos/persist)
  (:export
   #:*reporter*
   #:run-benchmark
   #:find-benchmark
   #:define-benchmark))

(in-package #:cheetos/convenience)

(defvar *root-benchmark*
  (make-instance 'persisting-benchmark
                 :name '()
                 :thunk nil))

(defvar *reporter*
  (make-instance 'standard-reporter)
  "The current benchmark run reporter.")

(defun run-benchmark (name &key (reporter *reporter*)
                                (tag nil tag-supplied))
  "Run benchmark associated with NAME, including any descendants, and
report performance information."
  (let ((benchmarks (collect-benchmarks
                     (find-benchmark name :if-does-not-exist :error)))
        (new-runs '()))
    (report-start-schedule reporter benchmarks)
    (with-db
      (dolist (benchmark benchmarks)
        (report-start-benchmark reporter benchmark)
        (let ((new-run (create-run benchmark
                                   (if tag-supplied
                                       tag
                                       (tag benchmark)))))
          (add-run benchmark new-run)
          (push new-run new-runs)
          (report-end-benchmark reporter new-run))))
    (report-end-schedule reporter (nreverse new-runs))))

(defun find-benchmark (name &key (if-does-not-exist nil))
  (labels ((rec (benchmark path)
             (cond ((null benchmark)
                    (ecase if-does-not-exist
                      ((nil) nil)
                      (:error (error "No benchmark with name ~S was found." name))))
                   ((null path)
                    benchmark)
                   (t
                    (rec (lookup-child benchmark (first path))
                         (rest path))))))
    (rec *root-benchmark* name)))

(defun collect-benchmarks (benchmark)
  "Return a list of BENCHMARK and its descendants."
  (cons benchmark
        (mapcan #'collect-benchmarks (children benchmark))))

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
    `(add-child *root-benchmark*
                (make-instance 'persisting-benchmark
                               :name ',name
                               :tag ,tag
                               :thunk (lambda () ,@body)))))
