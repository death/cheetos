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
   #:collect-benchmarks
   #:define-benchmark
   #:measure))

(in-package #:cheetos/convenience)

(defvar *root-benchmark*
  (make-instance 'persisting-benchmark
                 :name '()
                 :body-function nil))

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
          (when new-run
            (add-run benchmark new-run)
            (push new-run new-runs))
          (report-end-benchmark reporter new-run))))
    (report-end-schedule reporter (nreverse new-runs))))

(defun find-benchmark (name &key (if-does-not-exist nil))
  "Find benchmark corresponding to NAME.

If there is no benchmark that corresponds to NAME, act according to
IF-DOES-NOT-EXIST: if it is null, return NIL; if it is :ERROR, signal
an error.

Note that the root benchmark is named ()."
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

BODY will be evaluated when the benchmark is run.  Within body, place
the forms to instrument inside a MEASURE form.

The following properties may be specified prior to the actual forms:

  :TAG <form>

    The value of <form> will be used as the benchmark's tag.  By
    default, the benchmark tag is NIL."
  (let ((measure-function (gensym))
        (body-function-var (gensym))
        (tag nil))
    (loop
     (cond ((eq (car body) :tag)
            (setf tag (cadr body))
            (setf body (cddr body)))
           (t
            (return))))
    `(let ((,body-function-var
             ,(if (null body)
                  `nil
                  `(lambda (,measure-function)
                     (declare (ignorable ,measure-function))
                     (macrolet ((measure (&body forms)
                                  `(funcall ,',measure-function (lambda () ,@forms))))
                       ,@body)))))
       (add-child *root-benchmark*
                  (make-instance 'persisting-benchmark
                                 :name ',name
                                 :tag ,tag
                                 :body-function ,body-function-var)))))
