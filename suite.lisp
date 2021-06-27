;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/suite
  (:use
   #:cl
   #:cheetos/protocols
   #:cheetos/benchmark)
  (:import-from
   #:alexandria)
  (:export
   #:standard-benchmark-suite))

(in-package #:cheetos/suite)

(defclass standard-benchmark-suite (benchmark-suite)
  ((table :initform (make-hash-table :test 'equal)
          :reader benchmark-suite-table)))

(defmethod list-all-benchmarks ((suite standard-benchmark-suite))
  (loop for benchmark being each hash-value of (benchmark-suite-table suite)
        collect benchmark))

(defmethod ensure-benchmark ((suite standard-benchmark-suite) name &key function tag)
  (let ((benchmark (or (lookup-benchmark suite name)
                       (create-benchmark suite name function))))
    (when (and function
               (not (eql function (benchmark-function benchmark))))
      (setf (benchmark-function benchmark) function))
    (when (not (equal tag (benchmark-tag benchmark)))
      (setf (benchmark-tag benchmark) tag))
    benchmark))

(defmethod create-benchmark ((suite standard-benchmark-suite) name function)
  (let ((benchmark (lookup-benchmark suite name)))
    (unless (null benchmark)
      (error "Benchmark with name ~S already exists in suite ~S." name suite)))
  (alexandria:ensure-functionf function)
  (setf (gethash name (benchmark-suite-table suite))
        (make-instance 'standard-benchmark
                       :suite suite
                       :name name
                       :function function)))

(defmethod lookup-benchmark ((suite standard-benchmark-suite) name)
  (gethash name (benchmark-suite-table suite)))

(defmethod remove-benchmark ((suite standard-benchmark-suite) benchmark)
  (let ((name (benchmark-name benchmark)))
    (remhash name (benchmark-suite-table suite))))
