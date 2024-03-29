;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/benchmark
  (:use
   #:cl
   #:cheetos/protocols
   #:cheetos/run)
  (:import-from
   #:alexandria)
  (:import-from
   #:sb-ext)
  (:import-from
   #:sb-impl)
  (:import-from
   #:sb-sprof)
  (:export
   #:standard-benchmark))

(in-package #:cheetos/benchmark)

(defclass standard-benchmark (benchmark)
  ((parent :initform nil :accessor parent)
   (children-table :initform (make-hash-table) :reader children-table)
   (name :initarg :name :reader name)
   (tag :initarg :tag :accessor tag)
   (body-function :initarg :body-function :accessor body-function)
   (runs :initform (make-array 0 :adjustable t :fill-pointer 0)
         :reader runs))
  (:default-initargs :tag nil))

(defmethod print-object ((benchmark standard-benchmark) stream)
  (print-unreadable-object (benchmark stream :type t)
    (with-slots (name tag runs) benchmark
      (format stream "~:S~@[ tag ~S~]~[~; ~:*~D run~:P~]" name tag (length runs)))))

(defmethod tag :around ((benchmark standard-benchmark))
  (or (call-next-method)
      (let ((parent (parent benchmark)))
        (if (null parent)
            nil
            (tag parent)))))

(defmethod profile ((benchmark standard-benchmark))
  (when (body-function benchmark)
    (funcall (body-function benchmark)
             (lambda (function-to-measure)
               (sb-sprof:with-profiling (:reset t :report :graph)
                 (funcall function-to-measure))))))

(defmethod create-run ((benchmark standard-benchmark) tag)
  (let ((start-time (get-universal-time))
        r-user-run-time-us
        r-bytes-consed)
    (when (body-function benchmark)
      (funcall (body-function benchmark)
               (lambda (function-to-measure)
                 ;; FIXME: SBCL-specific code here, for now.  It looks
                 ;; like we get more consistent results if we perform
                 ;; a full GC first.
                 (sb-ext:gc :full t)
                 (sb-impl::call-with-timing
                  (lambda (&key user-run-time-us bytes-consed &allow-other-keys)
                    (setf r-user-run-time-us user-run-time-us)
                    (setf r-bytes-consed bytes-consed))
                  function-to-measure)))
      (when (and r-user-run-time-us r-bytes-consed)
        (make-instance 'standard-run
                       :benchmark benchmark
                       :start-time start-time
                       :end-time (get-universal-time)
                       :tag tag
                       :user-run-time-us r-user-run-time-us
                       :bytes-consed r-bytes-consed)))))

(defmethod add-run ((benchmark standard-benchmark) run)
  (when run
    (assert (eq (benchmark run) benchmark))
    (vector-push-extend run (runs benchmark))
    run))

(defmethod list-session-runs ((benchmark standard-benchmark))
  (map 'list #'identity (runs benchmark)))

(defmethod children ((benchmark standard-benchmark))
  (loop for child being each hash-value of (children-table benchmark)
        collect child))

(defun valid-child-name-p (parent-name child-name)
  (alexandria:starts-with-subseq parent-name
                                 child-name
                                 :return-suffix t))

(defmethod add-child ((parent standard-benchmark) child)
  (let ((existing-parent (parent child)))
    (cond ((eq existing-parent parent))
          ((not (null existing-parent))
           (error "Benchmark ~S is already a child of ~S, so cannot be added to new parent ~S."
                  child existing-parent parent))
          (t
           (multiple-value-bind (is-prefix suffix)
               (valid-child-name-p (name parent) (name child))
             (when (or (not is-prefix)
                       (null suffix))
               (error "Benchmark ~S is not named suitably to be a child of ~S."
                      child parent))
             (let ((existing-child
                     (gethash (first suffix) (children-table parent))))
               (cond ((null (rest suffix))
                      (when existing-child
                        (setf (parent existing-child) nil)
                        (dolist (grandchild (children existing-child))
                          (remove-child existing-child grandchild)
                          (add-child child grandchild)))
                      (setf (parent child) parent)
                      (setf (gethash (first suffix) (children-table parent)) child))
                     (existing-child
                      (add-child existing-child child))
                     (t
                      (let ((intermediate
                              (make-instance 'standard-benchmark
                                             :name (append (name parent) (list (first suffix)))
                                             :body-function nil)))
                        (add-child parent intermediate)
                        (add-child intermediate child))))))))))

(defmethod lookup-child ((benchmark standard-benchmark) child-name-relative)
  (values (gethash child-name-relative (children-table benchmark))))

(defmethod remove-child ((parent standard-benchmark) child)
  (multiple-value-bind (is-prefix suffix)
      (valid-child-name-p (name parent) (name child))
    (cond ((or (not is-prefix) (null suffix))
           (error "Benchmark ~S is not a descendant of ~S by name." child parent))
          ((null (rest suffix))
           (cond ((null (parent child))
                  (assert (null (gethash (first suffix) (children-table parent)))))
                 ((eq (parent child) parent)
                  (remhash (first suffix) (children-table parent))
                  (setf (parent child) nil))
                 (t
                  (error "Benchmark ~S is not a descendant of ~S." child parent))))
          (t
           (let ((existing-child (gethash (first suffix) (children-table parent))))
             (cond ((null existing-child))
                   (t
                    (remove-child existing-child child))))))))
