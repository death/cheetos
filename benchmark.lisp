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
  (:export
   #:standard-benchmark))

(in-package #:cheetos/benchmark)

(defclass standard-benchmark (benchmark)
  ((parent :initform nil :accessor benchmark-parent)
   (children-table :initform (make-hash-table) :reader benchmark-children-table)
   (name :initarg :name :reader benchmark-name)
   (tag :initarg :tag :accessor benchmark-tag)
   (function :initarg :function :accessor benchmark-function)
   (runs :initform (make-array 0 :adjustable t :fill-pointer 0)
         :reader benchmark-runs))
  (:default-initargs :tag nil))

(defmethod print-object ((benchmark standard-benchmark) stream)
  (print-unreadable-object (benchmark stream :type t)
    (with-slots (name tag runs) benchmark
      (format stream "~S~@[ tag ~S~]~[~; ~:*~D run~:P~]" name tag (length runs)))))

(defmethod create-benchmark-run ((benchmark standard-benchmark) tag)
  (let ((start-time (get-universal-time))
        end-time
        plist)
    (when (benchmark-function benchmark)
      ;; FIXME: SBCL-specific code here, for now...
      (sb-impl::call-with-timing
       (lambda (&rest timing-info)
         (setf plist (copy-list timing-info)))
       (benchmark-function benchmark)))
    (make-instance 'standard-benchmark-run
                   :benchmark benchmark
                   :start-time start-time
                   :end-time (get-universal-time)
                   :tag tag
                   :plist plist)))

(defmethod add-benchmark-run ((benchmark standard-benchmark) run)
  (assert (eq (benchmark-run-benchmark run) benchmark))
  (vector-push-extend run (benchmark-runs benchmark))
  run)

(defmethod list-session-runs ((benchmark standard-benchmark))
  (map 'list #'identity (benchmark-runs benchmark)))

(defmethod benchmark-children ((benchmark standard-benchmark))
  (loop for child being each hash-value of (benchmark-children-table benchmark)
        collect child))

(defun valid-child-name-p (parent-name child-name)
  (alexandria:starts-with-subseq parent-name
                                 child-name
                                 :return-suffix t))

(defmethod benchmark-add-child ((parent standard-benchmark) child)
  (let ((existing-parent (benchmark-parent child)))
    (cond ((eq existing-parent parent))
          ((not (null existing-parent))
           (error "Benchmark ~S is already a child of ~S, so cannot be added to new parent ~S."
                  child existing-parent parent))
          (t
           (multiple-value-bind (is-prefix suffix)
               (valid-child-name-p (benchmark-name parent)
                                   (benchmark-name child))
             (when (or (not is-prefix)
                       (null suffix))
               (error "Benchmark ~S is not named suitably to be a child of ~S."
                      child parent))
             (let ((existing-child
                     (gethash (first suffix)
                              (benchmark-children-table parent))))
               (cond ((null (rest suffix))
                      (when existing-child
                        (setf (benchmark-parent existing-child) nil)
                        (dolist (grandchild (benchmark-children existing-child))
                          (benchmark-remove-child existing-child grandchild)
                          (benchmark-add-child child grandchild)))
                      (setf (benchmark-parent child) parent)
                      (setf (gethash (first suffix)
                                     (benchmark-children-table parent))
                            child))
                     (existing-child
                      (benchmark-add-child existing-child child))
                     (t
                      (let ((intermediate
                              (make-instance 'standard-benchmark
                                             :name (append (benchmark-name parent)
                                                           (list (first suffix)))
                                             :function nil)))
                        (benchmark-add-child parent intermediate)
                        (benchmark-add-child intermediate child))))))))))

(defmethod benchmark-lookup-child ((benchmark standard-benchmark) child-name-relative)
  (values (gethash child-name-relative (benchmark-children-table benchmark))))

(defmethod benchmark-remove-child ((parent standard-benchmark) child)
  (multiple-value-bind (is-prefix suffix)
      (valid-child-name-p (benchmark-name parent)
                          (benchmark-name child))
    (cond ((or (not is-prefix) (null suffix))
           (error "Benchmark ~S is not a descendant of ~S by name." child parent))
          ((null (rest suffix))
           (cond ((null (benchmark-parent child))
                  (assert (null (gethash (first suffix)
                                         (benchmark-children-table parent)))))
                 ((eq (benchmark-parent child) parent)
                  (remhash (first suffix)
                           (benchmark-children-table parent))
                  (setf (benchmark-parent child) nil))
                 (t
                  (error "Benchmark ~S is not a descendant of ~S." child parent))))
          (t
           (let ((existing-child (gethash (first suffix)
                                          (benchmark-children-table parent))))
             (cond ((null existing-child))
                   (t
                    (benchmark-remove-child existing-child child))))))))
