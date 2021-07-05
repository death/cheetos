;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/persist
  (:use
   #:cl
   #:cheetos/protocols
   #:cheetos/benchmark
   #:cheetos/run)
  (:import-from
   #:uiop)
  (:import-from
   #:sqlite)
  (:export
   #:persisting-benchmark
   #:with-db
   #:list-runs
   #:delete-run
   #:id))

(in-package #:cheetos/persist)

(defvar *db*
  nil)

;; Currently we assume the database file is already there and that the
;; CHEETOS schema has been applied.

(defvar *cheetos-db-pathname*
  (uiop:xdg-data-pathname "cheetos/cheetos.db" :output))

(defun call-with-db (function)
  (if (null *db*)
      (sqlite:with-open-database (*db* *cheetos-db-pathname*)
        (funcall function))
      (funcall function)))

(defmacro with-db (&body forms)
  `(call-with-db (lambda () ,@forms)))

(defvar *in-tx* nil)

(defun call-with-tx (function)
  (cond (*in-tx*
         (funcall function))
        (*db*
         (sqlite:with-transaction *db*
           (funcall function)))
        (t
         (with-db
           (sqlite:with-transaction *db*
             (funcall function))))))

(defmacro with-tx (&body forms)
  `(call-with-tx (lambda () ,@forms)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sql-string (sql)
    (etypecase sql
      (string sql)
      (list
       (with-output-to-string (out)
         (dolist (part sql)
           (write-string part out)
           (write-char #\Space out)))))))

(defmacro e/s (sql &rest parameters)
  `(sqlite:execute-single *db* ,(sql-string sql) ,@parameters))

(defmacro e/l (sql &rest parameters)
  `(sqlite:execute-to-list *db* ,(sql-string sql) ,@parameters))

(defmacro e (sql &rest parameters)
  `(sqlite:execute-non-query *db* ,(sql-string sql) ,@parameters))

(defun count-benchmarks ()
  (e/s "SELECT COUNT(*) FROM benchmarks"))

(defun add-benchmark (name)
  (e "INSERT INTO benchmarks (name) VALUES (?)" name))

(defun list-benchmarks ()
  (e/l "SELECT id, name FROM benchmarks"))

(defun find-benchmark (name)
  (e/s "SELECT id FROM benchmarks WHERE name = ?" name))

(defun intern-benchmark (name)
  (or (find-benchmark name)
      (progn
        (add-benchmark name)
        (find-benchmark name))))

(defun add-tag (tag)
  (e "INSERT INTO tags (tag) VALUES (?)" tag))

(defun list-tags ()
  (e/l "SELECT id, tag FROM tags"))

(defun find-tag (tag)
  (e/s "SELECT id FROM tags WHERE tag = ?" tag))

(defun find-tag-by-id (tag-id)
  (e/s "SELECT tag FROM tags WHERE id = ?" tag-id))

(defun intern-tag (tag)
  (or (find-tag tag)
      (progn
        (add-tag tag)
        (find-tag tag))))

(defun intern-run (run)
  (with-standard-io-syntax
    (let ((*package* (load-time-value (find-package "KEYWORD"))))
      (e ("INSERT INTO runs"
          "(benchmark_id, start_time, end_time, tag_id, user_run_time_us, bytes_consed)"
          "VALUES (?, ?, ?, ?, ?, ?)")
         (intern-benchmark (prin1-to-string (name (benchmark run))))
         (start-time run)
         (end-time run)
         (intern-tag (prin1-to-string (tag run)))
         (user-run-time-us run)
         (bytes-consed run)))))

(defclass persisted-run (standard-run)
  ((id :initarg :id :reader id)))

(defun list-runs (benchmark)
  (with-standard-io-syntax
    (let ((*package* (load-time-value (find-package "KEYWORD")))
          (*read-eval* nil))
      (with-tx
        (let ((benchmark-id (find-benchmark (prin1-to-string (name benchmark))))
              (tag-ids-to-tags (make-hash-table)))
          (labels ((make-run (row)
                     (destructuring-bind (id benchmark-id start-time
                                          end-time tag-id user-run-time-us
                                          bytes-consed)
                         row
                       (declare (ignore benchmark-id))
                       (make-instance 'persisted-run
                                      :id id
                                      :benchmark benchmark
                                      :start-time start-time
                                      :end-time end-time
                                      :tag (or (gethash tag-id tag-ids-to-tags)
                                               (setf (gethash tag-id tag-ids-to-tags)
                                                     (read-from-string (find-tag-by-id tag-id))))
                                      :user-run-time-us user-run-time-us
                                      :bytes-consed bytes-consed))))
            (when benchmark-id
              (mapcar #'make-run
                      (e/l ("SELECT id, benchmark_id, start_time, end_time, tag_id, user_run_time_us, bytes_consed"
                            "FROM runs"
                            "WHERE benchmark_id = ?"
                            "ORDER BY id DESC")
                           benchmark-id)))))))))

(defun delete-run (run)
  (when (typep run 'persisted-run)
    (e "DELETE FROM runs WHERE id = ?" (id run))))

(defclass persisting-benchmark (standard-benchmark)
  ())

(defmethod add-run :after ((benchmark persisting-benchmark) run)
  (when run
    (with-tx
      (intern-run run))))
