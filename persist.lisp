;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/persist
  (:use
   #:cl
   #:cheetos/protocols
   #:cheetos/benchmark)
  (:import-from
   #:uiop)
  (:import-from
   #:sqlite)
  (:export
   #:persisting-benchmark
   #:with-db))

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

(defclass persisting-benchmark (standard-benchmark)
  ())

(defmethod add-run :after ((benchmark persisting-benchmark) run)
  (with-tx
    (intern-run run)))
