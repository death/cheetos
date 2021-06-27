;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:cheetos
  :description "Benchmark your code."
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("cheetos/all"))
