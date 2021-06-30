;;;; +----------------------------------------------------------------+
;;;; | BANE                                                           |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:cheetos-test
  :description "Tests for CHEETOS"
  :author "death <github.com/death>"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("cheetos/tests")
  :perform (asdf:test-op (op c) (uiop:symbol-call :cheetos/tests :run-tests)))
