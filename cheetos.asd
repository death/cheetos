;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:cheetos
  :description "Benchmark your code."
  :author "death <github.com/death>"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("cheetos/all")
  :in-order-to ((asdf:test-op (asdf:test-op #:cheetos-test))))
