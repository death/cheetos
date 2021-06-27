;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(uiop:define-package #:cheetos/all
  (:nicknames #:cheetos)
  (:documentation
   "Contains and exports public symbols of CHEETOS.")
  (:use-reexport
   #:cheetos/protocols
   #:cheetos/suite
   #:cheetos/benchmark
   #:cheetos/run
   #:cheetos/reporter
   #:cheetos/convenience))
