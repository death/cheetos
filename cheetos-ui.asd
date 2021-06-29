;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:cheetos-ui
  :description "CHEETOS User interface."
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("mcclim" "cheetos/ui"))
