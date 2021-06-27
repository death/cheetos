;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/utils
  (:use #:cl)
  (:export
   #:time-point-string
   #:time-range-string))

(in-package #:cheetos/utils)

(defun time-point-string (universal-time)
  (multiple-value-bind (now-s now-m now-h now-d now-mm now-y)
      (get-decoded-time)
    (declare (ignore now-s now-m now-h))
    (multiple-value-bind (s m h d mm y)
        (decode-universal-time universal-time)
      (if (and (= now-d d) (= now-mm mm) (= now-y y))
          (format nil "~2,'0D:~2,'0D:~2,'0D" h m s)
          (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
                  y mm d h m s)))))

(defun time-range-string (start-time end-time)
  (let ((s1 (time-point-string start-time))
        (s2 (time-point-string end-time)))
    (if (equal s1 s2)
        s1
        (format nil "~A to ~A" s1 s2))))
