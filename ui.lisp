;;;; +----------------------------------------------------------------+
;;;; | CHEETOS                                                        |
;;;; +----------------------------------------------------------------+

(defpackage #:cheetos/ui
  (:use #:clim-lisp #:clim)
  (:import-from
   #:cheetos)
  (:import-from
   #:cheetos/persist)
  (:import-from
   #:cheetos/utils)
  (:export
   #:cheetos))

(in-package #:cheetos/ui)

(define-presentation-type tree-pane-node ())

(define-presentation-type benchmark ())

(define-presentation-type run ())

(define-application-frame cheetos ()
  ((root-benchmark :initform cheetos/convenience::*root-benchmark*
                   :reader root-benchmark)
   (expand-table :initform (make-hash-table)
                 :reader expand-table)
   (selected-benchmark :initform nil
                       :accessor selected-benchmark))
  (:panes
   (benchmark-tree :application
                   :display-function 'display-benchmark-tree
                   :min-width 200
                   :text-margins '(:left (:relative 10)
                                   :top (:relative 10)
                                   :right (:relative 10)
                                   :bottom (:relative 10))
                   :end-of-page-action :allow)
   (runs :application
         :display-function 'display-benchmark-runs
         :text-margins '(:left (:relative 10)
                         :top (:relative 10)
                         :right (:relative 10)
                         :bottom (:relative 10))
         :end-of-page-action :allow)
   (int :interactor))
  (:layouts
   (default
    (vertically ()
      (9/10 (horizontally ()
              (2/10 benchmark-tree)
              (8/10 runs)))
      (1/10 int)))))

(defun cheetos ()
  (run-frame-top-level
   (make-application-frame 'cheetos)))

(defun display-benchmark-tree (frame pane)
  (with-text-size (pane :huge)
    (with-drawing-options (pane :ink +deepskyblue4+)
      (surrounding-output-with-border (pane :shape :underline)
        (format pane "Benchmarks~%"))))
  (terpri pane)
  (stream-increment-cursor-position pane nil 10)
  (display-benchmark-node frame pane (root-benchmark frame)))

(defun display-benchmark-node (frame pane benchmark)
  (let* ((name (cheetos:name benchmark))
         (children (cheetos:children benchmark))
         (expand-table (expand-table frame))
         (is-expanded (eq (gethash benchmark expand-table :expand) :expand)))
    (with-text-family (pane :fix)
      (if (null children)
          (format pane " ")
          (with-output-as-presentation (pane benchmark 'tree-pane-node :single-box t)
            (multiple-value-bind (x y)
                (stream-cursor-position pane)
              (draw-point* pane x y :ink +transparent-ink+)
              (format pane "~A"
                      (if is-expanded "-" "+")))))
      (format pane " ")
      (with-output-as-presentation (pane benchmark 'benchmark :single-box t)
        (multiple-value-bind (ink background-ink)
            (if (eq (selected-benchmark frame) benchmark)
                (values +white+ +deepskyblue3+)
                (values +black+ +white+))
          (surrounding-output-with-border (pane :ink background-ink :padding 1 :filled t :move-cursor nil)
            (with-drawing-options (pane :ink ink)
              (format pane "~:(~A~) " (if (null name)
                                          "Root"
                                          (first (last name))))))))
      (terpri pane))
    (when (and children is-expanded)
      (indenting-output (pane '(1 :character))
        (dolist (child children)
          (display-benchmark-node frame pane child))))))

(define-cheetos-command (com-toggle-node)
    ((node tree-pane-node))
  (let* ((frame *application-frame*)
         (expand-table (expand-table frame)))
    (setf (gethash node expand-table)
          (ecase (gethash node expand-table :expand)
            (:expand :collapse)
            (:collapse :expand)))))

(define-presentation-to-command-translator toggle-a-node
    (tree-pane-node com-toggle-node cheetos)
    (object)
  (list object))

(define-cheetos-command (com-select-benchmark)
    ((benchmark benchmark))
  (let ((frame *application-frame*))
    (setf (selected-benchmark frame) benchmark)))

(define-cheetos-command (com-run-benchmark)
    ((benchmark benchmark))
  ;; FIXME: run this in another thread
  (let ((*standard-output* (find-pane-named *application-frame* 'int)))
    (cheetos:run-benchmark (cheetos:name benchmark))))

(define-cheetos-command (com-run-benchmark-with-tag)
    ((benchmark benchmark)
     (tag keyword))
  (let ((*standard-output* (find-pane-named *application-frame* 'int)))
    (cheetos:run-benchmark (cheetos:name benchmark)
                           :tag tag)))

;; The following translator forms are used to populate entries in a
;; benchmark's "context menu".  They will appear in reverse order,
;; though I doubt that it's guaranteed by the CLIM specification.

(define-presentation-to-command-translator run-a-benchmark-with-tag
    (benchmark com-run-benchmark-with-tag cheetos :gesture nil)
    (object)
  (list object))

(define-presentation-to-command-translator run-a-benchmark
    (benchmark com-run-benchmark cheetos :gesture nil)
    (object)
  (list object))

(define-presentation-to-command-translator select-a-benchmark
    (benchmark com-select-benchmark cheetos)
    (object)
  (list object))

(defun display-benchmark-runs (frame pane)
  (let ((benchmark (selected-benchmark frame)))
    (if (null benchmark)
        (display-benchmark-runs-1 pane benchmark)
        (cheetos/persist:with-db
          (dolist (b (cheetos/convenience::collect-benchmarks benchmark))
            (display-benchmark-runs-1 pane b))))))

(defun display-benchmark-runs-1 (pane benchmark)
  (with-text-size (pane :huge)
    (with-drawing-options (pane :ink +deepskyblue4+)
      (surrounding-output-with-border (pane :shape :underline :move-cursor t)
        (if (null benchmark)
            (format pane "No benchmark selected")
            (let ((name (cheetos:name benchmark)))
              (format pane "~:(~{~A~^ :: ~}~) Runs~%" (or name '("Root"))))))))
  (terpri pane)
  (stream-increment-cursor-position pane nil 10)
  (when benchmark
    (display-benchmark-runs-2 pane benchmark))
  (terpri pane))

(defun display-benchmark-runs-2 (pane benchmark)
  (let ((runs (cheetos/persist:list-runs benchmark))
        (last-start-date nil)
        (user-run-time-us-pct-threshold 0.1)
        (bytes-consed-pct-threshold 0.1))
    (when runs
      (formatting-table (pane :x-spacing '(5 :character) :equalize-column-widths t)
        (loop for i upfrom 0
              for (run previous-run) on runs
              do (let ((tag (cheetos:tag run))
                       (user-run-time-us (cheetos:user-run-time-us run))
                       (bytes-consed (cheetos:bytes-consed run))
                       (start-time (cheetos:start-time run)))
                   (multiple-value-bind (s m h d mm y) (decode-universal-time start-time)
                     (let ((start-date (list y mm d)))
                       (when (or (null last-start-date)
                                 (not (equal last-start-date start-date)))
                         (formatting-row (pane)
                           (formatting-cell (pane)
                             (with-text-size (pane :large)
                               (with-drawing-options (pane :ink +deepskyblue4+)
                                 (format pane "~4,'0D-~2,'0D-~2,'0D" y mm d)))))
                         (setf last-start-date start-date)))
                     (surrounding-output-with-border (pane :ink (if (oddp i)
                                                                    +gray90+
                                                                    +white+)
                                                           :filled t
                                                           :padding 0
                                                           :move-cursor nil)
                       (with-output-as-presentation (pane run 'run :single-box t)
                         (formatting-row (pane)
                           (formatting-cell (pane)
                             (format pane "~2,'0D:~2,'0D:~2,'0D" h m s))
                           (formatting-cell (pane)
                             (when tag
                               (format pane "~:(~A~)" tag)))
                           (formatting-cell (pane :align-x :right)
                             (format pane "~D μs" user-run-time-us))
                           (formatting-cell (pane :align-x :right)
                             (when previous-run
                               (let ((previous-user-run-time-us (cheetos:user-run-time-us previous-run)))
                                 (display-change-percentage pane
                                                            user-run-time-us
                                                            previous-user-run-time-us
                                                            user-run-time-us-pct-threshold))))
                           (formatting-cell (pane :align-x :right)
                             (format pane "~D b" bytes-consed))
                           (formatting-cell (pane :align-x :right)
                             (when previous-run
                               (let ((previous-bytes-consed (cheetos:bytes-consed previous-run)))
                                 (display-change-percentage pane
                                                            bytes-consed
                                                            previous-bytes-consed
                                                            bytes-consed-pct-threshold))))))))))))))

(defun display-change-percentage (stream current-value previous-value threshold)
  (cond ((< current-value previous-value)
         (with-drawing-options (stream :ink +green4+)
           (if (zerop current-value)
               (format stream "-100.0%")
               (let ((pct (* 100.0 (/ (- previous-value current-value) previous-value))))
                 (when (> pct threshold)
                   (format stream "-~,1F%" pct))))))
        ((> current-value previous-value)
         (with-drawing-options (stream :ink +red3+)
           (if (zerop previous-value)
               (format stream "BAD")
               (let ((pct (* 100.0 (/ (- current-value previous-value) previous-value))))
                 (when (> pct threshold)
                   (format stream "+~,1F%" pct))))))))

(define-cheetos-command (com-delete-run)
    ((run run))
  (cheetos/persist:with-db
    (cheetos/persist:delete-run run)))

(define-presentation-to-command-translator delete-a-run
    (run com-delete-run cheetos :gesture nil)
    (object)
  (list object))
