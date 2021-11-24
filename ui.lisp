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

(defclass cheetos-application-pane (application-pane)
  ()
  (:default-initargs
   :text-margins '(:left (:relative 10)
                   :top (:relative 10)
                   :right (:relative 10)
                   :bottom (:relative 10))
   :min-width 200
   :end-of-page-action :allow
   :incremental-redisplay t))

(define-application-frame cheetos ()
  ((root-benchmark :initform cheetos/convenience::*root-benchmark*
                   :reader root-benchmark)
   (expand-table :initform (make-hash-table)
                 :reader expand-table)
   (selected-benchmark :initform nil
                       :accessor selected-benchmark)
   (reference-run :initform nil
                  :accessor reference-run)
   (reference-pane :initform nil
                   :accessor reference-pane))
  (:panes
   (benchmark-tree
    (make-pane 'cheetos-application-pane
               :display-function 'display-benchmark-tree))
   (runs
    (make-pane 'cheetos-application-pane
               :display-function 'display-benchmark-runs))
   (int :interactor))
  (:layouts
   (default
    (vertically ()
      (9/10
       (horizontally ()
         (2/10 (scrolling () benchmark-tree))
         (make-pane 'clime:box-adjuster-gadget)
         (8/10
          (vertically (:name 'dynamic)
            (+fill+ (scrolling () runs))))))
      (make-pane 'clime:box-adjuster-gadget)
      (1/10 int)))))

(defun cheetos (&key (new-process t))
  (labels ((run ()
             (run-frame-top-level
              (make-application-frame 'cheetos))))
    (if new-process
        (clim-sys:make-process #'run :name "Cheetos")
        (run))))

(defun display-benchmark-tree (frame pane)
  (with-text-size (pane :huge)
    (with-drawing-options (pane :ink +deepskyblue4+)
      (surrounding-output-with-border (pane :shape :underline :move-cursor nil)
        (format pane "Benchmarks"))))
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

(define-cheetos-command (com-toggle-node :name t)
    ((node 'tree-pane-node))
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

(define-cheetos-command (com-select-benchmark :name t)
    ((benchmark 'benchmark))
  (let ((frame *application-frame*))
    (setf (selected-benchmark frame) benchmark)))

(define-cheetos-command (com-run-benchmark :name t)
    ((benchmark 'benchmark))
  ;; FIXME: run this in another thread
  (let ((*standard-output* (find-pane-named *application-frame* 'int)))
    (cheetos:run-benchmark (cheetos:name benchmark))))

(define-cheetos-command (com-run-benchmark-with-tag :name t)
    ((benchmark 'benchmark)
     (tag 'keyword))
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
      (surrounding-output-with-border (pane :shape :underline :move-cursor nil)
        (if (null benchmark)
            (format pane "No benchmark selected")
            (let ((name (cheetos:name benchmark)))
              (with-output-as-presentation (pane benchmark 'benchmark)
                (format pane "~:(~{~A~^ :: ~}~)" (or name '("Root"))))
              (format pane " Runs"))))))
  (terpri pane)
  (stream-increment-cursor-position pane nil 10)
  (when benchmark
    (display-benchmark-runs-2 pane (cheetos/persist:list-runs benchmark)))
  (terpri pane))

(defun display-benchmark-runs-2 (pane runs)
  (let ((last-start-date nil)
        (user-run-time-us-pct-threshold 0.1)
        (bytes-consed-pct-threshold 0.1))
    (when runs
      (formatting-table (pane :x-spacing '(5 :character) :equalize-column-widths t)
        (loop for i upfrom 0
              for (run previous-run) on runs
              do (let ((tag (cheetos:tag run))
                       (user-run-time-us (cheetos:user-run-time-us run))
                       (bytes-consed (cheetos:bytes-consed run))
                       (start-time (cheetos:start-time run))
                       (reference-run (or (reference-run *application-frame*) previous-run)))
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
                     (surrounding-output-with-border
                         (pane :ink (cond ((cheetos:run-equal run reference-run)
                                           +pink1+)
                                          ((oddp i)
                                           +gray90+)
                                          (t
                                           +white+))
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
                             (format pane "~:D μs" user-run-time-us))
                           (formatting-cell (pane :align-x :right)
                             (when reference-run
                               (let ((reference-user-run-time-us (cheetos:user-run-time-us reference-run)))
                                 (display-change-percentage pane
                                                            user-run-time-us
                                                            reference-user-run-time-us
                                                            user-run-time-us-pct-threshold))))
                           (formatting-cell (pane :align-x :right)
                             (format pane "~:D b" bytes-consed))
                           (formatting-cell (pane :align-x :right)
                             (when reference-run
                               (let ((reference-bytes-consed (cheetos:bytes-consed reference-run)))
                                 (display-change-percentage pane
                                                            bytes-consed
                                                            reference-bytes-consed
                                                            bytes-consed-pct-threshold))))))))))))))

(defun display-change-percentage (stream current-value reference-value threshold)
  (cond ((< current-value reference-value)
         (with-drawing-options (stream :ink +green4+)
           (if (zerop current-value)
               (format stream "-100.0%")
               (let ((pct (* 100.0 (/ (- reference-value current-value) reference-value))))
                 (when (> pct threshold)
                   (format stream "-~,1F%" pct))))))
        ((> current-value reference-value)
         (with-drawing-options (stream :ink +red3+)
           (if (zerop reference-value)
               (format stream "BAD")
               (let ((pct (* 100.0 (/ (- current-value reference-value) reference-value))))
                 (when (> pct threshold)
                   (format stream "+~,1F%" pct))))))))

(define-cheetos-command (com-delete-run :name t)
    ((run 'run))
  (cheetos/persist:with-db
    (cheetos/persist:delete-run run)))

(define-presentation-to-command-translator delete-a-run
    (run com-delete-run cheetos :gesture nil)
    (object)
  (list object))

(defclass reference-pane (application-pane)
  ()
  (:default-initargs
   :display-function 'display-reference-pane))

(defun display-reference-pane (frame pane)
  (let ((run (reference-run frame)))
    (when run
      (with-text-size (pane :large)
        (with-drawing-options (pane :ink +pink4+)
          (surrounding-output-with-border (pane :shape :underline :move-cursor nil)
            (let* ((benchmark (cheetos:benchmark run))
                   (name (cheetos:name benchmark)))
              (with-output-as-presentation (pane benchmark 'benchmark)
                (format pane "~:(~{~A~^ :: ~}~)" (or name '("Root"))))
              (format pane " Reference Run")))))
      (terpri pane)
      (display-benchmark-runs-2 pane (list run)))))

(define-cheetos-command (com-toggle-reference-run :name t)
    ((run 'run))
  (let* ((frame *application-frame*)
         (pane (or (reference-pane frame)
                   (setf (reference-pane frame)
                         (make-pane 'reference-pane))))
         (dynamic (find-pane-named frame 'dynamic)))
    (setf (reference-run frame)
          (if (cheetos:run-equal (reference-run frame) run)
              nil
              run))
    (cond ((and (null (reference-run frame))
                (sheet-parent pane))
           (sheet-disown-child dynamic pane))
          ((and (reference-run frame)
                (null (sheet-parent pane)))
           (sheet-adopt-child dynamic pane)
           ;; CLIM does not expose interfaces to control box layout
           ;; children, so we resort to fiddling with McCLIM internals
           ;; in order to change the pane's proportion in the box.
           ;; The pane was wrapped as a box-client and appended to the
           ;; box layout's clients list.
           (let* ((clients (climi::box-layout-mixin-clients dynamic))
                  (box-client (find pane clients :key #'climi::box-client-pane)))
             (assert (not (null box-client)))
             (setf (climi::box-client-fixed-size box-client) 100)
             (change-space-requirements dynamic))))))

(define-presentation-to-command-translator toggle-reference-run
    (run com-toggle-reference-run cheetos)
    (object)
  (list object))
