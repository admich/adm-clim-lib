(in-package :adm-clim-lib)

(defclass treeview-pane (application-pane)
  ((tree-roots :accessor treeview-pane-roots :initarg :tree-roots :initform nil)
   (printer :accessor treeview-pane-printer :initarg :printer)
   (tabular-printers :accessor treeview-pane-tabular-printers :initarg :tabular-printers)
   (inferior-producer :accessor treeview-pane-inferior-producer :initarg :inferior-producer)
   (expanded-table :initform (make-hash-table))))

(defclass expandable-graph-node ()
  ((expand :initform nil :accessor expand-tree)))

(define-presentation-type expandable-graph-node ())

(define-presentation-method present (object (type expandable-graph-node) stream view &key acceptably for-context-type)
  (if (expand-tree object)
      (multiple-value-bind (x y) (stream-cursor-position stream)
        (draw-polygon* stream (list x y  (+ x 10) y (+ x 5) (+ y 10)))
        (stream-increment-cursor-position stream 20 0))
      (multiple-value-bind (x y) (stream-cursor-position stream)
        (draw-polygon* stream (list x y x (+ y 10) (+ x 10) (+ y 5)))
        (stream-increment-cursor-position stream 20 0))))

(define-presentation-method present (object (type expandable-graph-node) stream (view textual-view) &key acceptably for-context-type)
  (if (expand-tree object)
      (format stream " - ")
      (format stream " + ")))

(define-command-table treeview)

(define-presentation-action toggle-treeview
    (expandable-graph-node nil treeview
                           :menu nil
                           :documentation "Expand/Shrink tree node"
                           :pointer-documentation "Expand/Shrink tree node"
                           :gesture :select)
    (object window)
  (setf (expand-tree object) (not (expand-tree object)))
  (redisplay-frame-pane *application-frame* window))

(defmethod display-pane (frame pane)
  (display-pane-with-view frame pane (stream-default-view pane)))

(defmethod display-pane-with-view (frame (pane treeview-pane) view)
  (with-slots (printer inferior-producer expanded-table) pane
    (labels ((expanded (node)
               (and (funcall inferior-producer node) (expand-tree (gethash node expanded-table))))
             (print-node (node indent)
               (unless (or (gethash node expanded-table)
                           (not (funcall inferior-producer node)))
                 (setf (gethash node expanded-table)
                       (make-instance 'expandable-graph-node)))
               (fresh-line pane)
               (indenting-output (pane indent)
                 (if (funcall inferior-producer node)
                     ;; (present (gethash node expanded-table) 'expandable-graph-node :stream pane)
                     (let ((obj (gethash node expanded-table)))
                       (with-output-as-presentation (pane obj 'expandable-graph-node)
                         (if (expand-tree obj)
                             (multiple-value-bind (x y) (stream-cursor-position pane)
                               (draw-polygon* pane (list x y (+ x 10) y (+ x 5) (+ y 10))))
                             (multiple-value-bind (x y) (stream-cursor-position pane)
                               (draw-polygon* pane (list x y x (+ y 10) (+ x 10) (+ y 5))))))))
                 (stream-increment-cursor-position pane 20 0))
               (indenting-output (pane (+ indent))
                 (funcall printer node pane))
               (when (expanded node)
                 (dolist (node (funcall inferior-producer node))
                   (print-node node (+ indent 20))))))
      (dolist (node (funcall (treeview-pane-roots pane) frame))
        (print-node node 0)))))

(defclass tabular-view (view)
  ())

(defconstant +tabular-view+ (make-instance 'tabular-view))

(defmethod display-pane-with-view (frame (pane treeview-pane) (view tabular-view))
  (with-slots (printer inferior-producer expanded-table tabular-printers) pane
    (labels ((expanded (node)
               (and (funcall inferior-producer node) (expand-tree (gethash node expanded-table))))
             (print-node (node indent)
               (unless (or (gethash node expanded-table)
                           (not (funcall inferior-producer node)))
                 (setf (gethash node expanded-table)
                       (make-instance 'expandable-graph-node)))
               (formatting-row (pane)
                 (formatting-cell (pane)
                   (format pane " ")
                   (stream-increment-cursor-position pane indent 0)
                   (if (funcall inferior-producer node)
                       (let ((obj (gethash node expanded-table)))
                         (with-output-as-presentation (pane obj 'expandable-graph-node)
                           (multiple-value-bind (x y) (stream-cursor-position pane)
                             (if (expand-tree obj)
                                 (draw-polygon* pane (list x y (+ x 10) y (+ x 5) (+ y 10)))
                                 (draw-polygon* pane (list x y x (+ y 10) (+ x 10) (+ y 5))))))))
                   (stream-increment-cursor-position pane 20 0)
                   (funcall printer node pane))
                 (dolist (printer tabular-printers)
                   ;; very ugly code
                   (let* ((printer (a:ensure-list printer))
                          (opts (cdr printer))
                          (align-x (getf opts :align-x :left))
                          (align-y (getf opts :align-y :baseline))
                          (min-width (getf opts :min-width 1))
                          (min-height (getf opts :min-height 1))
                          (record-type (getf opts :record-type 'standard-cell-output-record)))
                     (formatting-cell (pane :align-x align-x
                                            :align-y align-y
                                            :min-width min-width
                                            :min-height min-height
                                            :record-type record-type)
                                     (funcall (car printer) node pane)))))
               (when (expanded node)
                 (dolist (node (funcall inferior-producer node))
                   (print-node node (+ indent 20))))))
      (dolist (node (funcall (treeview-pane-roots pane) frame))
        (formatting-table (pane :x-spacing 50 :equalize-column-widths t)
          (print-node node 0))))))
