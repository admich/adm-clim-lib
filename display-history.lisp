(in-package #:adm-clim-lib)

(defclass history ()
  ())

(defclass pane-with-display-history-mixin ()
  ((undo-list :initform nil)
   (redo-list :initform nil)
   (changing-view-p :initform nil)))

(defclass application-with-display-history-pane (pane-with-display-history-mixin application-pane)
  ())

(defmethod undo-display-history ((pane pane-with-display-history-mixin))
  (with-slots (undo-list redo-list changing-view-p) pane
      (a:when-let ((view (pop undo-list)))
        (setf changing-view-p t)
        (push (stream-default-view pane) redo-list)
        (setf (stream-default-view pane) view)
        (setf changing-view-p nil))))

(defmethod redo-display-history ((pane pane-with-display-history-mixin))
  (with-slots (undo-list redo-list changing-view-p) pane
      (a:when-let ((view (pop redo-list)))
        (setf changing-view-p t)
        (push (stream-default-view pane) undo-list)
        (setf (stream-default-view pane) view)
        (setf changing-view-p nil))))

(defmethod (setf stream-default-view) :around (new-value (stream pane-with-display-history-mixin))
  (with-slots (undo-list redo-list changing-view-p) stream
    (if changing-view-p
        (call-next-method)
        (progn
          (when redo-list
            (dolist (view (reverse redo-list))
              (push view undo-list))
            (setf redo-list nil))
          (push (stream-default-view stream) undo-list)
          (call-next-method)))))


