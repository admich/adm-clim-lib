;;;; clim-datetime.lisp

(in-package #:adm-clim-lib)

(define-presentation-type timestamp () :options ((format '(:year "-" (:month 2) "-" (:day 2)))))

(define-presentation-method present (object (type timestamp) stream view &key)
  (declare (ignore view))
  (format-timestring stream object :format format))

(define-presentation-method accept ((type timestamp) stream (view textual-view)
                                    &key (default nil defaultp)
                                    (default-type type))
  (let* ((result (read-token stream))
         (datetime (chronicity:parse result)))
    (if datetime
        (values datetime type)
        (input-not-of-required-type result type))))

;;;; [wip] calendar application

(define-application-frame calendar ()
  ((current-date :initarg :current-date :accessor current-date :initform (today)))
  (:pane (make-pane 'application-pane :display-function 'display-pane)))

(defmethod display-pane ((frame calendar) pane)
  (present (current-date frame) 'timestamp :stream pane))

(defun open-calendar (&key (current-date (today)) master-frame)
  (let ((frame (make-application-frame 'calendar :current-date current-date
                                       ;:frame-event-queue (and master-frame (climi::frame-event-queue master-frame))
                                       ;:input-pane (and master-frame (frame-standard-input master-frame))
                                       :calling-frame master-frame
                                       )))
    (clim-sys:make-process (lambda () (run-frame-top-level frame)) :name "calendar")))

