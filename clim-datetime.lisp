;;;; clim-datetime.lisp

(in-package #:adm-clim-lib)

(define-presentation-type timestamp (&optional low high) :options ((format lt:+iso-8601-format+)))

(define-presentation-method presentation-typep (object (type timestamp))
  (and (or (eq low '*) (lt:timestamp<= low object))
       (or (eq high '*) (lt:timestamp<= object high))))

(define-presentation-method presentation-subtypep ((type timestamp) maybe-supertype)
  (with-presentation-type-parameters (timestamp maybe-supertype)
    (let ((super-low low)
          (super-high high))
      (with-presentation-type-parameters (timestamp type)
        (values
         (and (or (eq super-low '*)  (and (typep low 'timestamp) (lt:timestamp<= super-low low)))
              (or (eq super-high '*)  (and (typep high 'timestamp) (lt:timestamp<= high super-high))))
         t)))))

(define-presentation-method present (object (type timestamp) stream view &key acceptably)
  (declare (ignore view))
  (write-token (format-timestring nil object :format format) stream :acceptably acceptably))

(define-presentation-type date (&optional low high) :options ((format '(:year "-" (:month 2) "-" (:day 2)))) :inherit-from 'timestamp)

(define-presentation-method presentation-typep (object (type date))
  (and (call-next-method)
       (typep object 'date)))

;;;; calendar application
(define-application-frame calendar ()
  ((current-date :initarg :current-date :accessor current-date :initform (today))
   (frame-type :initarg :frame-type :accessor frame-type :initform nil))
  (:pane (make-pane 'application-pane :display-function 'display-pane)))

(defun decrease-month (frame)
  (let ((date (current-date frame)))
    (setf (current-date frame)
          (lt:timestamp- date 1 :month))
    (redisplay-frame-panes frame)))

(defun increase-month (frame)
  (let ((date (current-date frame)))
    (setf (current-date frame)
          (lt:timestamp+ date 1 :month))
    (redisplay-frame-panes frame)))

(defmethod display-pane ((frame calendar) pane)
  (let* ((*standard-output* pane)
         (cdate (current-date frame))
         (cmonth (local-time:timestamp-month cdate))
         (cyear (local-time:timestamp-year cdate))
         (days-in-month (local-time:days-in-month cmonth cyear))
         (start-day (lt:adjust-timestamp
                        (lt:timestamp-minimize-part cdate :day) (:offset :day-of-week :sunday)))
         (stop-day (lt:adjust-timestamp
                       (lt:encode-timestamp 0 0 0 0 days-in-month cmonth cyear) (:offset :day-of-week :saturday)))
         (days (loop :for d = start-day :then (lt:timestamp+ d 1 :day)
                     :while (lt:timestamp<= d stop-day)
                     :collect d)))
    (setf (stream-cursor-position pane) (values 10 10))
    (formatting-table ()
      (formatting-row ()
        (formatting-cell (t :align-x :center)
          (formatting-table ()
            (formatting-row ()
              (formatting-cell (t :align-x :left)
                (with-output-as-gadget (pane)
                  (make-pane 'push-button
                             :label "<"
                             :activate-callback
                             (lambda (gadget)
                               (declare (ignore gadget))
                               (decrease-month *application-frame*)))))
              (formatting-cell (t :align-x :center :min-width 150)
                (format t "~a ~d~%" (elt local-time:+month-names+ cmonth) cyear))
              (formatting-cell (t :align-x :right)
                (with-output-as-gadget (pane)
                  (make-pane 'push-button
                             :label ">"
                             :activate-callback
                             (lambda (gadget)
                               (declare (ignore gadget))
                               (increase-month *application-frame*)))))))))
      (formatting-row ()
        (formatting-cell (t :align-x :center)
          (formatting-table ()
            (formatting-row ()
              (dolist (x '("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa"))
                (formatting-cell (t :align-x :right)
                  (format t "~a" x))))
            (loop :for w :from 0 :below (floor (length days) 7) :do
              (formatting-row ()
                (dotimes (n 7)
                  (let ((date (nth (+ n (* w 7)) days)))
                    (formatting-cell (t :align-x :right)
                      (with-output-as-presentation (t date 'timestamp)
                        (with-drawing-options (pane :ink (if (= cmonth (lt:timestamp-month date))
                                                             +foreground-ink+
                                                             +gray+))
                          (format t "~d" (local-time:timestamp-day date)))))))))))))))

(defun open-calendar (&key (current-date (today)) master-frame new-process)
  (let ((frame (make-application-frame 'calendar
                                       :current-date current-date
                                       :calling-frame master-frame
                                       :width 250
                                       :height 200)))
    (if new-process
        (clim-sys:make-process (lambda () (run-frame-top-level frame)) :name "calendar")
        (run-frame-top-level frame))))

(define-application-frame calendar-choose (calendar)
  ((%result :initform nil :initarg :result :accessor result))
  (:pane (make-pane 'application-pane :display-function 'display-pane
                                      :background +light-slate-grey+)))

(define-calendar-choose-command (com-select-date)
    ((timestamp 'timestamp :gesture :select))
  (setf (result *application-frame*) timestamp)
  (frame-exit *application-frame*))

(define-gesture-name quit-calendar :keyboard (#\q :control))
(define-gesture-name quit-calendar :keyboard (#\g :control) :unique nil)
(define-gesture-name quit-calendar :keyboard (#\c :control) :unique nil)
(define-gesture-name quit-calendar :keyboard :Escape :unique nil)

(define-calendar-choose-command (com-quit :keystroke quit-calendar)
    ()
  (setf (result *application-frame*) nil)
  (frame-exit *application-frame*))

(defmethod clime:find-frame-type ((frame calendar-choose))
  (frame-type frame))

(defun choose-date (&key (current-date (today)) default master-frame top left frame-type)
  (let ((frame (make-application-frame 'calendar-choose
                                       :current-date current-date
                                       :calling-frame master-frame
                                       :event-queue (and master-frame (climi::frame-event-queue master-frame))
                                       :result default
                                       :frame-type frame-type
                                       :top top
                                       :left left
                                       :width 250
                                       :height 200)))
    (run-frame-top-level frame :left left :top top)
    (result frame)))

(defmethod run-frame-top-level :before ((frame calendar-choose) &key left top &allow-other-keys)
  (multiple-value-bind (x y) (pointer-position (port-pointer (port frame)))
    (move-sheet (frame-top-level-sheet frame) (or left x) (or top y))))

(define-presentation-method accept ((type timestamp) stream (view textual-view)
                                                     &key (default nil defaultp)
                                                     (default-type type))
  (labels ((calculate-widget-position ()
             (let ((tr (sheet-delta-transformation stream (graft stream))))
               (multiple-value-bind (x y) (stream-cursor-position stream)
                 (transform-position tr x y)))))
    (let ((*accelerator-gestures* (append *possibilities-gestures*
                                          *accelerator-gestures*)))
      (handler-bind ((accelerator-gesture
                       #'(lambda (c)
                           (let ((gesture (accelerator-gesture-event c)))
                             (when (climi::gesture-match gesture *possibilities-gestures*)
                               (multiple-value-bind (left top) (calculate-widget-position)
                                 (a:when-let ((date (choose-date :master-frame  *application-frame*
                                                                 :left (floor left)
                                                                 :top (floor top)
                                                                 :default default
                                                                 :frame-type :override-redirect)))
                                   (replace-input stream (present-to-string date 'timestamp)))))))))
        (let* ((result (read-token stream))
               (datetime (chronicity:parse result)))
          (if datetime
              (values datetime type)
              (input-not-of-required-type result type)))))))

;; pull request the following in McCLIM
(climi::def-stream-method graft
    ((stream standard-encapsulating-stream)))

(climi::def-stream-method port
    ((stream standard-encapsulating-stream)))

;; TODO/IDEAS:
;; in acceptparse stream and set widget date
;; parameter of presentation (low high)
;; options: format
;; date presentation

