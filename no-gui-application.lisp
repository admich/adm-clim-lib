(in-package #:adm-clim-lib)

(define-application-frame no-gui-application ()
  ())

(defmethod clim:enable-frame :around ((frame no-gui-application))
  (warn "A NO-GUI-APPLICATION can not be enabled. It has no frame."))

(defmethod clim:dispatch-event ((client no-gui-application) event)
  (clim:queue-event client event))

(defmethod clim:queue-event ((client no-gui-application) event)
  (climi::event-queue-append (climi::frame-event-queue client) event))

(defmethod clim:handle-event ((client no-gui-application) (event t))
  (warn "The event ~a is not processed by ~a" event client))
