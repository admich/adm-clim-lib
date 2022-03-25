(in-package :adm-clim-lib)

(defvar *cl-store-command-table-code* (cl-store:register-code 111 'command-table))

;; Create a custom storing method
;; outputting the code previously registered.
(cl-store:defstore-cl-store (obj command-table stream)
  (cl-store:output-type-code *cl-store-command-table-code* stream)
  (cl-store:store-object (command-table-name obj) stream))

;; Define a restoring method.
(cl-store:defrestore-cl-store (command-table stream)
  (find-command-table (cl-store:restore-object stream)))

(define-application-frame store-history-application-mixin ()
  ((%history-file :initarg :history-file)
   (%restore-history-p :initarg :restore-history-p :initform t)))

(defmethod run-frame-top-level :around ((frame store-history-application-mixin) &key)
  (with-slots ((history climi::presentation-history)
               (store-file %history-file)
               %restore-history-p) frame
    (when %restore-history-p
      (when (uiop:file-exists-p store-file)
        (setf history (cl-store:restore store-file)))))
  (unwind-protect 
       (call-next-method)
    (with-slots ((history climi::presentation-history)
                 (store-file %history-file)) frame
      (cl-store:store history store-file))))
