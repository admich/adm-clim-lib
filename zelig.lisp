(in-package :adm-clim-lib)

;; In this file is defined ZELIG presentation type. A presentation
;; that inherit from ZELIG have in the presentation menu (right click)
;; not only the commands defined on presentation types but also the
;; commands defined on the presentation types for which it has a
;; translator.
;; It's almost a ugly hack.

(defclass zelig () ())

(define-command-table zelig)

(define-presentation-type zelig ())

(define-presentation-action zelig-menu
    (zelig nil zelig
       :documentation "Menu"
       :menu nil
       :gesture :menu
       :tester ((object presentation frame window x y event)
                (declare (ignore object))
                (find-applicable-translators presentation
                                             *input-context* ; XXX ?
                                             frame window x y
                                             :event event ; XXX ?
                                             :for-menu t
                                             :fastp t)))
    (object presentation frame window x y)
  (declare (ignore object))
  (call-zelig-presentation-menu presentation *input-context*
                          frame window x y
                          :for-menu t
                          :label (format nil "Operation on ~A"
                                         (presentation-type presentation))))


(defun call-zelig-presentation-menu (presentation input-context frame window x y
                               &key (for-menu t) label
                                     &aux (items nil) (processed nil))
  (climi::map-applicable-translators
   #'(lambda (translator presentation context
              &aux (key (cons translator presentation)))
       (unless (member key processed :test #'equal)
        (push key processed)
         (push
          `(,(climi::make-presentation-translator-menu-item :translator translator
                                                            :presentation presentation
                                                            :context context)
             :documentation ,(with-output-to-string (stream)
                               (document-presentation-translator
                                translator
                                presentation
                                input-context
                                frame nil window x y
                                :stream stream)))
          items)))
   presentation input-context frame window x y :for-menu for-menu)
  
  (let ((to-type-translators (remove-if-not (lambda (x)  (and (climi::to-type x) (eql (class-of x) (find-class 'climi::presentation-translator))))
                                            (find-presentation-translators (presentation-type presentation) t (frame-command-table frame)))))
    (loop for to-type-translator in to-type-translators do
         (multiple-value-bind (object ptype context-type)
             (call-presentation-translator to-type-translator presentation (climi::to-type to-type-translator) frame nil window x y)
           (let ((newpre (make-instance 'standard-presentation
                                        :object object 
                                        :type ptype))
                 sub-items)
             (climi::map-applicable-translators
              #'(lambda (translator presentation context
                         &aux (key (cons translator presentation)))
                  (unless (member key processed :test #'equal)
                    (push key processed)
                    (push
                     `(,(climi::make-presentation-translator-menu-item :translator translator
                                                                :presentation presentation
                                                                :context context)
                        :documentation ,(with-output-to-string (stream)
                                          (document-presentation-translator
                                           translator
                                           presentation
                                           input-context
                                           frame nil window x y
                                           :stream stream)))
                     sub-items)))
              newpre input-context frame window nil nil :for-menu for-menu)
             (when  sub-items
               (push (list (format nil "As ~a ->" ptype)
                           :documentation (format nil "Use as ~a" ptype)
                           :style '(nil :italic nil)
                           :items sub-items) items))))))
  (unless items
    (return-from call-zelig-presentation-menu))
  (setq items (nreverse items))
  (multiple-value-bind (item object event)
      (menu-choose
       items
       :label label
       :associated-window window
       :printer #'(lambda (item stream)
                    (let ((object (first item)))
                      (if (climi::presentation-translator-menu-item-p object)
                          (document-presentation-translator
                           (climi::presentation-translator-menu-item-translator object)
                           (climi::presentation-translator-menu-item-presentation object)
                           (climi::presentation-translator-menu-item-context object)
                           frame nil window x y
                           :stream stream)
                          (princ object stream))))
       :pointer-documentation *pointer-documentation-output*)
    (declare (ignore object))
    (when item
      (multiple-value-bind (object ptype options)
          (call-presentation-translator
           (climi::presentation-translator-menu-item-translator item)
           (climi::presentation-translator-menu-item-presentation item)
           (input-context-type (climi::presentation-translator-menu-item-context item))
           frame event window x y)
        (when ptype
          (funcall (cdr (climi::presentation-translator-menu-item-context item))
                   object ptype event options))))))
