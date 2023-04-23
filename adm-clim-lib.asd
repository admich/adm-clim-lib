;;;; adm-clim-lib.asd

(asdf:defsystem #:adm-clim-lib
  :description "A collection of tools for CLIM"
  :author "Andrea De Michele"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:mcclim #:local-time #:chronicity #:cl-store #:alexandria)
  :components ((:file "package")
               (:file "clim-datetime")
               (:file "treeview")
               (:file "history")
               (:file "zelig")
               (:file "display-history")
               (:file "no-gui-application")))
