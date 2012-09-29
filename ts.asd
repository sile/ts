(in-package :asdf)

(defsystem :ts
  :name "ts"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "Transport-Stream parser"
  
  :serial t
  :components ((:file "package")
               (:file "packet")
               (:file "parser/package")
               (:file "parser/parser")
               (:file "ts")))
