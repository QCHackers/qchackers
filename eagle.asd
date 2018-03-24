(asdf:defsystem #:eagle
  :description "Quantum programming language implementation"
  :author "Victory Omole"
  :license "MIT"
  :depends-on (#:alexa)
  :serial t
  :components ((:file "compiler/parser")
               (:file "compiler/compiler")))
