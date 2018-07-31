(asdf:defsystem #:qchackers
    :description "Lisp system definition for qchackers repo"
    :author "Victory Omole"
    :license "MIT"
    :depends-on (#:alexandria)
    :serial t
    :components ((:file "reversible/package")
		 (:file "reversible/irreversible")
		 (:file "reversible/reversible")
		 (:file "reversible/sat")))

