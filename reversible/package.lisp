(defpackage #:gates
  (:use :cl)
  (:export :define-bool))

(defpackage #:irreversible
  (:use :cl :gates)
  (:shadow :or :and :not)
  (:export :or :and :not :xor))


(defpackage #:reversible
  (:use :cl :irreversible)
  (:shadow :or :and :not))

(defpackage #:sat
  (:use :cl :reversible)
  (:shadow :or :and :not))
