(deftype token ()
  `(cons keyword t))

(defun tok (type &optional val)
  (cons type val))

(alexa:define-string-lexer qlexer
  "Make a lexical analyzer for QASM expressions."
  ((:num "\\d+")
   (:name "[A-Za-z][A-Za-z0-9_]*"))
  ("{{NAME}}" (return (tok :variable (intern $@))))
  ("{{NUM}}"  (return (tok :number (parse-integer $@))))
  ("\\("      (return (tok :left-paren)))
  ("\\)"      (return (tok :right-paren)))
  ("\\s+"     nil))

(defun lex-line (string)
  "Returns tokens of string"
  (loop :with lexer := (qlexer string)
        :for tok := (funcall lexer)
        :while tok
     :collect tok))



