#!/usr/bin/sbcl --script

;;;Load quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "~/quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :eagle)

(defun execute (x)
  "Compiles expressions into specified files"
    (case (length x)
       (1 (error "Specify file to compile"))
       (2  (compiler (second *posix-argv*) "a.eg"))
       (3  (compiler (second *posix-argv*) (third *posix-argv*)))
       (t (error "More than 3 arguments"))))

(execute *posix-argv*)
