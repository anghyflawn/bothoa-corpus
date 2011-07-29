;;;; bothoa-parser.asd

(asdf:defsystem #:bothoa-parser
  :serial t
  :depends-on (#:cl-ppcre
               #:iterate
	       #:lisp-unit
               #:cl-fad)
  :components ((:file "package")
               (:file "classes-methods")
	       (:file "io")
               (:file "bothoa-parser")
	       (:file "tests")
               (:static-file "test-corpus")
               (:static-file "cb-corpus")
               (:static-file "README")
               (:static-file "LICENSE")))
