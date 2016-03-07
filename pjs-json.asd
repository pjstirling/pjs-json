(asdf:defsystem #:pjs-json
  :serial t
  :depends-on ("pjs-utils" "parenscript")
  :components ((:file "package")
	       (:file "helpers")
               (:file "pjs-json")
	       (:file "parser")))
