(asdf:defsystem #:sheyn
  :description "A Common Lisp code formatter."
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://yitzchak.github.io/sheyn/"
  :bug-tracker "https://github.com/yitzchak/sheyn/issues"
  :depends-on
    (#:closer-mop
     #-clasp :eclector
     #:trivial-do
     #:trivial-gray-streams)
  :components
    ((:module code
      :serial t
      :components
        ((:file "packages")
         (:file "parser")))))
