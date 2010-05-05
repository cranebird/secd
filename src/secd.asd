(asdf:operate 'asdf:load-op :test-framework)

(asdf:defsystem :secd
  :description "secd: SECD Machine in Common Lisp."
  :version "0.0.4"
  :author "cranebird <quasi@kc4.so-net.ne.jp>"
  :depends-on (:test-framework)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "compile" :depends-on ("package" "util"))
               (:file "vm" :depends-on ("package" "util"))
               (:file "insn" :depends-on ("package" "vm" "util"))
               (:file "launch" :depends-on ("package" "util" "compile" "vm"))
               (:file "test-secd" :depends-on ("package" "util" "compile" "vm"))
               (:file "graph" :depends-on ("package" "vm"))
               ))

(asdf:load-system :secd)
