(asdf:operate 'asdf:load-op :test-framework)
;; (asdf:operate 'asdf:load-op :cl-match)

(asdf:defsystem :secd
  :description "secd: SECD Machine in Common Lisp."
  :version "0.0.4"
  :author "cranebird <quasi@kc4.so-net.ne.jp>"
  :depends-on (:test-framework)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "compile" :depends-on ("package" "util"))
               (:file "match" :depends-on ("package" "util"))
               (:file "interp" :depends-on ("package" "util"  "compile" "match"))
               (:file "vecinterp" :depends-on ("package" "util"  "compile" "match" "interp"))
               (:file "defmachine" :depends-on ("package" "util" "compile" "interp"))
               (:file "vm" :depends-on ("package" "util"))
               (:file "insn" :depends-on ("package" "vm" "util"))
               (:file "launch" :depends-on ("package" "util" "compile" "vm"))
               (:file "test-secd" :depends-on ("package" "util" "compile" "vm"))
               (:file "graph" :depends-on ("package" "vm"))
               ))

(asdf:load-system :secd)
