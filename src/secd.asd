(asdf:operate 'asdf:load-op :test-framework)

(asdf:defsystem :secd
  :description "secd: SECD Machine in Common Lisp."
  :version "0.1.0"
  :author "cranebird <quasi@kc4.so-net.ne.jp>"
  :depends-on (:test-framework)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "match" :depends-on ("package" "util"))
               (:file "compile" :depends-on ("package" "util" "match"))
               (:file "interp" :depends-on ("package" "util"  "compile" "match"))
               ;; (:file "vecinterp" :depends-on ("package" "util"  "compile" "match" "interp"))
               ;;(:file "defmachine" :depends-on ("package" "util" "compile" "interp"))
               (:file "defsecd" :depends-on ("package" "util" "compile" "interp"))
               (:file "test-basic" :depends-on ("package" "util" "compile" "vm"))

;; TODO; not implemented yet.               
               ;; (:file "vm" :depends-on ("package" "util"))
               ;; (:file "insn" :depends-on ("package" "vm" "util"))
               ;; (:file "launch" :depends-on ("package" "util" "compile" "vm"))
               ;; (:file "test-secd" :depends-on ("package" "util" "compile" "vm"))
               ;; (:file "graph" :depends-on ("package" "vm"))


               ))

(asdf:load-system :secd)
