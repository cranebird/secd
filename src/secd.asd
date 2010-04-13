(asdf:operate 'asdf:load-op :test-framework)

(defpackage :secd
  (:use :common-lisp :asdf :com.gigamonkeys.test))


(in-package :secd)

(asdf:defsystem :secd
  :description "SECD Machine"
  :version "0.0.1"
  :author "cranebird <quasi@kc4.so-net.ne.jp>"
  :depends-on (:test-framework)
  :components ((:file "util")
               (:file "compile" :depends-on ("util"))
               (:file "vm" :depends-on ("util"))
               (:file "launch" :depends-on ("util" "compile" "vm"))
               (:file "test-secd" :depends-on ("util" "compile" "vm"))
               ;;(:file "tagged-pointer" :depends-on ("package"))
               ))


(asdf:load-system :secd)
