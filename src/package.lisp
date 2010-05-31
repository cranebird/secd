(defpackage :secd.util
  (:use :common-lisp :cl-match)
  (:export :set-scm-macro-character :as-keyword :with-gensyms
           :mkstr :symb :group :flatten))

(defpackage :secd.compile
  (:use :common-lisp :secd.util)
  (:export :extend-env :lookup :compile-exp
           :compile-pass1
           :compile-pass2))

(defpackage :secd.interp
  (:use :common-lisp :secd.util :secd.compile :cl-match)
  )


(defpackage :secd.vm
  (:use :common-lisp :secd.util)
  (:shadow :cons :car :cdr)
  (:export :make-vm :describe-object :print-object :vm-car :vm-cdr :vm-cons :*debug*
           :*profile* :dispatch :vm
           :vm-stack-top
           :sp-of :get-sp :set-sp
           :ap-of :get-ap :set-ap
           :env-of :get-env :set-env
           :pc-of :get-pc :set-pc
           :code-of :get-code :set-code
           :dump-of :get-dump :set-dump
           :code-ref
           :value-of
           :convert-scheme-obj
           :immediate-rep
           :profile-of :get-profile
           :execution-count-of
           :bool-f
           :bool-t
           :empty
           :fetch-operand
           :graphviz-vm
           :fetch-insn
           :stop-vm-condition
           :unknown-immediate-rep-error
           :allocation-fail-error
           :gc-condition
           :memory-of
           :get-memory
           :from-memory
           :to-memory
           :copying
           ))

(defpackage :secd
  (:use :common-lisp :secd.util :secd.compile :secd.vm :asdf :com.gigamonkeys.test)
  )

(defpackage :secd.test
  (:use :common-lisp :secd))
