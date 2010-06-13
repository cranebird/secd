;;; visualize in graphviz
(in-package :secd.vm)

(defvar *graphviz-dot-program* "/opt/local/bin/dot")

(defun format-type (type)
  ""
  (case type
    (:fixnum "FIXNUM")
    (:pair "PAIR__")
    (:vector "VECTOR")
    (:empty "EMPTY_")
    (:bool-f "BOOL_F")
    (:bool-t "BOOL_T")))

(defun format-val (val)
  (format nil "~8,,,'_@a" val))

(defmethod graphviz-object ((vm vm) stream)
  (format stream "digraph structs {~%")
  (format stream "graph [fontsize=\"9\", fontname=\"monospace\"]~%")
  (format stream "rankdir=LR~%")
  ;;(format stream "edge [sametail,samehead]~%")
  (format stream "node [shape=record, fontsize=9, fontname=\"monospace\", margin=\"0,0\"];~%")
  (format stream "mem [color=orange1,label=\"");

  ;; memory box
  (with-accessors ((mem memory-of) (ap ap-of)) vm
    (loop :for addr :from 0 :to ap :by wordsize
       :do
       (multiple-value-bind (val type) (value-of (read-word mem addr))
         (format stream "{<p~8,'0,,x> ~8,'0,,x| ~a| ~a}" addr addr 
                 (format-type type) (format-val val))
         (unless (= addr ap)
           (format stream " | ")))))
  (format stream "\"];~%")

  ;; pointer
  (with-accessors ((mem memory-of) (ap ap-of)) vm
    (loop :for addr :from 0 :to ap :by wordsize
       :do
       (multiple-value-bind (val type) (value-of (read-word mem addr))
         (when (eql type :pair)
           (format stream "mem:p~8,'0,,x:w -> mem:p~8,'0,,x:w~%"
                   addr val)))))
  ;; SECD pointer
  (format stream "secd [color=indigo,label=\"{{<sp> sp | <env> env | <dump> dump | <ap> ap | <pc> pc }}\"];~%")

  (format stream "secd:sp -> mem:p~8,'0,,x:w;~%" (value-of (sp-of vm)))
;  (format stream "secd:env -> mem:p~8,'0,,x:w;~%" (value-of (env-of vm)))
;  (format stream "secd:dump -> mem:p~8,'0,,x:w;~%" (value-of (dump-of vm)))
;  (format stream "secd:ap -> mem:p~8,'0,,x:w;~%" (ap-of vm))

  ;; code
  ;; (format stream "code [color=navy,label=\"");
  ;; (loop :for c :across (code-of vm) for i from 0
  ;;    :do
  ;;    (progn
  ;;      (format stream "<c~d> ~a" i c) ;; TODO format > to other string. 20100429
  ;;      (unless (= i (- (length (code-of vm)) 1))
  ;;        (format stream " | "))))
  ;; (format stream "\"];~%")
  ;; (format stream "secd:pc -> code:c~d:w;~%" (1- (pc-of vm)))

  ;; S
  ;; pointer

  (format stream "}~%")
  )

(defun graphviz-vm (vm &optional (file "tmp.vm.dot"))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (graphviz-object vm out))
  (sb-ext:run-program *graphviz-dot-program* `("-Tpng" "-O" ,file)))