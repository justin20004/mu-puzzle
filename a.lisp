(ql:quickload "cl-ppcre")
(ql:quickload "bordeaux-threads")
(java:add-to-classpath #p"/mnt/shared-things-1.0.0-jar-with-dependencies.jar")
(ql:quickload :cl-jena)
(ql:system-apropos "metering")
(ql:quickload "metering")

(mon:monitor-form (derive-theorems "MI"))
(mon:monitor-form (print-it 8))

(jena:get-default-model)

(jena:sparql-update "insert data {<GEB> <implies> <MI> }")
(jena::print-alists  
  (jena:sparql-select "select * where {?s ?p ?o} limit 4 "))

(jena:sparql-select "select (count(?s) as ?cnt) 
                     where {?s ?p ?o} ") 

(jena:write-dataset-to-file "/mnt/mu1.ttl")

(ql:quickload "alexandria")
(alexandria:alist-hash-table *)
(print-ht *)

; get-fresh-theorems (from graph)
;      -> alist -> HT
(alexandria:alist-hash-table
  (theorem-list->alist '("MI"))
  :test #'equal)
(setf *ht* *)

(let ((ht (make-hash-table :test #'equal)))
  (maphash #'(lambda (k v)
               (derive-theorems-ht k ht))
           *ht*)
  ht)
(print-ht *ht*)
(print-ht *)



(bt:thread-alive-p *thr*)
(setf *thr* (bt:make-thread #'(lambda ()
                    (mapcar #'insert-derivations
                            (get-fresh-theorems)))
                :name 'derive))




                

(bt:make-thread #'(lambda ()
(mon:monitor-form (do-n '("MI") 8)))
                :name 'muu)


; TODO if i keep the theorems in a HT duplicates wont be a problem
(setf *seven* (do-n '("MI") 7))
(length *s2*)
(setf *s2* (append *s2* *s2*))
(time (remove-duplicates *s2* :test #'string= ))

(length (remove-duplicates (do-n '("MI") 7)
        :test #'string=))

(time (find "MU" (do-n '("MI") 7)
      :test #'string=))


(print-it 8)
         

(bt:make-thread #'(lambda ()
                   (mon:monitor-form 
                    (print-it 8)))
                :name 'mu)
(setf thr *)
(bt:thread-alive-p thr)

  
;CL-USER> (time (find "MU" (do-n '("MI") 9)
;      :test #'string=))
;13961.783 seconds real time
;27565864700 cons cells
;NIL
(/ (/ 13961  60.0) 60.0)

(eql "hi" "hi")
(setf *ht* (make-hash-table :test 'equal))
(setf (gethash "MI" *ht*) t)

(bt:make-thread #'(lambda ()
                    (maphash #'(lambda (k v)
                                 (derive-theorems-ht k *ht*))
                             *ht*)
                    (format #.*standard-output* "done~%"))
                :name 'mu-hash)
                

(print-ht *ht*)
(hash-table-size *ht*)
(hash-table-count *ht*)

(derive-theorems-ht "MI")

(let ((rule (car *production-rules*)))
(iter-greed (rule-regex rule)
            (rule-replacement rule)
            "MII" h))
(setf h *)
(mapcar #'print-ht *)


(iter-greed "I$"  "IU"  "MUIIIIIB")
(iter-greed "M(.*)"  "M\\1\\1"  "MIU")
(delete-duplicates 
  (iter-greed "(.*?)UU(.*)"  "\\1\\2"  "MUUBUU") 
  :test #'string=)
(ppcre:regex-replace-all "UU"
                         "MIUUBlalaUUj"
                         "")
                         

(ppcre:regex-replace 
(f (ppcre:parse-string "I$") 0)
"helloII" "IU")


(non-greedy-in-tree? (ppcre:parse-string "(.*?)I$"))


(iter-greed "(.*?)III(.*)"  "\\1U\\2"  "MUIUUIIIIlo" )

(ppcre:scan "s/III//" "heIIIIlo")
(ppcre:all-matches  ".*III.*" "heIIIIlo")




(ppcre:regex-replace (ppcre:parse-string "(.*?)III(.*)")
                     "MIIIU" "\\1U\\2")
(iter-greed "(.*?)III(.*)"  "\\1U\\2"  "MIIIIIUIIIIIU" )


(delete-duplicates (iter-greed "(.*?)III(.*)"  "hellIIIIIlo") :test #'string=)

(loop :for i :in (list 1 2 3 4 nil 8 9)
      :while i
      :collect i)

(ppcre:regex-replace 
  (f '(:SEQUENCE (:REGISTER (:NON-GREEDY-REPETITION 0 nil :EVERYTHING)) "III" (:REGISTER (:GREEDY-REPETITION 0 NIL :EVERYTHING))) 7)
  "hellIIIIIlo"
  "\\1U\\2")




(f '(:SEQUENCE (:REGISTER (:NON-GREEDY-REPETITION 6 NIL :EVERYTHING)) "III" (:REGISTER (:GREEDY-REPETITION 0 NIL :EVERYTHING))))
(setf a *)
(f a 5)
(setf b *)
a
b

(let ((bob '()))
  (cond ((null bob) 12)
        (t 'la)))







(progn
  (setf *scanner* (ppcre:create-scanner 
                    '(:SEQUENCE (:REGISTER (:NON-GREEDY-REPETITION 6 NIL :EVERYTHING)) "III" (:REGISTER (:GREEDY-REPETITION 0 NIL :EVERYTHING)))))
(multiple-value-bind (a b)
  (ppcre:regex-replace *scanner*
                     "heIIIIIIlo"
                     "\\1U\\2")
  b))

  (ppcre:scan-to-strings *scanner* "heIIIIIIIlo")
(ppcre:regex-replace *scanner*
                     "heIIIIIIlo"
                     "\\1U\\2")



                      
(ppcre:parse-string  "(.*?)III(.*)" )
(ppcre:scan-to-strings "(.*?)III(.*)" 
                     "heIIIIIIIlo")

(ppcre:regex-replace "(?g:(.*))III(.*)" 
                     "heIIIIlo"
                     "\\1U\\2")

(ppcre:parse-string  "(.*)III(.*)")
(ppcre:parse-string "(?:ab)??")
(ppcre:parse-string "(.*ab)\\1??")

;;;;;;;;

(quote hi)
(intern "hi")
(intern "bye")

(eq
(intern "HI")
'hi)

(elt (bt:all-threads) 6)

