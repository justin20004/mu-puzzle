(ql:quickload "cl-ppcre")
(ql:quickload "bordeaux-threads")


; mu puzzle  - symbol shunting
;1) I$ -> IU
;2) Mx -> Mxx  e.g. MIU -> MIUIU
 ;2) M.* -> M&&  e.g. MIU -> MIUIU
;3) xIIIx -> xUx
 ;3) \(.*\)III\(.*\) -> \1U\2
;4) UU -> nil

(setf *production-rules*
      '((1 "I$" "IU")
        (2 "M(.*)" "M\\1\\1")
        (3 "(.*?)III(.*)" "\\1U\\2")
        (4 "(.*?)UU(.*)" "\\1\\2")))

(defun rule-number (rule)
  (car rule))
(defun rule-regex (rule)
  (second rule))
(defun rule-replacement (rule)
  (third rule))


(defun do-n (target n)
  "target is a list -- returns a list"
  (if (= n 1)
      (remove-duplicates
        (reduce #'append
                (mapcar #'get-offspring 
                        target))
        :test #'string=)
      (do-n
        (remove-duplicates
          (reduce #'append
                  (mapcar #'get-offspring 
                          target))
          :test #'string=)
        (- n 1))))

(length (do-n '("MI") 7))

(length (remove-duplicates (do-n '("MI") 7)
        :test #'string=))

(time (find "MU" (do-n '("MI") 6)
      :test #'string=))

(defun print-it (n)
  (let* ((orig (do-n '("MI") n))
         (found (find "MU" orig
               :test #'string=)))
    (format #.*standard-output* "at ~A orig  len ~A~%" n (length orig))
    (format #.*standard-output* "at ~A found ~A~%" n found)))

(print-it 8)
         

(bt:make-thread #'(lambda ()
                    (print-it 9))
                :name 'mu)
(setf thr *)

  
;CL-USER> (time (find "MU" (do-n '("MI") 9)
;      :test #'string=))
;13961.783 seconds real time
;27565864700 cons cells
;NIL
(/ (/ 13961  60.0) 60.0)


(reduce #'append 
(mapcar #'get-offspring
(get-offspring "MIU")))

(defun get-offspring (target)
  (reduce #'append 
          (remove nil
                  (mapcar #'(lambda (rule)
                              (iter-greed-clean (rule-regex rule)
                                                (rule-replacement rule)
                                                target))
                          *production-rules*))))


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

(defun non-greedy-in-tree? (tree)
  (cond  ((null tree) nil)
         ((listp tree)
          (if (eq (car tree)
                      :NON-GREEDY-REPETITION)
              t
              (or (non-greedy-in-tree? (car tree))
                  (non-greedy-in-tree? (cdr tree)))))))

(iter-greed "(.*?)III(.*)"  "\\1U\\2"  "MUIIlo" )

(ppcre:scan "s/III//" "heIIIIlo")
(ppcre:all-matches  ".*III.*" "heIIIIlo")


(defun iter-greed-clean (perl-regex replacement-string target-string)
  (delete-duplicates 
    (iter-greed  perl-regex replacement-string target-string)
    :test #'string=))

(defun iter-greed (perl-regex replacement-string target-string)
  "TODO assumes only 1 non greedy repitition clause"
  (let* ((tree (ppcre:parse-string perl-regex))
         (has-greedy (non-greedy-in-tree? tree)))
    (if has-greedy
        (loop :for i :from 1
              :while (multiple-value-bind (a b)
                       (ppcre:regex-replace 
                         (f tree i)
                         target-string
                         replacement-string)
                       b)
              :collect (multiple-value-bind (a b)
                         (ppcre:regex-replace 
                           (f tree i)
                           target-string
                           replacement-string)
                         a))
        (when (multiple-value-bind (a b)
                (ppcre:regex-replace perl-regex
                                     target-string
                                     replacement-string) b)
          (list 
          (multiple-value-bind (a b)
            (ppcre:regex-replace perl-regex
                                 target-string
                                 replacement-string) a))))))

(delete-duplicates 
  (iter-greed "(.*?)III(.*)"  "\\1U\\2"  "MUIIIIIlo" ) :test #'string=)

(delete-duplicates (iter-greed "(.*?)III(.*)"  "hellIIIIIlo") :test #'string=)

(loop :for i :in (list 1 2 3 4 nil 8 9)
      :while i
      :collect i)

(ppcre:regex-replace 
  (f '(:SEQUENCE (:REGISTER (:NON-GREEDY-REPETITION 0 nil :EVERYTHING)) "III" (:REGISTER (:GREEDY-REPETITION 0 NIL :EVERYTHING))) 7)
  "hellIIIIIlo"
  "\\1U\\2")



(defun f (tree num)
  "TODO -- this mutates the tree"
  (cond  ((null tree) nil)
         ((listp tree)
          (progn
            (when (eq (car tree) 
                      :NON-GREEDY-REPETITION)
              (setf (second tree) num))
            (cons (f (car tree) num)
                  (f (cdr tree) num))))
         (t tree)))

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

(eq
(intern "HI")
'hi)
