(asdf:load-system "justinsys")
(ql:system-apropos "nov")
(ql:quickload "cl-ppcre")

(defun bob ())

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

(rule-replacement
(car *production-rules*))

(let ((target "MIUU"))
  (remove nil
          (mapcar #'(lambda (rule)
                      (list (rule-number  rule)
                      (iter-greed-clean (rule-regex rule)
                                        (rule-replacement rule)
                                        target)))
                  *production-rules*)))


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
