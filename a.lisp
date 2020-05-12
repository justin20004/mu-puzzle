(asdf:load-system "justinsys")
(ql:system-apropos "nov")
(ql:quickload "cl-ppcre")

(defun bob ())

; mu puzzle  - symbol shunting
;1) I$ -> append U
;2) Mx -> Mxx  e.g. MIU -> MIUIU
;2) M.* -> M&&  e.g. MIU -> MIUIU
;3) xIIIx -> xUx
;3) \(.*\)III\(.*\) -> \1U\2
;4) UU -> nil


(ppcre:scan "s/III//" "heIIIIlo")
(ppcre:all-matches  ".*III.*" "heIIIIlo")


(defun iter-greed (perl-regex replacement-string target-string)
  "TODO assumes only 1 non greedy rep clause"
  (let* ((tree (ppcre:parse-string perl-regex))
         (scanner (f tree 0)))
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
                     a))))

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
