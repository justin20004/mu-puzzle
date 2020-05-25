(java:add-to-classpath #p"/mnt/shared-things-1.0.0-jar-with-dependencies.jar")
(ql:quickload :cl-jena)


(defun theorem-list->alist (lis)
  (mapcar #'(lambda (theorem)
              (cons theorem t))
          lis))


(defun get-fresh-theorems ()
  "from default dataset"
  (mapcar #'(lambda (x)
              (cdar x))
          (jena:sparql-select "select ?theorem
                               where {
                               ?s <implies> ?o .
                               optional {
                               ?o <implies> ?oo .
                               }
                               filter (!bound(?oo)) .
                               bind(substr(str(?o),13)   as ?theorem)
                               }")))




; insert initial theorem
; get-fresh-therems (HT)
; use that HT to derive 1 round of theorems (HT)
; insert those theorems (from HT)
(defun insert-theorems-ht (hashtable)
    (dolist (new-theorem derivations)
      (jena:sparql-update 
        (format nil 
                "insert data {<~A> <implies> <~A>}"
                theorem
                new-theorem))))




(defun insert-derivations (theorem)
  (let* ((derivations (derive-theorems theorem)))
    (dolist (new-theorem derivations)
      (jena:sparql-update 
        (format nil 
                "insert data {<~A> <implies> <~A>}"
                theorem
                new-theorem)))))




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
                (mapcar #'derive-theorems 
                        target))
        :test #'string=)
      (do-n
        (remove-duplicates
          (reduce #'append
                  (mapcar #'derive-theorems 
                          target))
          :test #'string=)
        (- n 1))))



(defun print-it (n)
  (let* ((orig (do-n '("MI") n))
         (found (find "MU" orig
               :test #'string=)))
    (format #.*standard-output* "at ~A orig  len ~A~%" n (length orig))
    (format #.*standard-output* "at ~A found ~A~%" n found)))



; with HT
(defun derive-theorems-ht (theorem &optional hashtable)
  (let* ((ht (if (null hashtable)
                 (make-hash-table)
                 hashtable)))
    (dolist (rule *production-rules*)
                (iter-greed       (rule-regex rule)
                                  (rule-replacement rule)
                                  theorem
                                  ht))
    ht))


(defun derive-theorems (theorem)
  (reduce #'append 
          (remove nil
                  (mapcar #'(lambda (rule)
                              (iter-greed-clean (rule-regex rule)
                                                (rule-replacement rule)
                                                theorem))
                          *production-rules*))))





(defun non-greedy-in-tree? (tree)
  (cond  ((null tree) nil)
         ((listp tree)
          (if (eq (car tree)
                      :NON-GREEDY-REPETITION)
              t
              (or (non-greedy-in-tree? (car tree))
                  (non-greedy-in-tree? (cdr tree)))))))



(defun iter-greed-clean (perl-regex replacement-string target-string)
  (delete-duplicates 
    (iter-greed  perl-regex replacement-string target-string)
    :test #'string=))



(defun iter-greed (perl-regex replacement-string target-string 
                              &optional (hashtable nil))
  "TODO assumes only 1 non greedy repitition clause 
   returns HT of matches"
  (let* ((ht (if (null hashtable)
               (make-hash-table)
               hashtable))
         (tree (ppcre:parse-string perl-regex))
         (has-non-greedy (non-greedy-in-tree? tree)))
    (if has-non-greedy
        (let* ((hit nil)
               (matches nil))
          (loop :for i :from 1
                :while (multiple-value-bind (match match-found)
                         (ppcre:regex-replace 
                           (f tree i)
                           target-string
                           replacement-string)
                         (progn
                           (setf hit match-found)
                           (when match-found
                             ; TODO this assumes a new theorem is only deriable from a single theorem (in the current generation)
                             (setf (gethash match ht) target-string)
                             ;(setf matches (append matches (list match)))
                             )
                         match-found)))
          ht)
        ; just a single match or no match
        (let* ((m nil))
          (when (multiple-value-bind (match match-found)
                  (ppcre:regex-replace perl-regex
                                       target-string
                                       replacement-string)
                  (progn 
                     (setf (gethash match ht) target-string)
                    ;(setf m match)
                  match-found))
            ;(list m)
            ht
            )))))



(defun print-ht (ht)
  (maphash #'(lambda (k v)
               (format t "~A: ~A~%" k v))
           ht))



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



