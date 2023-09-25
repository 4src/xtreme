(defvar *settings* 
  '((about "cutr" ("cutr: to understand 'it',  cut 'it' up, then seek patterns in" 
                   "the pieces. E.g. here we use cuts for multi- objective,"
                   "semi- supervised, rule-based explanation."  
                   "(c) Tim Menzies <timm@ieee.org>, BSD-2 license"))
    (bins      "initial number of bins"     16)
    (bootstrap "bootstraps"                 256)
    (cliffs    "nonparametric small delta"  .147)
    (cohen     "parametric small delta"     .35)
    (file      "read data file"             "../data/auto93.lisp")
    (go        "start up action"            help)
    (help      "show help"                  nil)
    (seed      "random number seed"         1234567891)
    (min       "min size"                   .5)
    (rest      "exapansion best to rest"    3)
    (top       "top items to explore"       10)
    (want      "optimization goal"          plan)))

(defmacro ? (x)
  `(second (cdr (assoc ',x *settings* :test #'equalp))))

(defmacro o (s x &rest xs)
  (if xs `(o (slot-value ,s ',x) ,@xs) `(slot-value ,s ',x)))

(defmacro freq (x lst &optional (init 0))      
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

;--------------------------------------------------------
(defstruct num (n 0) at txt has ok (heaven 0))
(defun num! (n &optional (s " ")) 
  (make-num :at n :txt s :heaven (if (eql #\- (charn s)) 0 1)))

(defstruct sym (n 0) at txt has  ok)
(defun sym! (n s) 
  (make-sym :at n :txt s))

(defstruct row cells bins)
(defun row! (lst) 
  (make-row :cells lst :bins (copy-list lst)))

(defstruct cols names all x y)
(defun cols! (lst &optional (n 0))
  (labels ((goalp (s) (member (charn s) '(#\! #\- #\+))) 
           (col! (&optional (n 0) (s " "))
                 (if (upper-case-p (char0 s)) (num! n s) (sym! n s))))
    (let (x y (all (mapcar (lambda (s) (col! (incf n) s)) lst)))
      (dolist (col all (make-cols :x x :y y :all all :names lst))
        (unless (eql #\X (charn (o col txt)))
          (if (goalp (o col txt)) (push col y) (push col x)))))))

(defstruct data has cols)
(defmethod data! ((src string))
  (data! (mapcar #'row! (with-open-file (s src) (read s)))))

(defmethod data! ((src cons))
  (let ((data1 (make-data :cols (cols! (car src)))))
    (dolist (row1 (cdr src) data1)
      (setf (o row1 data) (or (o row1 data) data1))
      (add data1 row1)
      (push row1 (o data1 has)))))

;-------------------------------
(defmethod cell ((row1 row) col)
  (elt (row-cells row1) (o col at)))

(defmethod has (x) (o x has))
(defmethod has ((num1 num))
  (unless (o num1 ok) (sort has #'<))
  (setf (o num1 ok) t)
  (o num1 has))
;---------------------------------------
(defmethod add ((data1 data) (row1 row))
  (dolist (cols (list (o data1 cols x) (o data1 cols y)))
    (dolist (col cols) 
      (add col (cell row1 col)))))

(defmethod add ((num1 num) x)
  (unless (eql x '?)
    (with-slots (n has ok) num1
      (incf n)
      (push x has)
      (setf ok nil))))

(defmethod add ((sym1 sym) x)
  (unless (eql x '?)
    (with-slots (n has) num1
      (incf n)
      (incf (freq x has)))))
;---------------------------------------
(defmethod mid ((num1 num)) (median (has num1))
(defmethod mid ((sym1 syn)) (node (has sym1))

(defmethod div ((num1 num)) (stdev (has num1))
;---------------------------------------
(defmethod bins ((data1 data))
  (cdr (assoc (? bins) *breaks*))

(defvar *breaks* '(
      (3  -.43	 .43])
      (4  -.67     0	 .67)
      (5  -.84  -.25  .25  .84)
      (6  -.97	-.43    0	 .43  .97)
      (7  -1.07	-.57	-.18	 .18  .57 1.07)
      (8  -1.15	-.67	-.32 	 0	 .32  .67 1.15)
      (9  -1.22	-.76	-.43	-.14	 .14	 .43  .76	1.22)
     (10  -1.28	-.84	-.52	-.25	   0	 .25  .52	 .84	1.28)))

(defun char0 (s) (char s 0))
(defun charn (s) (char s (1- (length s))))

(defun eltn (n) 
  (lambda (lst) (elt lst n)))

(defun col (n lst) 
  (coerce (sort (remove-if (lambda (x) (eq x '?)) (mapcar (eltn n) (cdr lst))) #'<) 'vector))

(defun mode (alist &aux out  (max 0) )
  (loop for (k . v) in alist do (when (> v max) (setf max v out k)))
  out)

(defun median (seq) (per seq .5))
(defun stdev  (seq) (/ (- (per seq .9) (per seq .1)) 2.56))

(defun per (seq &optional (n .5))
  (elt seq (floor (* n (length seq)) 1)))
;--------------------------------------------------------
(main (? file))
