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
(defstruct num (n 0) at txt (mu 0) (m2 0)  (heaven 0) (hi -1E30) (lo 1E30))
(defun num! (n &optional (s " ")) 
  (make-num :at n :txt s :heaven (if (eql #\- (charn s)) 0 1)))

(defstruct sym (n 0) at txt has  ok mode (most 0))
(defun sym! (n s) 
  (make-sym :at n :txt s))

(defstruct row cells bins cols)
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
  (let (rows (cols0 (cols! (car src))))
    (dolist (row1 (cdr src)) 
      (setf (o row1 cols) (or (o row1 cols) cols0))
      (add cols0 row1))
    (discretize (make-data :cols cols0 :rows (cdr src)))))

;-------------------------------
(defmacro cell (row1  col)
  `(elt (row-cells ,row1) (o ,col at)))

;---------------------------------------
(defmethod add ((cols1 cols) (row1 row))
  (dolist (cols (list (o cols x) (o cols y)))
    (dolist (col cols) 
      (add col (cell row1 col)))))

(defmethod add ((num1 num) x)
  (unless (eql x '?)
    (with-slots (n  m2 mu lo hi) num1
      (let ((d (- x mu)))
        (incf n)
        (incf mu (/ d n))
        (incf m2 (* d (-  x mu)))
        (setf lo (min lo x)
              hi (max hi x))))))

(defmethod add ((sym1 sym) x)
  (unless (eql x '?)
    (with-slots (n has) num1
      (incf n)
      (when (> (incf (freq x has)))
        (setf mode x
              most (freq x has))))))
;---------------------------------------
(defmethod mid ((num1 num)) (o num1 mu))
(defmethod mid ((sym1 sym)) (o sym1 mode))

(defmethod div ((num1 num)) 
  (with-slots (m2 n) num1 (sqrt (/ m2 (- n 1)))))

(defmethod div ((s sym))
  (with-slots (has n) s
    (* -1  (loop :for (_ . v) :in has :sum  (* (/ v n)  (log (/ v n) 2))))))

(defmethod norm ((num1 num) x)
  (with-slots (lo hi) num
    (if (eql x '?)
      x
      (/ (- x lo) (- hi lo -1E-30)))))
;-----------------------------
(defmethod discretize ((data1 data))
  (dolist (row1 (o data1 rows) data1)
    (dolist (col (o row1 cols x))
      (setf (cell row1 col) 
            (discretize col (cell row1 col))))))

(defmethod discretize ((_    sym) x) x)
(defmethod discretize ((num1 num) x)
  (if (eql x '?)
    x
    (let ((b -1)
          (y (/ (- x (mid num1)) (+ (div num1)  1E-30))))
      (dolist (n (cdr (assoc (? bins) +breaks+)) b)
        (incf b)
        (if (> y n)
          (return-from bin b))))))

(defconstant +breaks+ '(
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

(defun median (seq) (per seq .5))
(defun stdev  (seq) (/ (- (per seq .9) (per seq .1)) 2.56))

(defun per (seq &optional (n .5))
  (elt seq (floor (* n (length seq)) 1)))
;--------------------------------------------------------
(main (? file))
