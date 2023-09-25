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

(defstruct pretty)
(defmethod print-object ((self pretty))
  (labels
    ((klass-slots (it) 
        #+clisp (clos:class-slots (class-of it)) 
        #+sbcl (sb-mop:class-slots (class-of it)))
     (klass-slot-definition-name (x) 
        #+clisp (clos:slot-definition-name x) 
        #+sbcl (sb-mop:slot-definition-name x))
     (slots (it) (mapcar 'klass-slot-definition-name (klass-slots it))))
    (remove-if (lambda (s) (eql (char (symbol-name s) 0) #\_)) (slots self))))

(defun data! (file)
  (let* ((tmp  (with-open-file (s file) (read s)))
         (data (make-data :cols (cols! (car tmp)))))
    (dolist (row (cdr tmp) data)
      (push (row! row) (o data rows)))))

(defstruct num (n 0) at txt _has ok (heaven 0))
(defun num! (n &optional (s " ")) 
  (make-num :at n :txt s :heaven (if (eql #\- (charn s)) 0 1)))

(defstruct sym (n 0) at txt has  ok)
(defun sym! (n s) 
  (make-sym :at n :txt s))

(defstruct row cells bins)
(defun row! (lst data) 
  (make-row :_data data :cells lst :bins (copy-list lst)))

(defstruct cols names all x y)
(defun cols! (lst &optional (n 0))
  (labels ((goalp (s) (member (charn s) '(#\! #\- #\+))) 
           (col! (&optional (n 0) (s " "))
                 (if (upper-case-p (char0 s)) (num! n s) (sym! n s))))
    (let (x y (all (mapcar (lambda (s) (col! (incf n) s)) lst)))
      (dolist (col all (make-cols :x x :y y :all all :names lst))
        (unless (eql #\X (charn (o col txt)))
          (if (goalp (o col txt)) (push col y) (push col x)))))))


(defstruct data rows cols)
;--------------------------------------------------------
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
