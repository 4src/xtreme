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

(defun with-read (file) 
  (with-open-file (s file) (read s)))

(defun eltn (n) 
  (lambda (lst) (elt lst n)))

(defun col (n lst) 
  (coerce (sort (remove-if (lambda (x) (eq x '?)) (mapcar (eltn n) (cdr lst))) #'<) 'vector))

(defun median (seq) (per seq .5))
(defun stdev  (seq) (/ (- (per seq .9) (per seq .1)) 2.56))

(defun per (seq &optional (n .5))
  (elt seq (floor (* n (length seq)) 1)))

(let ((data (col 2 (with-read (? file)))))
  (print (median data))
  (print (stdev data)))

(defstruct row cells bins)
