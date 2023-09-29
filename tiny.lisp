(defvar *settings* 
  '((about 
      "mu: less is more"
      "don;t u just love it")
    (seed "-s" posint "random number seed" 1234567891)
    (file "-f" file   "data file"          "../data/auto93.csv")
    (bins "-b" posint "number of bins"     10)
))

(defmacro my (x) `(fifth (assoc ',x  (cdr *settings*))))

(defmacro freq (x lst &optional (init 0))      
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

(defconstant +big+ 1E30)

(defstruct num txt has ok)
(defstruct sym txt has)

(defmethod bin ((s sym) x)
  (with-slots (has) s

(defmethod has ((n num))
  (with-slots (has ok) n
    (unless ok (sort has #'<))
    (setf ok t)
    (values has (elt has 0) (elt has (1- (length has))))))

(defstruct row data cells bins)
(defun row! (a data) (make-row :data data1 :cells a :bins (copy-list a)))

(defun col!(word)
  (if (upper-case-p (elt word 0) (num! word) (sym! word))))

(defstruct cols all)
(defun cols! (words)
   (make-cols :all (mapcar #'col! words)))

(defmethod add ((c cols) lst)
  (mapcar (lambda (col (cols-all c)) (add col cell))  cols lst))

(defstruct data cols rows)
(defun data! (&optional src)
  (let ((data1 (make-data)))
    (if (stringp src)
      (with-csv s (lambda (row1) (add data1 (row! row1 data1))))
      (dolist (row src) (add data1 row)))
    data1))

(defmethod add ((data1 data) row1)
  (with-slots (rows cols) data1
    (cond (cols (push row1 rows)
                (add  cols        (row-cells row1)))
          (t    (setf cols (cols! (row-cells row1)))))))

(defun thing (s)
  (let ((x (read-from-string (string-trim '(#\Space #\Tab) s))))
    (cond ((equal x "?") '?)
          ((numberp x) x)
          (t s))))

(defun with-csv (file fun)
  (labels ((split (s &optional (sep #\,) (here 0))
                  (let ((there (position sep s :start here)))
                    (cons (thing (subseq s here there)) 
                          (if there (split s sep (1+ there)))))))
    (with-open-file (s (or file  *standard-input*))
      (loop (funcall fun (split (or (read-line s nil) (return))))))))

(let ((seed  1234567891)
      (n1    2147483647.0d0)
      (n2    16807.0d0)
      (b     1000000000000.0))
  (defun rand-seed (n) (setf seed n))
  (defun rand      (&optional (n 1)) (setf seed (mod (* n2  seed) n1)) (* n (- 1.0d0 (/ seed n1))))
  (defun rand-int  (&optional (n 1) &aux (base b)) (floor (* n (/ (rand base) base)))))

(with-csv "../data/auto93.csv" #'print)
