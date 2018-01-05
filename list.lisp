;; Conses
(setq x (cons 'a nil)) ; 表达Cons的方式叫做box notation
(car x)
(cdr x)

(setf y (list 'a 'b 'c)) ; flatlist
(cdr y)

(setf z (list 'a (list 'b 'c) 'd))
(car (cdr z))

(defun our-listp (x)
  (or (null x) (consp x)))

(defun our-atom (x)
  (not (consp x))) ; 判断式atom

;;;; nil既是一个原子，也是一个列表。
