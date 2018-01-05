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

;; Equality
(eql (cons 'a nil) (cons 'a nil))

(setf x (cons 'a nil))
(eql x x) ; eql只有在它的参数是相同物件时才返回真

(equal x (cons 'a nil)) ; equal若它的参数打印的值相同时，返回真

(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
	   (consp y)
	   (our-equal (car x) (car y))
	   (our-equal (cdr x) (cdr y)))))

;; Why lisp has no pointers
(setf x '(a b c))
(setf y x)
(eql x y)

;; Building Lists
(setf x '(a b c)
      y (copy-list x))

(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst))))) ; x与(copy-list x)会equal

(append '(a b) '(c d))
(append '(a b) '(c d) '(e f))
(append '(a b) 'c)
