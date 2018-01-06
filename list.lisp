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


;; Example:Compression
;;; run-length encoding

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(compress '(1 1 1 0 1 0 0 0 0 1))

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
	    (rest (uncompress (cdr lst))))
	(if (consp elt)
	    (append (apply #'list-of elt)
		    rest)
	    (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(uncompress '((3 1) 0 1 (4 0) 1))

(list-of 3 'ho) ; make-list


;; Access
(nth 0 '(a b c)) ; 找到列表特定位置的元素
(nth 2 '(a b c))
(nthcdr 1 '(a b c)) ; slice

(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
      (our-nthcdr (- n 1) (cdr lst))))

(last '(a b c))
(car (last '(a b c)))

(eql (second '(x y z)) (nth 1 '(x y z)))
(caddr '(x y z)) ; (car (cdr (cdr '(x y z))))


;; Mapping Functions
(mapcar #'(lambda (x) (+ x 10))
	'(1 2 3))

(mapcar #'list
	'(a b c)
	'(1 2 3 4))

(maplist #'(lambda (x) x)
	 '(a b c))


;; Trees
(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
	    (our-copy-tree (cdr tr)))))

(copy-tree '(a (b c) d))
(our-copy-tree '(a (b c) d))

(and (integerp x) (zerop (mod x 2)))

(substitute 'y 'x '(and (integerp x) (zerop (mod x))))
(subst 'y 'x '(and (integerp x) (zerop (mod x 2))))

(defun our-subst (new old tree)
  (if (eql tree old)
      new
      (if (atom tree)
	  tree
	  (cons (our-subst new old (car tree))
		(our-subst new old (cdr tree))))))

;; Understanding Recursion
(defun len (lst)
  (if (null lst)
      0
      (+ (len (cdr lst)) 1)))

(defun our-member (obj lst)
  (if (eql (car lst) obj)
      lst
      (our-member obj (cdr lst))))


;; Sets
(member 'b '(a b c)) ; 返回由尋找物件所開始的那部分

;;; 关键字参数
(member '(a) '((a) (z)) :test #'equal)
(member 'a '((a b) (c d)) :key #'car)

(member 2 '((1) (2)) :key #'car :test #'equal)
(member 2 '((1) (2)) :test #'equal :key #'car)

;;; oddp
(member-if #'oddp '(2 3 4))

(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
	   lst
	   (our-member-if fn (cdr lst)))))

;;; adjoin
(adjoin 'b '(a b c))
(adjoin 'z '(a b c))

;;; union intersection complement
(union '(a b c) '(c b s))

(intersection '(a b c) '(b b c))

(set-difference '(a b c d e) '(b e))

;; Sequences
(length '(a b c))

(subseq '(a b c d) 1 2)
(subseq '(a b c d) 1)

(reverse '(a b c))

;;; palindrome
(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
	 (let ((mid (/ len 2)))
	   (equal (subseq s 0 mid)
		  (reverse (subseq s mid)))))))

(mirror? '(a b b a))

(sort '(0 2 1 3 8) #'>)

;;; 接受一個整數n，返回列表中第n大的元素
(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

(nthmost 2 '(0 2 1 3 8))

(every #'oddp '(1 3 5))
(some #'evenp '(1 2 3))

(every #'> '(1 3 5) '(0 2 4))


;; Stacks
;;; (push obj lst)
;;; (setf lst (cons obj lst))
;;; (pip lst)

;;; (let ((x (car lst)))
;;;   (setf lst (cdr lst))
;;;   x)

(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))

;;; pushnew 宏是push的變種，使用了adjoin而不是cons


;; Dotted Lists
(defun proper-list? (x)
  (or (null x)
      (and (consp x)
	   (proper-list? (cdr x)))))

(setf pair (cons 'a 'b))
(cons 'a (cons 'b (cons 'c 'd)))

'(a . (b . nil))
'(a . (b))
'(a b . nil)
'(a b)
