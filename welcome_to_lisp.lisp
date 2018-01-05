;; Form
1
(+)
(+2)
(+ 2 3)
(+ 2 3 4)
(+ 2 3 4 5)
(/ (- 7 1) (- 4 3))

;; Evaluation
;; 首先，从左至右对实参求值。
;; 实参的值传入以操作符命名的函数。
(quote (+ 3 5)) ; quote 操作符接受一个实参，并完封不动地返回它。
'(+ 3 5) ; 缩写'等同于quote

;; Data
;;; symbol
'Artichoke ; 通常会被转换为大写，符号不对自身求值

;;; lists
'(my 3 "Sons") ; 列表是由被括号包住的零个或多个元素来表示。
'(the list (a b c) has 3 elements) ; 使用列表必须要引用，不然 Lisp 会以为这是个函数调用
					; 注意引号保护了整个表达式（包含内部的子表达式）被求值。

(list 'my (+ 2 1) "Sons")
(list (+ 2 1) (+ 2 1))
(list '(+ 2 1) (+ 2 1))


;; List Operations
(cons 'a '(b c d)) ; 用函数cons来构造列表
(cons 'a (cons 'b nil))
(list 'a 'b)
(car '(a b c)) ; 对列表取 car 返回第一个元素
(cdr '(a b c)) ; 对列表取 cdr 返回第一个元素之后的所有元素
(car (cdr (cdr '(a b c d))))
(third '(a b c d))
(second '(a b c d))

;; Truth
;;; 符号 t 是表示逻辑真的缺省值
;;; 与nil相同，t也是对自身求值
(listp '(a b c)) ; 如果实参是一个列表，则函数listp返回真

;;; 函数的返回值将会别解释成逻辑真或逻辑假时，则称此函数为谓词(predicate)
;;; 在clisp里，谓词的名字通常以p结尾
(listp 27)
(null nil)
(not nil)

;;; if - 接受三个实参：一个test表达式，一个then表达式和一个else表达式
(if (listp '(a b c))
    (+ 1 2)
    (+ 5 6))

(if (listp 27)
    (+ 1 2)
    (+ 5 6))

;;; if 是特殊的操作符。
(if (listp 27)
    (+ 1 2))

(if 27 1 2)

;;; and or
(and t (+ 1 2))
(and nil (+ 1 2))


;; Functions
(defun our-third (x)
  (car (cdr (cdr x))))

(our-third '(a b c d))

(> (+ 1 4) 3)

(defun sum-greater (x y z)
  (> (+ x y) z))

(sum-greater 1 4 3)


;; Recursion
(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (car lst)))))

;; Input and Output
(format t "~A plus ~A equals ~A. ~%" 2 3 (+ 2 3)) ; ~%表示一个换行

(defun askem (string)
  (format t "~A" string)
  (read)) ;read 是一个完整的 Lisp 解析器(parser)

;; Variables
;;; local variable
(let ((x 1) (y 2)) ; let引入新的局部变量
  (+ x y))

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
	val
	(ask-number))))

;;; global variable
(defparameter *glob* 99)

;;; 全局的常量
(defconstant limit (+ *glob* 1))

;;; 如果你想要检查某些符号，是否为一个全局变量或常量，使用boundp函数
(boundp '*glob*)

;; Assignment
(setf *glob* 98) ; setf 用来给全局或局部变量赋值

(let ((n 10))
  (setf n 2)
  n)

;;; 如果setf的第一个实参是符号(symbol)，且符号不是某个局部变量的名字
;;; 则setf把这个符号设为全局变量：
;;; 不建议这样用
(setf x (list 'a 'b 'c))
(setf (car x) 'n)
(setf a 'b
      c 'd
      e 'f)
(setf a 'b)
(setf c 'd)
(setf e 'f)

;; Functional Programming
(setf lst '(c a r a t))
(remove 'a lst)
lst
(setf x (remove 'a x))

;; Iteration
(defun show-squares (start end)
  (do ((i start (+ i 1))) ; do宏是clisp里最基本的迭代操作符。(variable initial update)
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(show-squares 2 5)

;;; 递归版本
(defun show-squares-1 (i end)
  (if (> i end)
      'done
      (progn ; 接受任意数量的表达式，依序求值，并返回最后一个表达式的值
	(format t "~A ~A~%" i (* i i))
	(show-squares (+ i 1) end))))


(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))


(defun our-length-1 (lst)
  (if (null lst)
      0
      (+ (our-length-1 (cdr lst)) 1)))


;; Functions as Objects
(function +) ; function 是一个特殊操作符
#'+ ; sharp-quote
(apply #'+ '(1 2 3)) ; apply接受一个函数和实参
(apply #'+ 1 2 '(3 4 5))

(funcall #'+ 1 2 3) ; 函数 funcall 做的是一样的事情，但不需要把实参包装成列表

;; lambda
(lambda (x y)
  (+ x y))

((lambda (x) (+ x 100)) 1)

(funcall #'(lambda (x) (+ x 100))
	1) ; lambda表达式前面贴上#'，可以得到对应的函数


;; Types
;; 显示类型(manifest typing)
;; 不需要声明变量的类型，变量可以存放任何类型的对象。
;; 数字27的类型，依普遍性的增加排序, fixnum integer rational real number atom t
;; 类型t是所有类型的基类(supertype)

;;; 函数typep接受一个对象和一个类型，然后判定对象是否为该类型
(typep 27 'integer)


;; Looking Forward

;;; C是拿来写Unix的语言，
;;; Lisp是拿来写Lisp的语言
;;; 你不但在语言之中编程，还是把语言改善成适合程序的语言

;; Summary

;; Exercises

;;; 1
(if (listp 1) (+ 1 2) (+ 3 4))
(list (and (listp 3) t) (+ 1 2))

;;; 2
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))
(cons 'a (cons 'b (cons 'c nil)))

;;; 3
(defun fourth-elements (lst)
  (car (cdr (cdr (cdr lst)))))

;;; 4
(defun bigger (x y)
  (if (> x y) x y))

;;; 5
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
	   (enigma (cdr x)))))

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))

;;; 6
(car (car (cdr '(a (b c) d))))
(or 13 (/ 1 0))
(apply #'list 1 nil)

;;; 7
(defun nest-p (lst)
  (if lst
      (or (listp (car lst))
	  (nest-p (cdr lst)))))

;;; 8
(defun ndots-rep (n)
  (do ((i 0 (+ i 1))) ((= i n))
    (format t ".")))

(defun ndots-rec (n)    
  (if (plusp n)
      (progn
         (format t ".")
         (ndots-rec (- n 1)))))

(defun a-rep (ls)
  (do ((ls1 ls (cdr ls1))
       (n 0 (+ n (if (eq (car ls1) 'a) 1 0))))
      ((not ls1) n)))


(defun a-rec (ls) 
  (if ls
      (+ (if (eq (car ls) 'a) 1 0) (a-rec (cdr ls)))
      0))

;;; 9
(defun summit (lst)
  (apply #'+ (remove nil lst)))

(defun summit (lst)
   (if lst
      (+ (or (car lst) 0) (summit (cdr lst)))
      0))
