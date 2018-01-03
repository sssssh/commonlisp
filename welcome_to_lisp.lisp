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
