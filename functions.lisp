;;;; functions.lisp

;;; lisp程序的三个最基本组成部分就是函数、变量和宏。
;;; &optional &rest

;; 5.1 定义新函数
(defun hello-world ()
  (format t "hello, world"))

(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

;; 5.2 函数形参列表

;; 5.3 可选形参
(defun foo (a b &optional c d)
  (list a b c d))

(defun bar (a &optional (b 10))
  (list a b))

(defun spam (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

;; 5.4 剩余形参

;; 5.5 关键字形参
(defun apple (&key a b c)
  (list a b c))

(defun banana (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(defun orange (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))

;; 5.6 混合不同的形参类型
(defun foo (&rest rest &key a b c)
  (list rest a b c))

;; 5.7 函数返回值
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
	(return-from foo (list i j))))))

;; 5.8 高阶函数
(defun foo (x) (* 2 x))

(defun plot (fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))

;; 5.9 匿名函数
(defun double (x) (* 2 x))
