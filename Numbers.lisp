(require 'cl)

;; Numbers

;;; 1.1 Predicates
(= 23 23)
(/= 23 24)

(> 24 23)
(>= 23 23)
(< 23 24)
(<= 24 24)


;;; T if a < 0, a = 0, or a > 0
(minusp -1)
(zerop 0)
(plusp 1)


;;; T if int is even or odd
(evenp 24)
(oddp 23)

;;; T if foo is of indicated type
;(f numberp foo)
;(f realp foo)
;(f rationalp foo)
;(f floatp foo)
;(f integerp foo)
;(f complexp foo)
;(f random-state-p foo)


;; 1.2 Numeric Functions

;;; Return ∑a or ∏a
(+ 2 3 4 5)
(* 2 3 4 5)

;;; Return a - ∑b or a/∏ b
(- 10 2 3)
(/ 24 2 2)

;;; Return a + 1 or a - 1
(1+ 2)
(1- 3)

;;; Increment or decrement the value of place by delta
;;; Return new value
(setf x 23)
(setq y 24)

(incf x 30)
(incf x)
(incf x 61.4)

(decf y 12)
(decf y)
(decf y 0.1)

;;; Return e^p or b^p
(exp 2)
(expt 2 10)

;;; Return logb a or without b, ln a
(log 10)
(log 10 100)
(log 100 10)
(log 100.0 10)
(log #c(0 1) #c(0 -1))
(log 8.0 2)
(log #c(-16 16) #c(2 2))

;;; √n in complex numbers/natural numbers
(sqrt 9.0)
(sqrt 25)
(isqrt 16)

;;; Least common multiple or greatest common denominator
;;; respectively, of integers. (gcd) returns 0
(lcm 10)
(lcm 1 2 3 4 5)
(lcm -24 18 10)
(lcm 14 35)
(lcm 0 5)

(gcd)
(gcd 60 42)
(gcd 3333 -33 101) =>  1
(gcd 3333 -33 1002001) =>  11
(gcd 91 -49) =>  7
(gcd 63 -42 35) =>  7
(gcd 5) =>  5
(gcd -4)

;;; long-float approximation of pi, Ludolph's number
pi =>  3.141592653589793L0
(cos pi) =>  -1.0L0

(defun sin-of-degrees (degrees)
  (let ((x (if (floatp degrees) degrees (float degrees pi))))
    (sin (* x (/ (float pi x) 180)))))

;;; sin a, cos a, or tan a,(a in radians)
(sin 0) =>  0.0
(cos 0.7853982) =>  0.707107
(tan #c(0 1)) =>  #C(0.0 0.761594)

;;; arcsin a or arccos a, in radians
(asin 0) =>  0.0 
(acos #c(0 1))  =>  #C(1.5707963267948966 -0.8813735870195432)
(/ (atan 1 (sqrt 3)) 6)  =>  0.087266 

;;; arctan a/b in radians
(atan #c(0 2)) =>  #C(-1.5707964 0.54930615)

;;; sinh a, cosh a, or tanh a
(sinh 0) =>  0.0 
(cosh (complex 0 -1)) =>  #C(0.540302 -0.0)

;;; asinh a, acosh a, or atanh a

;;; Return e^ia = cosa+isina
(cis 0) =>  #C(1.0 0.0)

;;; Return complex conjugate of a
(conjugate #c(0 -1)) =>  #C(0 1)
(conjugate #c(1 1)) =>  #C(1 -1)
(conjugate 1.5) =>  1.5
(conjugate #C(3/5 4/5)) =>  #C(3/5 -4/5)
(conjugate #C(0.0D0 -1.0D0)) =>  #C(0.0D0 1.0D0)
(conjugate 3.7) =>  3.7

;;; Greatest or least, of nums
(max 3) =>  3 
(min 3) =>  3
(max 6 12) =>  12 
(min 6 12) =>  6
(max -6 -12) =>  -6 
(min -6 -12) =>  -12
(max 1 3 2 -7) =>  3 
(min 1 3 2 -7) =>  -7
(max -2 3 0 7) =>  7 
(min -2 3 0 7) =>  -2
(max 5.0 2) =>  5.0 
(min 5.0 2)
(max 3.0 7 1)
(min 3.0 7 1)
(max 1.0s0 7.0d0) =>  7.0d0
(min 1.0s0 7.0d0)
(max 3 1 1.0s0 1.0d0)
(min 3 1 1.0s0 1.0d0)

;;; Return as integer or float, n/d rounded, or rounded towards
;;; −∞, +∞, or 0 and remainder
(round .5) =>  0, 0.5
(fround -7 2) =>  -4.0, 1

(floor 3/2) =>  1, 1/2
(ffloor 3 2) =>  1.0, 1
(ffloor -4.7) =>  -5.0, 0.3
(ffloor 3.5d0) =>  3.0d0, 0.5d0

(ceiling 3 2) =>  2, -1
(fceiling 3/2) =>  2.0, -1/2

(truncate 1) =>  1, 0
(truncate .5) =>  0, 0.5
(ftruncate -7 2) =>  -3.0, -1

(dolist (n '(2.6 2.5 2.4 0.7 0.3 -0.3 -0.7 -2.4 -2.5 -2.6))
  (format t "~&~4,1@F ~2,' D ~2,' D ~2,' D ~2,' D"
	  n (floor n) (ceiling n) (truncate n) (round n)))

;;; Same as floor or truncate, but return remainder only
(rem -1 5) =>  -1
(mod -1 5) =>  4
(mod 13 4) =>  1
(rem 13 4) =>  1
(mod -13 4) =>  3
(rem -13 4) =>  -1
(mod 13 -4) =>  -3
(rem 13 -4) =>  1
(mod -13 -4) =>  -1
(rem -13 -4) =>  -1
(mod 13.4 1) =>  0.4
(rem 13.4 1) =>  0.4
(mod -13.4 1) =>  0.6
(rem -13.4 1) =>  -0.4

;;; Return non-negative random number less than limit, and the same type
(<= 0 (random 1000) 1000)  =>  true
(let ((state1 (make-random-state))
      (state2 (make-random-state)))
  (= (random 1000 state1) (random 1000 state2)))

;;; Copy of random-state object state or of the current random state
;;; or a randomly initialized fresh random state
(let* ((rs1 (make-random-state nil))
       (rs2 (make-random-state t))
       (rs3 (make-random-state rs2))
       (rs4 nil))
  (list (loop for i from 1 to 10
	   collect (random 100)
	   when (= i 5)
	   do (setq rs4 (make-random-state)))
	(loop for i from 1 to 10 collect (random 100 rs1))
	(loop for i from 1 to 10 collect (random 100 rs2))
	(loop for i from 1 to 10 collect (random 100 rs3))
	(loop for i from 1 to 10 collect (random 100 rs4))))

;;; Current random state
(random-state-p *random-state*) =>  true
(setq snap-shot (make-random-state))
 ;; The series from any given point is random,
 ;; but if you backtrack to that point, you get the same series.
(list (loop for i from 1 to 10 collect (random))
      (let ((*random-state* snap-shot))
	(loop for i from 1 to 10 collect (random)))
      (loop for i from 1 to 10 collect (random))
      (let ((*random-state* snap-shot))
	(loop for i from 1 to 10 collect (random))))

;;; num-b with num-a's sign
(decode-float .5) =>  0.5, 0, 1.0
(decode-float 1.0) =>  0.5, 1, 1.0
(scale-float 1.0 1) =>  2.0
(scale-float 10.01 -2) =>  2.5025
(scale-float 23.0 0) =>  23.0
(float-radix 1.0) =>  2
(float-sign 5.0) =>  1.0
(float-sign -5.0) =>  -1.0
(float-sign 0.0) =>  1.0
(float-sign 1.0 0.0) =>  0.0
(float-sign 1.0 -10.0) =>  10.0
(float-sign -1.0 10.0) =>  -10.0
(float-digits 1.0) =>  24
(float-precision 1.0) =>  24
(float-precision least-positive-single-float) =>  1
(integer-decode-float 1.0) =>  8388608, -23, 1

;;; Number of magnitude 1 representing sign or phase of n
(signum 0) =>  0
(signum 99) =>  1
(signum 4/5) =>  1
(signum -99/100) =>  -1
(signum 0.0) =>  0.0
(signum #c(0 33)) =>  #C(0.0 1.0)
(signum #c(7.5 10.0)) =>  #C(0.6 0.8)
(signum #c(0.0 -14.7)) =>  #C(0.0 -1.0)
(eql (signum -0.0) -0.0) =>  true

;;; Numerator or denominator, of rational's canonical form
(numerator 1/2) =>  1
(denominator 12/36) =>  3
(numerator -1) =>  -1
(denominator (/ -33)) =>  33
(numerator (/ 8 -6)) =>  -4
(denominator (/ 8 -6)) =>  3

;;; Real part or imaginary part, of number
(realpart #c(23 41)) =>  23
(imagpart #c(23 41.0)) =>  41.0
(realpart #c(23 41.0)) =>  23.0
(imagpart 23.0) =>  0.0

;;; Make a complex number
(complex 0) =>  0
(complex 0.0) =>  #C(0.0 0.0)
(complex 1 1/2) =>  #C(1 1/2)
(complex 1 .99) =>  #C(1.0 0.99)
(complex 3/2 0.0) =>  #C(1.5 0.0)

;;; Angle of num's polar representation
(phase 1) =>  0.0s0
(phase 0) =>  0.0s0
(phase (cis 30)) =>  -1.4159266
(phase #c(0 1)) =>  1.5707964

;;; Return |n|
(abs 0) =>  0
(abs 12/13) =>  12/13
(abs -1.09) =>  1.09
(abs #c(5.0 -5.0)) =>  7.071068
(abs #c(5 5)) =>  7.071068
(abs #c(3/5 4/5)) =>  1 or approximately 1.0
(eql (abs -0.0) -0.0) =>  true

;;; Convert real to rational
;;; Assume complete/limited accuracy for real
(rational 0) =>  0
(rationalize -11/100) =>  -11/100
(rational .1) =>  13421773/134217728 ;implementation-dependent
(rationalize .1) =>  1/10

;;; Convert real into float with type of prototype
(float 0) =>  0.0
(float 1 .5) =>  1.0
(float 1.0) =>  1.0
(float 1/2) =>  0.5

(eql (float 1.0 1.0d0) 1.0d0) =>  true
