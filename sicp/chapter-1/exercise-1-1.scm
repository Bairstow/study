; Exercise 1.1
10
; 10
(+ 5 3 4)
; 12
(- 9 1)
; 8
(/ 6 2)
; 3
(+ (* 2 4) (- 4 6))
; 6
(define a 3)
; Value: a
(define b (+ a 1))
; Value: b
(+ a b (* a b))
; 19
(= a b)
; #f
(if (and (> b a) (< b (* a b)))
    b
    a)
; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16
(+ 2 (if (> b a) b a))
; 6
(* (cond ((> a b) a)
   ((< a b) b)
   (else -1))
   (+ a 1))
; 16

; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
; -37/150

; Exercise 1.3
(define (square x) (* x x))
(define (smallest x y z) (and (< x y) (< x z)))
(define (larger-squares x y z) (cond ((smallest x y z) (+ (square y) (square z)))
                                     ((smallest y x z) (+ (square x) (square z)))
                                     (else (+ (square x) (square y)))))

; Exercise 1.4
; operator is conditionally set for the final return expression, adding the two operands if b > 0
; and subtracting b from a if b <= 0

; Exercise 1.5
; applicative-order evaluation would in the process of recursively calling through and evaluating
; operands attempt to evaluate (p) and loop at this expression as it is returning itself. whereas
; normal-order evaluation would expand all of the expression elements without evaluation and in the
; process of finding a #t value returned for the if predicate would return only the consequent
; expression of 0 skipping the alternative expression of (p)

; Exercise 1.6
; the special form of the if statement allows this to run while the new-if conditional form will not
; due the different manner in which they handle evaluation. the order of processing for the if
; evaluation is first the predicate and then following and dependent on this the consequent or the
; alternative. however the new-if form will apply applicative-order evaluation to the cond
; combinations, meaning that if the revised guess condition is evaluated it will first attempt to
; evaluate another call to sqrt-iter, which will in turn do the same, etc. leading to a max
; recursion depth error.

; Exercise 1.7
; very small numbers: as we have defined a static precision limit, numbers that are very small (i.e.
; those of similar mag the limit) will iterate in a similar fashion of halving and then terminating
; at the limit.
; very large numbers: the issue of limited precision means that a machine may not be able to
; represent correctly small differences between large numbers, such that a looping condition might
; occur where the return value of good-enough? is always false but the resulting value of improve
; is always the same.
; original strategy:
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt-avg x)
  (sqrt-iter 1.0 x))
; alternative strategy (stop on fraction change):
(define (sqrt-change-iter guess x)
  (if (small-enough? guess x)
    guess
    (sqrt-change-iter (improve guess x)
                      x)))
(define (small-enough? guess x)
  (< (abs (- (improve guess x) guess)) (/ guess 1000)))
(define (sqrt-change x)
  (sqrt-change-iter 1.0 x))
; the alternative strategy is an improvement for larger numbers as the precision limit for finding
; a solution will only ever require accuracy to a relative percentage of the calculated guess.
; avoiding the machine precision errors when dealing with small differences in larger numbers that
; was present in the original solution.

; Exercise 1.8
; approximation of the cube root:
(define (cube x)
  (* x x x))
(define (cbrt-iter guess x)
  (if (good-enough-cb? guess x)
      guess
      (cbrt-iter (improve-cb guess x)
                 x)))
(define (improve-cb guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))
(define (good-enough-cb? guess x)
  (< (abs (- (improve-cb guess x) guess)) (/ guess 1000)))
(define (cb-avg x)
  (cbrt-iter 1.0 x))
