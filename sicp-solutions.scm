;;; Author : Sid Jain <sid26@ucla.edu>

;;; Harold Abelson and Gerald Jay Sussman with Julie Sussman. “Structure and Interpretation of Computer Programs.”

;;; Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (sum-square-greater-two a b c)
  (cond ((and (<= a b) (<= a c)) (+ (* b b) (* c c)))
        ((and (<= b a) (<= b c)) (+ (* a a) (* c c)))
        (else (+ (* a a) (* b b)))))

;;; Exercise 1.7.  The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average a b)
    (/ (+ a b) 2))
  (define (good-enuf? guess)
    (< (abs (- (improve guess) guess)) (* guess 0.0001)))
  (define (sqrt-iter guess)
    (if (good-enuf? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;; Exercise 1.8.  Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value (((x/y^2) + 2y) / 3). Use this formula to implement a cube-root procedure analogous to the square-root procedure.

(define (cbrt x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enuf? guess)
    (< (abs (- (* guess guess guess) x)) (* guess 0.0001)))
  (define (cbrt-iter guess)
    (if (good-enuf? guess)
      guess
      (cbrt-iter (improve guess))))
  (cbrt-iter 1.0))

;;; Exercise 1.11.  A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

;;; Recursive process
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

;;; Iterative process
(define (f n)
  (define (f-iter a b c x)
    (if (= x 2)
      (+ (* a 2) (* b 1) (* c 0))
      (f-iter (+ a b) (+ (* 2 a) c) (* 3 a) (- x 1))))
  (if (< n 3)
    n
    (f-iter 1 2 3 (- n 1))))

;;; Exercise 1.12.  Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

(define (pascal row pos)
  (cond ((or (> pos row) (< row 1) (< pos 1)) 0)
        ((or (= pos 1) (= pos row)) 1)
        (else (+ (pascal (- row 1) (- pos 1)) (pascal (- row 1) pos)))))
