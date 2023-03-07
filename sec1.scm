;; 1-7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; 1-8
(define (cubert-iter guess x)
  (if (good-enough? guess x)
    guess
    (cubert-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube x) (* x x x))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cubert x)
  (cubert-iter 1.0 x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
	    (+ counter 1))))
  (iter 1 1))

;; 1.10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		      (- kinds-of-coins 1))
		  (cc (- amount
			 (first-denomination kinds-of-coins))
		      kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

;; 1.11
(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1))
		 (* 2 (f (- n 2)))
		 (* 3 (f (- n 3)))) )))

(define (iterative-f n)
  (define (iter a b c count)
    (cond ((= count 0) c)
	  ((< n 3) a)
	  (else (iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (iter 2 1 0 n))

;; 1.12
(define (pascal row col)
  (cond ((= row 0) 1)
	((= col 0) 1)
	((= row col) 1)
	(else (+ (pascal (- row 1) col)
		 (pascal (- row 1) (- col 1))) )))

;; 1.17
(define (fast-expt b n)
  (define (expt-iter a b n)
    (cond ((= n 0) a)
	  ((even? n) (expt-iter a (* b b) (/ n 2)))
	  (else (expt-iter (* a b) (* b b) (/ (- n 1) 2)))))
  (expt-iter 1 b n))

;; 1.18
(define (double x) (* 2 x))
(define (halve x) (/ x 2))
(define (mul a b)
  (cond ((= b 0) 0)
	((even? b) (mul (double a) (halve b)))
	(else (+ (mul (double a) (halve (- b 1))) a))))

(define (mul-lazy a b)
  (if (= b 0)
    0
    (+ a (mul-lazy a (- b 1)))))

;; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (* q q) (* p p))
		   (+ (* q q) (* 2 p q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

;; 1.2.6
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder
	   (square (expmod base (/ exp 2) m))
	   m))
	(else
	  (remainder
	    (* base (expmod base (- exp 1) m))
	    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

;; 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n) ;; search for the three smallest prime numbers greater than n
  (define (iter n count)
    (cond ((= count 0) 0)
	  ((prime? n)
	   (timed-prime-test n)
	   (iter (+ n 2) (- count 1)))
	  (else (iter (+ n 2) count))))
  (if (even? n)
    (iter (+ n 1) 3)
    (iter n 3)))

;; 1.23
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
(define (next x)
  (if (= x 2)
    3
    (+ x 2)))
(define (prime? n)
  (= n (smallest-divisor n)))

;; 1.24
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n) ;; search for the three smallest prime numbers greater than n
  (define (iter n count)
    (cond ((= count 0) 0)
	  ((fast-prime? n 100)
	   (timed-prime-test n)
	   (iter (+ n 2) (- count 1)))
	  (else (iter (+ n 2) count))))
  (if (even? n)
    (iter (+ n 1) 3)
    (iter n 3)))

;; 1.27
(define (fermat-test-full n)
  (define (iter a)
    (cond ((= a n) 0)
	  ((= (expmod a n n) a) (iter (+ a 1)))
	  (else 1)))
  (iter 1))
; (fermat-test-full 561)
; (fermat-test-full 1105)
; (fermat-test-full 1729)
; (fermat-test-full 2465)
; (fermat-test-full 2821)
; (fermat-test-full 6601)

;; 1.28
(define (remainder-square-test n mod)
  (if (and (not (or (= n 1)
		    (= n (- mod 1))))
	   (= (remainder (* n n) mod) 1))
    0
    (remainder (* x x) mod)))
(define (expmod-with-test base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder-square-test (expmod-with-test base (/ exp 2) m)
				m))
	(else
	  (remainder
	    (* base (expmod-with-test base (- exp 1) m))
	    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (Prime? n times)
  (cond ((= times 0) true)
	((miller-rabin-test n) (Prime? n (- times 1)))
	(else false)))

;; 1.29
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))
(define (cube x) (* x x x))

(define (integral f a b n) ; iterative solution
  (define h (/ (- b a) n))
  (define (add-dx x)
    (+ x h))
  (define (sum-iter f a b n i)
    (cond ((> a b) 0)
	  ((or (= i 0) (= i n))
	   (+ (f a)
	      (sum-iter f (+ a h) b n (+ i 1))))
	  ((= (remainder i 2) 1)
	   (+ (* 4 (f a))
	      (sum-iter f (+ a h) b n (+ i 1))))
	  (else
	   (+ (* 2 (f a))
	      (sum-iter f (+ a h) b n (+ i 1)))) ))
  (* (sum-iter f a b n 0) (/ h 3)))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (term x)
    (+ (f x) (* 4 (f (+ x h))) (f (+ x (* 2 h)))))
  (define (next x)
    (+ x (* 2 h)))
  (* (sum term a next (- b (* 2 h))) (/ h 3)))

;; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

;; 1.31
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (inc x) (+ x 1))
(define (identity x) x)
(define (factorial x)
  (product identity 1 inc x))

(define (pi n)
  (define (pi-term n)
    (cond ((even? n) (/ (+ n 2) (+ n 1)))
	  (else (/ (+ n 1) (+ n 2)))))
  (* (product pi-term 1.0 inc n) 4))

;; 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
	      (accumulate combiner null-value term (next a) next b))))
(define (product term a next b)
  (accumulate * 1 term a next b))
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (filtered-accumulate combiner null-value term a next b pred?)
  (cond ((> a b) null-value)
	((pred? a)
	 (combiner (term a)
		   (filtered-accumulate combiner null-value term (next a) next b pred?)))
	(else (filtered-accumulate combiner null-value term (next a) next b pred?))))

(define (sum-prime-square a b) (filtered-accumulate + 0 square a inc b prime?))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (relative-prime? a b)
  (= (gcd a b) 1))

(define (product-of-relative-primes n)
  (define (relative-prime-of-n? x)
    (relative-prime? x n))
  (filtered-accumulate * 1 identity 1 inc n relative-prime-of-n?))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(try next))))
  (try first-guess))

; 1.35
;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 5.0)

; 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(try next))))
  (try first-guess))

; 1.37
(define (cont-frac n d k)
  (define (frac-iter n d i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (frac-iter n d (+ i 1))))))
  (frac-iter n d 1))

(define (cont-frac n d k)
  (define (iter i result)
    (if (< i 1)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 1))

; 1.38
(define (d i)
  (cond ((= (remainder i 3) 2)
	 (* 2 (+ 1 (/ (- i 2) 3))))
	(else 1)))

; (+ 2 (cont-frac (lambda (i) 1.0) d 10))

; 1.39
(define (tan-cf x k)
  (let ((frac (cont-frac (lambda (i) (* x x -1))
			 (lambda (i) (+ (* 2 i) 1))
			 (- k 1))))
    (/ x (+ 1 frac))))

; 1.40
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (cube x) (* x x x))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; 1.41
(define (double f)
  (lambda (x) (f (f x))))

; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; 1.43
(define (repeated f n)
  (cond ((= n 1) f)
	(else (lambda (x) ((compose f (repeated f (- n 1))) x)) )))

; 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (smooth-n f n)
  ((repeated smooth n) f))

; 1.45
(define (average-dump f)
  (lambda (x) (average x (f x))))

(define (pow a n)
  (cond ((= n 1) a)
	((= n 0) 1)
	((even? n) (pow (* a a) (/ n 2)))
	(else (* a (pow (* a a) (/ (- n 1) 2))))))

(define (nth-root x n)
  (fixed-point
    ((repeated average-dump (floor (log n 2)))
    (lambda (t) (/ x (pow t (- n 1)))))
    1.0))

; 1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
      guess
      ((iterative-improve good-enough? improve) (improve guess))) ))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (x) (close-enough? x (f x)))
		      f)
   first-guess))

(define (sqrt x)
  ((iterative-improve (lambda (y) (< (abs (- (square y) x)) 0.001))
		      (lambda (y) (average y (/ x y))))
   1.0))
