
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence lo hi std)
  (if (> lo hi)
      null
      (cons lo (sequence (+ lo std) hi std))))

(define (string-append-map xs s)
  (map (lambda (x) (string-append x s)) xs))

(define (list-nth-mod xs n)
  (cond [(negative? n) (print (negative? n)) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let* ([i (remainder n (length xs))]
                   [tail (list-tail xs i)])
              (car tail))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([e (s)])
        (cons (car e) (stream-for-n-steps (cdr e) (- n 1))))))

(define funny-number-stream
  (lambda ()
    (letrec ([f (lambda (x)
                  (cons
                   (if (= 0 (remainder x 5)) (- x) x)
                   (lambda () (f (+ 1 x)))))])
             (f 1))))

(define dan-then-dog
  (lambda ()
    (letrec ([f (lambda (x) (cons x
                   (lambda () (f (if (string=? "dan.jpg" x) "dog.jpg" "dan.jpg")))))])
             (f "dan.jpg"))))

(define (stream-add-zero s)
  (lambda ()
    (let ([e (s)])
      (cons (cons 0 (car e)) (lambda () ((stream-add-zero (cdr e))))))))

(define (cycle-lists xs ys)
  (lambda()
    (letrec([f (lambda(x)
                 (cons (cons (list-nth-mod xs x) (list-nth-mod ys x))
                       (lambda () (f (+ 1 x)))))])
           (f 0))))

(define (vector-assoc v vec)
  (letrec
      ([size (vector-length vec)]
       [f (lambda (x)
            (if (>= x size)
                #f
                (let ([e (vector-ref vec x)])
                  (if (and (pair? e) (equal? (car e) v))
                      e
                      (f (+ 1 x))))))])
    (f 0)))

(define (cached-assoc xs n)
  (let* (
         [cache (make-vector n #f)]
         [get-cache-val (lambda (i) (vector-assoc i cache))]
         [update-cache (let ([index 0])
                           (lambda (e)
                             (vector-set! cache index e)
                             (set! index (remainder (+ 1 index) n))))])
    (lambda (v)
      (let ([cache-val (get-cache-val v)])
        (or cache-val
            (let ([result (assoc v xs)])
              (begin
                (update-cache result)
                result)))))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([e e1]
              [f (lambda () (if (>= e2 e) #t (f)))])
       (f))]))

          
    






               
         

