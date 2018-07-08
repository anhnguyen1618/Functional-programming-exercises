;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist e)
  (if (null? e)
      (aunit)
      (apair (car e) (racketlist->mupllist (cdr e)))))

;; Problem 2
(define (mupllist->racketlist e)
  (if (aunit? e)
      null
      (if (apair? e)
          (cons (apair-e1 e) (mupllist->racketlist (apair-e2 e)))
          (apair e (aunit)))))

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(fun? e)
         (let ([f-name (fun-nameopt e)]
               [result (closure env e)])
           (if (string? f-name)
               (closure (cons (cons f-name result) env) e)
               result))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1)
                      (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL comparation applied to non-boolean")))]
        [(mlet? e)
         (if (string? (mlet-var e))
             (let ([name (mlet-var e)]
                   [val (eval-under-env (mlet-e e) env)])
               (eval-under-env (mlet-body e) (cons (cons name val) env)))
             (error "MUPL variable name has to be string"))]
        [(call? e)
         (let* ([clojure (let ([clojure (eval-under-env (call-funexp e) env)])
                           (if (closure? clojure) clojure (error "MUPL call applied to non-function")))]
                [arg (eval-under-env (call-actual e) env)]
                [fn (closure-fun clojure)]
                [envi (let ([fn-call (call-funexp e)]
                            [clojure-env (closure-env clojure)])
                        (if (var? fn-call)
                            (cons (cons (var-string fn-call) clojure) clojure-env) ;; Add bindings for recursive call
                            clojure-env))]
                [arg-name (fun-formal fn)]
                [fn-body (fun-body fn)])
           (eval-under-env fn-body (cons (cons arg-name arg) envi)))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e) (let ([pair (eval-under-env (fst-e e) env)])
                    (if (apair? pair) (apair-e1 pair) (error "MUPL fst applied to non-pair")))]
        [(snd? e) (let ([pair (eval-under-env (snd-e e) env)])
                    (if (apair? pair) (apair-e2 pair) (error "MUPL snd applied to non-pair")))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        ;;
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x")
                               e4
                               e3))))

;; Problem 4
(define mupl-map
  (fun #f "f"
       (fun "g" "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (apair (call (var "f") (fst (var "xs"))) (call (var "g") (snd (var "xs"))))))))
      
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "y"
             (fun #f "ls"
                  (call (call (var "map") (fun #f "x" (add (var "x") (var "y")))) (var "ls"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (free-vars-helper e not-free)
  ;;(println not-free)
  ;;(print e)
  (let ([f (lambda (g e) (free-vars-helper (g e) not-free))])
    (cond [(fun? e)
           (let* ([arg (fun-formal e)]
                 [body (fun-body e)]
                 [s (free-vars-helper body (set-add not-free arg))])
             (cons (fun-challenge (fun-nameopt e) arg (car s) (cdr s)) (cdr s)))]
          [(var? e)
           (cons e (if (set-member? not-free (var-string e)) (set) (set (var-string e))))]
          [(mlet? e)
           (let* ([name (mlet-var e)]
                  [val (mlet-e e)]
                  [body (mlet-body e)]
                  [s1 (free-vars-helper val not-free)]
                  [s2 (free-vars-helper body (set-add not-free name))])
               (cons (mlet name (car s1) (car s2))
                     (set-union (cdr s1) (cdr s2))))]
          [(add? e)
           (let ([s1 (f add-e1 e)]
                 [s2 (f add-e2 e)])
             (cons (add (car s1) (car s2))
                     (set-union (cdr s1) (cdr s2))))]
          [(ifgreater? e)
           (let ([s1 (f ifgreater-e1 e)]
                 [s2 (f ifgreater-e2 e)]
                 [s3 (f ifgreater-e3 e)]
                 [s4 (f ifgreater-e4 e)])
             (cons (ifgreater (car s1) (car s2) (car s3) (car s4))
                     (set-union (cdr s1) (cdr s2) (cdr s3) (cdr s4))))]
          [(call? e)
           (let ([s1 (f call-funexp e)]
                 [s2 (f call-actual e)])
             (cons (call (car s1) (car s2))
                     (set-union (cdr s1) (cdr s2))))]
          [(apair? e)
           (let ([s1 (f apair-e1 e)]
                 [s2 (f apair-e2 e)])
             (cons (apair (car s1) (car s2))
                     (set-union (cdr s1) (cdr s2))))]
          [(fst? e)
           (let ([s (f fst-e e)])
             (cons (fst (car s)) (cdr s)))]
          [(snd? e)
           (let ([s (f snd-e e)])
             (cons (snd (car s)) (cdr s)))]
          [(isaunit? e)
           (let ([s (f isaunit-e e)])
             (cons (isaunit (car s)) (cdr s)))]
          [#t (cons e (set))])))
        
               
  

(define (compute-free-vars e)
  (car (free-vars-helper e (set))))
  
               
 

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(fun-challenge? e)
         (let ([f-name (fun-challenge-nameopt e)]
               [result (closure env e)])
           (if (string? f-name)
               (closure (cons (cons f-name result) env) e)
               result))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1)
                      (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL comparation applied to non-boolean")))]
        [(mlet? e)
         (if (string? (mlet-var e))
             (let ([name (mlet-var e)]
                   [val (eval-under-env-c (mlet-e e) env)])
               (eval-under-env-c (mlet-body e) (cons (cons name val) env)))
             (error "MUPL variable name has to be string"))]
        [(call? e)
         (let* ([clojure (let ([clojure (eval-under-env-c (call-funexp e) env)])
                           (if (closure? clojure) clojure (error "MUPL call applied to non-function")))]
                [arg (eval-under-env-c (call-actual e) env)]
                [fn (closure-fun clojure)]
                [envi (let ([fn-call (call-funexp e)]
                            [clojure-env (closure-env clojure)])
                        (if (var? fn-call)
                            (cons (cons (var-string fn-call) clojure) clojure-env) ;; Add bindings for recursive call
                            clojure-env))]
                [arg-name (fun-formal fn)]
                [fn-body (fun-body fn)])
           (eval-under-env-c fn-body (cons (cons arg-name arg) envi)))]
        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
        [(fst? e) (let ([pair (eval-under-env-c (fst-e e) env)])
                    (if (apair? pair) (apair-e1 pair) (error "MUPL fst applied to non-pair")))]
        [(snd? e) (let ([pair (eval-under-env-c (snd-e e) env)])
                    (if (apair? pair) (apair-e2 pair) (error "MUPL snd applied to non-pair")))]
        [(isaunit? e) (if (aunit? (eval-under-env-c (isaunit-e e) env)) (int 1) (int 0))]
        ;;
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

             
