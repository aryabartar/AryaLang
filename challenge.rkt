;; PL Project - Fall 2018

;; NUMEX interpreter



#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file



;; definition of structures for NUMEX programs



;; CHANGE add the missing ones


(struct munit   ()      #:transparent)
(struct var  (string) #:transparent)
(struct num  (int)    #:transparent)  
(struct bool (boolean)  #:transparent)
(struct apair (e1 e2) #:transparent)

(struct plus  (e1 e2)  #:transparent) 
(struct minus (e1 e2) #:transparent)
(struct mult (e1 e2) #:transparent)
(struct div (e1 e2) #:transparent)
(struct neg (e) #:transparent)


(struct andalso (e1 e2) #:transparent)
(struct orelse (e1 e2) #:transparent)

(struct cnd (e1 e2 e3) #:transparent)
(struct iseq (e1 e2) #:transparent)
(struct ismunit (e) #:transparent)
(struct ifnzero (e1 e2 e3) #:transparent)
(struct ifleq (e1 e2 e3 e4) #:transparent)

(struct lam  (s1 s2 e) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (e1 e2)       #:transparent) ;; function application
(struct with (s e1 e2) #:transparent)
(struct letrec (s1 e1 s2 e2 e3) #:transparent)

(struct 1st (e) #:transparent)
(struct 2nd (e) #:transparent)

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 


(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r



;; Problem 1



(define (racketlist->numexlist xs)
  (if (null? xs)
      (munit)
      (apair (car xs) (racketlist->numexlist (cdr xs)))))


(define (numexlist->racketlist xs)
  (if (munit? xs)
      null
      (cons (apair-e1 xs) (numexlist->racketlist  (apair-e2 xs)))))



;; Problem 2



;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(eq? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]
        )
 )


;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.

(define (eval-under-env e env)
  (cond
    ;; Values
    [(string? e) (envlookup env e)]
    
    [(num? e) (if (integer? (num-int e))
                  e
                  (error "Not a valid num expression")
                  )]

    [(bool? e) (if (boolean? (bool-boolean e))
                   e
                   (error "Not a valid bool expression")
                   )]
    [(closure? e) e]

    [(apair? e)
     (let ([v1 (eval-under-env (apair-e1 e) env)]
           [v2 (eval-under-env (apair-e2 e) env)])
       (apair v1 v2))]

    [(munit? e) e]

                                       
    [(var? e) (envlookup env (var-string e))]

        
    ;; Arithmetic
    [(plus? e) 
     (let ([v1 (eval-under-env (plus-e1 e) env)]
           [v2 (eval-under-env (plus-e2 e) env)])
       (if (and (num? v1)
                (num? v2))
           (num (+ (num-int v1) 
                   (num-int v2)))
           (error "NUMEX addition applied to non-number")))
     ]

    [(minus? e) 
     (let ([v1 (eval-under-env (minus-e1 e) env)]
           [v2 (eval-under-env (minus-e2 e) env)])
       (if (and (num? v1)
                (num? v2))
           (num (- (num-int v1) 
                   (num-int v2)))
           (error "NUMEX subtraction applied to non-number")))
     ]

    [(mult? e) 
     (let ([v1 (eval-under-env (mult-e1 e) env)]
           [v2 (eval-under-env (mult-e2 e) env)])
       (if (and (num? v1)
                (num? v2))
           (num (* (num-int v1) 
                   (num-int v2)))
           (error "NUMEX multiplication applied to non-number")))
     ]

    [(div? e) 
     (let ([v1 (eval-under-env (div-e1 e) env)]
           [v2 (eval-under-env (div-e2 e) env)])
       (if (and (num? v1)
                (num? v2))
           (num (truncate (/ (num-int v1) 
                             (num-int v2))))
           (error "NUMEX division applied to non-number")))
     ]
        
    [(neg? e)
     (let ([v (eval-under-env (neg-e e) env)])
       (cond [(num? v) (num (* (num-int v) -1))]
             [(bool? v) (bool (not (bool-boolean v)))]
             [#t (error "NUMEX neg applied to non-number or non-bool")]))]

        
    ;;Logical
    [(andalso? e)
     (let ([v1 (eval-under-env (andalso-e1 e) env)])
       (if (bool? v1)
           (if (bool-boolean v1)
               (let ([v2 (eval-under-env (andalso-e2 e) env)])
                 (if (bool? v2)
                     v2
                     (error "NUMEX andalso applied to non-boolean")))
               (bool #f)
               )
           (error "NUMEX andalso applied to non-boolean")))
     ]
        
    [(orelse? e)
     (let ([v1 (eval-under-env (orelse-e1 e) env)])
       (if (bool? v1)
           (if (bool-boolean v1)
               (bool #t)
               (let ([v2 (eval-under-env (orelse-e2 e) env)])
                 (if (bool? v2)
                     v2
                     (error "NUMEX andalso applied to non-boolean")))
               )
           (error "NUMEX orelse applied to non-boolean")))
     ]

    ;;Condition
    [(cnd? e)
     (let ([v (eval-under-env (cnd-e1 e) env)])
       (if (bool? v)
           (if (bool-boolean v)
               (eval-under-env (cnd-e2 e) env)
               (eval-under-env (cnd-e3 e) env))
           (error "NUMEX cnd condition is not bool")))]

    [(iseq? e)
     (let ([v1 (eval-under-env (iseq-e1 e) env)]
           [v2 (eval-under-env (iseq-e2 e) env)])
       (cond [(and (num? v1) (num? v2)) (bool (= (num-int v1) (num-int v2)))]
             [(and (bool? v1) (bool? v2)) (bool (eq? (bool-boolean v1) (bool-boolean v2)))]
             [(and (munit? v1) (munit? v2)) (bool #t)]
             [#t (bool #f)]))]

    [(ismunit? e)
     (let ([v (eval-under-env (ismunit-e e) env)])
       (if (munit? v)
           (bool #t)
           (bool #f)))]

    [(ifnzero? e)
     (let ([v (eval-under-env (ifnzero-e1 e) env)])
       (if (num? v)
           (if (eq? (num-int v) 0)
               (eval-under-env (ifnzero-e3 e) env)
               (eval-under-env (ifnzero-e2 e) env))
           (error "NUMEX ifnzero condition is not num")))]

    [(ifleq? e)
     (let ([v1 (eval-under-env (ifleq-e1 e) env)]
           [v2 (eval-under-env (ifleq-e2 e) env)])
       (if (and (num? v1) (num? v2))
           (if (<= (num-int v1) (num-int v2))
               (eval-under-env (ifleq-e3 e) env)
               (eval-under-env (ifleq-e4 e) env))
           (error "NUMEX ifleq e1 or e2 is not num")
           ))]
               

    ;; Functions
    [(lam? e)
     (let ([v1 (lam-s1 e)]
           [v2 (lam-s2 e)])
       (if (and (or (string? v1) (null? v1)) (string? v2))
           (closure env e)
           (error "NUMEX lam name or arg is not string/null")))]

    [(apply? e)
     (let ([clo (eval-under-env (apply-e1 e) env)]
           [arg (eval-under-env (apply-e2 e) env)])
       (if (closure? clo)
           (let ([clo-env (closure-env clo)]
                 [clo-fun (closure-f clo)])
             (if (null? (lam-s1 clo-fun))
                 (eval-under-env (lam-e clo-fun) (list* (cons (lam-s2 clo-fun) (eval-under-env arg env)) clo-env))
                 (eval-under-env (lam-e clo-fun) (list* (cons (lam-s2 clo-fun) (eval-under-env arg env)) (cons (lam-s1 clo-fun) clo) clo-env))))
           (error "NUMEX apply applied to non-function values")))]

    [(with? e)
     (let ([v (eval-under-env (with-e1 e) env)])
       (eval-under-env (with-e2 e) (list* (list* (with-s e) v) env)))]

    ;; [(letrec? e)
    ;; (let* ([menv (list* (cons (letrec-s1 e) (letrec-e1 e)) (cons (letrec-s2 e) (letrec-e2 e)) env])
    ;;  (eval-under-env (letrec-e3 e) menv))
    #|
        [(letrec? e)
         (if (string? (letrec-s1 e))
             (if (string? (letrec-s2 e))
                 (let(
                      [e1 (letrec-e1 e)]
                      [e2 (letrec-e2 e)]
                      )
                   (let(
                        [record_env (append (list (cons (letrec-s1 e) e1) (cons (letrec-s2 e) e2)) env)]
                        )
                     (print record_env)
                     (eval-under-env (letrec-e3 e) record_env)
                     )
                   )
                 (error "NUMEX letrec s2 should be of type racket string")
                 )
             (error "NUMEX letrec s1 should be of type racket string")
             )
         ]
|#
    ;; Pair
    [(1st? e)
     (let ([v (eval-under-env (1st-e e) env)])
       (if (apair? v)
           (apair-e1 v)
           (error "NUMEX first applied to non-apair values")))]
         
    [(2nd? e)
     (let ([v (eval-under-env (2nd-e e) env)])
       (if (apair? v)
           (apair-e2 v)
           (error "NUMEX second applied to non-apair values")))]


    ;; Record
    [(key? e) e]

    [(record? e)
     (let ([evaledK (eval-under-env (record-k e) env)]
           [evaledR (eval-under-env (record-r e) env)])
       (if (and (key? evaledK) (or (munit? evaledR) (record? evaledR)))
           e
           (error "NUMEX record type error")))]

    [(value? e)
     (let ([evaledR (eval-under-env (value-r e) env)])
       (if (record? evaledR)
           (if (eq? (value-s e) (key-s (record-k (value-r e))))
               (eval-under-env (key-e (record-k (value-r e))) env)
               (if (munit? (record-r (value-r e)))
                   (munit)
                   (eval-under-env (value (value-s e) (record-r (value-r e))) env)))
           (error "NUMEX value type error")))]

    [(fun-challenge? e) (closure (short-env env (fun-challenge-freevars e)) e)]
    
    ;; Else
    [#t (error (format "bad NUMEX expression ~v" e))]
   )
)



(define (eval-exp e)
  (eval-under-env e null))







;; Problem 3



(define (ifmunit e1 e2 e3) (cnd (iseq (munit) e1) e2 e3))

(define (with* bs e)
                (cond [(null? bs) e]
                      [#t (with (car (car bs)) (cdr (car bs)) (with* (cdr bs) e))]))

(define (ifneq e1 e2 e3 e4) (cnd (iseq e1 e2) e4 e3))





;; Problem 4


(define numex-filter (lam "map" "f" (lam null "list"
                                         (ifmunit (var "list")
                                                  (munit)
                                                  (let ([first (apply (var "f") (1st "list"))]
                                                        [second (apply (apply (var "map") (var "f")) (2nd (var "list")))])
                                                    (if (num (apply (var "f") (1st "list")))
                                                        (cnd (iseq first (num 0)) second (apair first second))
                                                        (print (apply (var "f") (1st "list")))))))))

(define numex-find-min (lam "find-min" "list" (ifmunit "list"
                                           (num 999999) ; Max val
                                           (with "next" (apply (var "find-min") (2nd (var "list")))
                                                 (let ([firstval (1st (var "list"))])
                                                   (ifleq (var "next") firstval (var "next") firstval))))))

     

(define curryplus (lam null "a" (lam null "b" (plus "a" "b"))))
(define curryminus (lam null "a" (lam null "b" (minus "b" "a"))))


(define numex-all-gt 
  (lam "all-gt" "i"
       (lam null "list"
            (with "min-val" (apply numex-find-min "list")
                  (ifmunit "min-val"
                           (munit)
                           (ifleq "min-val" "i"
                                  (apply (apply "all-gt" "i") (apply (apply numex-filter (apply curryplus "min-val")) (apply (apply numex-filter (apply curryminus "min-val")) "list")))
                                  "list"))))))




;; #4

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

(define (short-env env freevars)
  (if (set-empty? freevars)
      null 
      (let ([k (set-first freevars)]
            [v (find-in-list env (set-first freevars))])
        (list* (cons k v) (short-env env (set-rest freevars)))
        )
      )
  )

(define find-in-list
  (lambda (xs k)
    (if (eq? xs '())
        null
        (if (eq? (car (car xs)) k)
             (cdr (car xs))
             (find-in-list (cdr xs) k)))))

(define (eval-exp-c e)
  (eval-under-env (compute-free-vars e) null))


(define (compute-free-vars e)
     (cond
       [(string? e) e]
       [(num? e) e]
       [(bool? e) e]
       [(var? e) e]
       [(munit? e) e]
       [(closure? e) e]

       [(lam? e)
        (fun-challenge (lam-s2 e) (lam-s1 e) (lam-e e) (get-free (lam-e e) (set-add (set-add null (lam-s1 e)) (lam-s2 e))))]

       [(with? e)
        (with (with-s e) (compute-free-vars (with-e1 e)) (compute-free-vars (with-e2 e)))]
       
       )
  )


(define (get-free e b)
  (cond
        [(string? e) (set)]
        [(num? e) (set)]
        [(bool? e) (set)]
        [(closure? e) (set)]
        [(munit? e) (set)]

        [(apair? e)
         (set-union (get-free (apair-e1 e) b) (get-free (apair-e2 e) b))]
        [(var? e)
         (if (set-member? b (var-string e)) (set) (set (var-string e)))]
        
        [(plus? e)
         (set-union (get-free (plus-e1 e) b) (get-free (plus-e2 e) b))]
        [(minus? e)
         (set-union (get-free (minus-e1 e) b) (get-free (minus-e2 e) b))]
        [(mult? e)
         (set-union (get-free (mult-e1 e) b) (get-free (mult-e2 e) b))]
        [(div? e)
         (set-union (get-free (div-e1 e) b) (get-free (div-e2 e) b))]
        
        [(neg? e)
         (get-free (neg-e e) b)]
        [(andalso? e)
         (set-union (get-free (andalso-e1 e) b) (get-free (andalso-e2 e) b))]
        [(orelse? e)
         (set-union (get-free (orelse-e1 e) b) (get-free (orelse-e2 e) b))]

        [(cnd? e)
         (set-union (set-union (get-free (cnd-e1 e) b) (get-free (cnd-e2 e) b) ) (get-free (cnd-e3 e) b))] 
        [(iseq? e)
         (set-union (get-free (iseq-e1 e) b) (get-free (iseq-e2 e) b))]
        [(ismunit? e)
         (get-free (ismunit-e e) b)]
        [(ifnzero? e)
         (set-union (get-free (ifnzero-e1 e) b) (get-free (ifnzero-e2 e) b) (get-free (ifnzero-e3 e) b))]
        [(ifleq? e)
         (set-union (get-free (ifleq-e1 e) b) (get-free (ifleq-e2 e) b) (get-free (ifleq-e3 e) b) (get-free (ifleq-e4 e) b))]

        [(lam? e)
         (get-free (lam-e e) (set-union b (set-add (set-add null (lam-s1 e)) (lam-s2 e))))]
        [(apply? e)
         (set-union (get-free (apply-e1 e) b) (get-free (apply-e2 e) b))]
        [(with? e)
         (let ([newbound (set-add b (with-s e))])
           (set-union (get-free (with-e1 e) newbound) (get-free (with-e2 e) newbound)))]
        
        [(1st? e)
         (get-free (1st-e e) b)]
        [(2nd? e)
         (get-free (2nd-e e) b)]

        [(key? e)
         (get-free (key-e e) b)]
        [(record? e)
         (set-union (get-free (record-k e) b) (get-free (record-r e) b))]
        [(value? e)
         (get-free (value-r e) b)]
        [(letrec? e)
         (set-union (get-free (letrec-e1 e) b) (get-free (letrec-e2 e) b) (get-free (letrec-e3 e) b))]
        )
  )