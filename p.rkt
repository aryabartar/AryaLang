;; PL Project - Fall 2018

;; NUMEX interpreter



#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file



;; definition of structures for NUMEX programs



;; CHANGE add the missing ones



(struct var  (string) #:transparent)
(struct num  (int)    #:transparent)  
(struct bool (boolean)  #:transparent)


(struct plus  (e1 e2)  #:transparent) 
(struct minus (e1 e2) #:transparent)
(struct mult (e1 e2) #:transparent)
(struct div (e1 e2) #:transparent)
(struct neg (e) #:transparent)


(struct andalso (e1 e2) #:transparent)
(struct orelse (e1 e2) #:transparent)


(struct cnd (e1 e2 e3) #:transparent)


(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application





(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false



;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 





(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r


(struct letrec (s1 e1 s2 e2 e3) #:transparent) ;; a letrec expression for recursive definitions


(struct apair (e1 e2) #:transparent)


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
        [#t (envlookup (cdr (env)) str)]
        )
 )



;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.

(define (eval-under-env e env)
  (cond
        ;; Values
        [(num? e) (if (integer? (num-int e))
                      e
                      (error "Not a valid num expression")
                      )]

        [(bool? e) (if (boolean? (bool-boolean e))
                       e
                       (error "Not a valid bool expression")
                      )]
        [(closure? e) e]

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
                 [#t (error "NUMEX neg applied to non-number")]))]

        
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
        ;;[(iseq
   )
)



;; Do NOT change

(define (eval-exp e)

  (eval-under-env e null))
                                            
#|

;; Problem 3



(define (ifmunit e1 e2 e3) "CHANGE")



(define (with* bs e2) "CHANGE")



(define (ifneq e1 e2 e3 e4) "CHANGE")



;; Problem 4



(define numex-filter "CHANGE")



(define numex-all-gt

  (with "filter" numex-filter

        "CHANGE (notice filter is now in NUMEX scope)"))



;; Challenge Problem



(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function



;; We will test this function directly, so it must do

;; as described in the assignment

(define (compute-free-vars e) "CHANGE")



;; Do NOT share code with eval-under-env because that will make grading

;; more difficult, so copy most of your interpreter here and make minor changes

(define (eval-under-env-c e env) "CHANGE")



;; Do NOT change this

(define (eval-exp-c e)

  (eval-under-env-c (compute-free-vars e) null))


|#
