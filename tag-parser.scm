(load "qq.scm")

(define parse
  (lambda (exp)
    (cond 
    ((constantExp? exp) (parse-constantExp exp))
    ((variableExp? exp) (parse-variableExp exp))
    ((conditionalExp? exp) (parse-conditionalExp exp))
    ((disjunctionExp? exp) (parse-disjunctionExp exp))
    ((lambdaExp? exp) (parse-lambdaExp exp))
	((defineExp? exp) (parse-defineExp exp))
    ((defMITExp? exp) (parse-defMITExp exp))     
    ((assignmentExp? exp) (parse-assignmentExp exp))
    ((applicationExp? exp) (parse-applicationExp exp))
    ((sequenceExp? exp) (parse-sequenceExp exp))
    ((andExp? exp) (parse-andExp exp))
    ((condExp? exp) (parse-condExp exp))
	((letExpr? exp) (let->expand exp))
	((let*Expr? exp) (let*->expand exp))
	((letrecExpr? exp) (letrec->let exp))
	((quasiquoteExpr? exp) (quasi->expand exp))
    (else 'error!)
    )
  )
)

;;;Helper Methods;;;

(define first 
  (lambda (lst)
    (car lst)
  )
)

(define second 
  (lambda (lst)
    (cadr lst)
  )
)

(define third 
  (lambda (lst)
    (caddr lst)
  )
)

(define fourth 
  (lambda (lst)
    (cadddr lst)
  )
)

;;;;;;;;;;;;;;;;;;;;

(define quoted?
  (lambda (exp)
    (and (list? exp) (equal? (car exp) 'quote))
  )
)

(define constantExp?
  (lambda (exp)
    (or (null? exp) (vector? exp) (boolean? exp) (vector? exp) (char? exp) (number? exp) (string? exp) (quoted? exp))
  )
)

(define parse-constantExp
  (lambda (exp)
    (if (quoted? exp)
    (cons 'const (cdr exp))
    (list 'const exp)
    )
  )
)

(define *reserved-words*
  '(and begin cond define do else if lambda
  let let* letrec or quasiquote unquote
  unquote-splicing quote set!))

(define variableExp?
  (lambda (exp)
    (and (symbol? exp) (equal? #f (memq exp *reserved-words*)))
  )
)

(define parse-variableExp
  (lambda (exp)
    (list 'var exp)
  )
)

(define conditionalExp?
  (lambda (exp)
    (and (list? exp) (equal? (car exp) 'if) (or (= 3 (length exp)) (= 4 (length exp))))
  )
)


(define parse-conditionalExp
  (lambda (exp)
    (if (= (length exp) 3)
      (append (cons 'if3 '()) (map parse (cdr exp)) (cons (list 'const (void)) '()))
      (append (cons 'if3 '()) (map parse (cdr exp)))
    )
  )
)

(define disjunctionExp?
  (lambda (exp)
    (and (list? exp) (equal? 'or (car exp)))
  )
)

(define parse-disjunctionExp
  (lambda (exp)
  (cond
    ((= 1 (length exp)) (parse #f))
    ((= 2 (length exp)) (parse (second exp)))
    (else (append (cons 'or '()) (cons (map parse (cdr exp)) '()))))
  )
)

;;;;;;;; lambda - help functions ;;;;;;;

(define improperEnding?
  (lambda (ending)
    (and (not (eq? (cdr ending) '()))
		 (not (pair? (cdr ending))))))

(define improper?
  (lambda (lst)
    (if (null? lst) #f
		(if (improperEnding? lst) #t
			(improper? (cdr lst))))))
			
(define improper->list
    (lambda (implst)
      (if (improperEnding? implst)
          (cons (car implst) (cons (cdr implst) '()))
          (cons (car implst) (improper->list (cdr implst))))))

(define opt-getArgs
    (lambda (lst)
      (reverse (cdr (reverse lst)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lambdaExp?
  (lambda (exp)
    (and (list? exp) (equal? 'lambda (car exp)) (>= (length exp) 3));might not need to check for length of more then 3
  )
)

(define simple-lambdaExp
  (lambda (exp)
    (if (= (length (cddr exp)) 1) 
	  `(lambda-simple ,(cadr exp) ,@(map parse (cddr exp)))
    `(lambda-simple ,(cadr exp) ,(parse `(begin ,@(cddr exp)))))))
	
(define opt-lambdaExp
  (lambda (exp)
	(if (= (length (cddr exp)) 1) 
	`(lambda-opt ,(opt-getArgs (improper->list (second exp))) ,(car (reverse (improper->list (second exp)))) ,@(map parse (cddr exp)))
    `(lambda-opt ,(opt-getArgs (improper->list (second exp))) ,(car (reverse (improper->list (second exp)))) ,(parse `(begin ,@(cddr exp)))))))
	
(define var-lambdaExp
  (lambda (exp)
	(if (= (length (cddr exp)) 1) 
	`(lambda-opt ,(list) ,(second exp) ,@(map parse (cddr exp)))
    `(lambda-opt ,(list) ,(second exp) ,(parse `(begin ,@(cddr exp)))))))


(define parse-lambdaExp
  (lambda (exp)
   (cond ((variableExp? (second exp)) (var-lambdaExp exp))
		 ((list? (second exp)) (simple-lambdaExp exp))
		 ((improper? (second exp)) (opt-lambdaExp exp))
		 (else 'error))))


;;;;;;;;;;;;;;;;;;;;;;;;
;should we allow define to a variable only ? ?????????????????????????????????????????????????TODO
(define defineExp? 
  (lambda (exp)
	(and (list? exp) (equal? 'define (car exp)) (or (= (length exp) 3) (= (length exp) 2)) (variableExp? (second exp)))))

(define defMITExp?
	(lambda (exp)
	  (and (list? exp) (equal? 'define (car exp)) (>= (length exp) 3) (or (list? (second exp))(improper? (second exp)) ) (variableExp? (car (second exp))))))
	  
;(define defineExp?
  ;(lambda (exp)
  ;  (or (regular-define? exp) (MIT-define? exp))))

 (define parse-defineExp
	(lambda (exp)
		(if (null? (cddr exp))
		`(define (var ,(second exp)) ,(list 'const (void)))
		`(define (var ,(second exp)) ,(parse (caddr exp))))))

(define parse-defMITExp
	(lambda (exp)
		(if (improperEnding? (second exp))
					(parse `(define ,(car (second exp)) (lambda  ,(cdr (second exp)) ,@(cddr exp))))
					(parse `(define ,(car (second exp)) (lambda ,(cdr (second exp)) ,@(cddr exp)))))))
;(define parse-defineExp
 ; (lambda (exp)
  ;  (cond ((regular-define? exp) 
	;			`(define (var ,(second exp)) ,(parse (caddr exp))))
	;	  ((MIT-define? exp)
	;			(if (improperEnding? (second exp))
	;				(parse `(define ,(car (second exp)) (lambda  ,(cdr (second exp)) ,@(cddr exp))))
	;				(parse `(define ,(car (second exp)) (lambda ,(cdr (second exp)) ,@(cddr exp))))))
	;	(else 'error))))

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASSIGNMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define assignmentExp?
  (lambda (exp)
    (and (list? exp) (equal? (car exp) 'set!) (= 3 (length exp)))
  )
)

(define parse-assignmentExp
  (lambda (exp)
    (list 'set (list 'var (second exp)) (parse (caddr exp)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Application ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define applicationExp?
  (lambda (exp)
    (and (list ? exp) (>= (length exp) 1) (equal? #f (memq (car exp) *reserved-words*)))))

(define parse-applicationExp
  (lambda (exp)		
    (if (= (length exp) 1)
		`(applic ,(parse (car exp)) ())
		`(applic ,(parse (car exp)) ,(map parse (cdr exp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sequenceExp?
  (lambda (exp)
    (and (list? exp) (equal? 'begin (car exp)))))

;(define parse-sequenceExp
;  (lambda (exp)
 ;   (if (= (length exp) 1)
;		`(seq ,(const (void)))
;		`(seq (,@(map parse (cdr exp))))
;  )
;))
(define begin*->begin
  (lambda (lst)
	(if (null? lst) '()
		(if (sequenceExp? lst)
			(append (map begin*->begin (cdr lst)))
			(parse lst)))))
			
			
;(define parse-sequenceExp
;  (lambda (exp)
;    (if (= (length exp) 1);
		;`(seq ,(const (void)))
		;`(seq (,@(map parse (cdr exp))))
  ;)
;))
(define parse-sequenceExp
  (lambda (exp)
    (if (= (length exp) 1)
		`(seq ,(const (void)))
		`(seq (,@(fold-left (lambda (a b)(if (and (list? b)(list? (car b))) (append a b) (cons a b)))		'() (begin*->begin exp))))
  )
))


(define andExp?
  (lambda (exp)
    (and (list? exp) (equal? 'and (car exp)))
  )
)

(define create-andif-exp 
  (lambda (lst)
    (if (= 1 (length lst)) (car lst)
    (list 'if (car lst) (create-andif-exp (cdr lst)) #f)
    )
  )
)

(define parse-andExp
  (lambda (exp)
    (cond
      ((= 1 (length exp)) (parse #t))
      ((= 2 (length exp)) (parse (second exp)))
      (else
        (parse (create-andif-exp (cdr exp)))
      )
    )
  )
)

(define condExp?
  (lambda (exp)
    (and (list? exp) (equal? 'cond (car exp)))
  )
)

(define create-condif-exp 
  (lambda (lst)
    (let 
        ((unit (car lst)))
        (if (equal? 'else (car unit))
            (append (cons 'begin '()) (cdr unit))
            (if (= 1 (length lst))
              (list 'if (car unit) (append (cons 'begin '()) (cdr unit)))
              (list 'if (car unit) (append (cons 'begin '()) (cdr unit)) (create-condif-exp (cdr lst)))
            )
        )
    )  
  )
)

(define parse-condExp
  (lambda (exp)  
    (let 
      ((lst (cdr exp)))
      (if (null? lst) lst
      (parse (create-condif-exp lst))
      )
    )
  )
)

;;; LET help functions
(define let-body
    (lambda (exp)
      (cddr exp)))

(define let-args
    (lambda (exp)
      (second exp)))

(define let-args->arglist
    (lambda (args)
      (map (lambda (tuple) (car tuple)) args)))

(define let-args->vallist
    (lambda (args)
      (map (lambda (tupel) (if (= (length (cdr tupel)) 1)
                               (cadr tupel)
                               (cdr tupel)
                               )) args)))

(define let->expand
	(lambda (exp)
	  (if (equal? (let-args->vallist (let-args exp)) '())
		`(applic ,(parse `(lambda ,(let-args->arglist (let-args exp)) ,@(let-body exp))) ,(list))
	  (parse 
	  `((lambda ,(let-args->arglist (let-args exp)) ,@(let-body exp)) ,@(let-args->vallist (let-args exp)))))))
;;;;;; LET ;;;;;;


;;TODO add better checks this misses many wrong cases
(define letExpr?
	(lambda (exp)
		(and (list? exp) (>= (length exp) 3) (equal? 'let (car exp))))) 

;;;; LET* ;;;;
(define let*Expr?
  (lambda (exp)
	(and (list? exp) (>= (length exp) 3) (equal? 'let* (car exp))))) 
(define let*->letlist
    (lambda (args body)
      (if (= (length args) 1)
          `(let (,(car args)) ,@body)
          `(let (,(car args)) ,(let*->letlist (cdr args) body)))))


(define let*->expand
  (lambda (exp)
    (if (equal? (let-args->vallist (let-args exp)) '())
	(parse `(let ,(list) ,@(let-body exp)))
    (parse (let*->letlist (let-args exp) (let-body exp))))))
	
	
(define letrec->let
    (lambda (exp)
		(if (equal? (let-args->vallist (let-args exp)) '())
			(parse `(let ,(list) (let ,(list) ,@(let-body exp))))
       (parse `(let ,(map (lambda (x) (list x #f)) (let-args->arglist (let-args exp)))
                (begin ,@(map (lambda (tuple) `(set! ,@tuple)) (let-args exp)) (let ,(list) ,@(let-body exp))))))))

(define letrecExpr?
	(lambda (exp)
		(and (list? exp) (>= (length exp) 3) (equal? 'letrec (car exp)))))  
		
		
(define quasiquoteExpr? 
	(lambda (exp)
		(and (list? exp) (>= (length exp) 2) (equal? 'quasiquote (car exp)))))
		
(define quasi->expand
	(lambda (exp)
		(parse (expand-qq (cadr exp)))))