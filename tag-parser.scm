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
    ((assignmentExp? exp) (parse-assignmentExp exp))
    ((applicationExp? exp) (parse-applicationExp exp))
    ((sequenceExp? exp) (parse-sequenceExp exp))
    (else 'error!)
    )
  )
)


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


(define parse-conditionalExp3
 (lambda (exp)
    (append (cons 'if3 '()) (map parse (cdr exp)) (cons (list 'const (void)) '())) 
  )
)

(define parse-conditionalExp4
 (lambda (exp)
    (append (cons 'if3 '()) (map parse (cdr exp)))
  )
)

(define parse-conditionalExp
  (lambda (exp)
    (if (= (length exp) 3)
      (parse-conditionalExp3 exp)
      (parse-conditionalExp4 exp)
    )
  )
)

(define disjunctionExp?
  (lambda (exp)
    #f
  )
)

(define parse-disjunctionExp
  (lambda (exp)
    'disjunctionExp
  )
)

(define lambdaExp?
  (lambda (exp)
    #f
  )
)

(define parse-lambdaExp
  (lambda (exp)
    'constantExp
  )
)

(define defineExp?
  (lambda (exp)
    #f
  )
)

(define parse-defineExp
  (lambda (exp)
    'defineExp
  )
)

(define assignmentExp?
  (lambda (exp)
    (and (list? exp) (equal? (car exp) 'set!) (= 3 (length exp)))
  )
)

(define parse-assignmentExp
  (lambda (exp)
    (list 'set (list 'var (cadr exp)) (parse (caddr exp)))
  )
)

(define applicationExp?
  (lambda (exp)
    #f
  )
)

(define parse-applicationExp
  (lambda (exp)
    'applicationExp
  )
)

(define sequenceExp?
  (lambda (exp)
    #f
  )
)

(define parse-sequenceExp
  (lambda (exp)
    'constantExp
  )
)

