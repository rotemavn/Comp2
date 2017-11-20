;;;; credit to asaf halili from 2017 course that made the original tests file
(load "testTagparser.so") 
(define <staff-parser> parse) 
(load "tag-parser.scm")
(define <my-parser> parse)   

(define testVSstaff
	(lambda (input)
		(let ((my-res (<my-parser> input))
		      (staff-res (<staff-parser> input)))
			(display input)
			(display ": ")			
			(cond ((equal? my-res staff-res)
				(display "\033[1;32mSuccess!\033[0m") (newline) #t)
				(else (display "\033[1;31mFailed!\033[0m ") 
					(display ", expected: ")					
					(display staff-res)
					(display ", actual:")
					(display my-res)
					(newline)
					#f))
			)
			
			))

			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "=============")
	(newline)
	(let ((results (map testVSstaff lst)))
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32mSUCCESS!\033[0m\n") (newline) #t)
		(else (display "\033[1;31mFAILED!\033[0m\n") (newline) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!! ALL TESTS SUCCEEDED !!!!\033[0m\n"))
		(else (display "\033[1;31m ##### SOME TESTS FAILED #####\033[0m\n")))
		(newline))
))		

(define constTests
	(list ''() #f #\a 34 "abc" '(quote a) '(quote (a b c)) '(quote (quote a b c)) '1 ''1 
	''()   
    ''#(2)
    ''#(1 (2 3 4) 2 3)
    #f
    #\a  
    34
    "abc"
    '(quote a)
    '(quote (a b c))
    '(quote (quote a b c))
	
	)
)
(define varTests
	(list 'abc '123x 'abc
    '123x
    'AbC1gfj5j959b949jm5b9gk5kg
    'let2*
    'set-bang!)
)
(define conditionalsTests
	(list 	'(if a b c)
			'(if (if a b c)
				'x
					'(x y z))
			'(if a b)
			'(if a 4)
			'(if #t 'abc)
			`(if (if a b c) 'c '(x y z))
    		`(if (if a b c) 'c (if "abc" 'x 123))

	 )
)
(define disjTests
	(list 
		'(or (zero? x) (zero? y) (zero? z))
		'(or (or (f1 x) (f2 y)))
		`(or)
		`(or 1)
		`(or 1 2)
		`(or 1 2 3)
		`(or (or "abc" '123) (if a b c))
		`(or 'shaham)
		`(or "naimark")
	)
)
(define lambdaTests
	(list
		'(lambda (x y z)
			(if x y z))
		'(lambda () a)
		'(lambda (x y z . rest)
			(if x y z))
		'(lambda (x . rest)
			rest)
		'(lambda args
			(if x y z))
		'(lambda args args)
		; lambda-simple
		'(lambda () body)
		'(lambda (x) a)
		'(lambda (exp rest) (if a 1 "abc"))
		'(lambda (a b c d e f Symbol1) E1 E2 E3 E4 (f1 a))
		'(lambda (exp rest) (or a b c) (if a 1 "abc"))
		'(lambda (a b c d e arg154) (if a 1 "abc"))
		'(lambda () (or 1))
		'(lambda (Sym1 Symbol2 Symbol1234567890) (display "Akuna Matata"))       
		
		;lambda-opt
		'(lambda (x y . z) a)
		'(lambda (abc . def) abc)
		'(lambda (exp . rest) (or a b c) (if a 1 "abc"))
		
		;lambda-Variables
		'(lambda args (if a b c))
		'(lambda args args)
		'(lambda args (or a b c) (if a 1 "abc"))
	)
)

(define defTests
	(list 
		'(define x 5)
		'(define x (lambda (x) x))
		'(define (id x) x)
		'(define (foo x y z)
			(if x y z))
		'(define (foo x y . z)
			(if x y z))
		'(define (list . args)
			args)
			 ;regular-define
		'(define a b c d)
		'(define a (b c d))
		
		;mit-style-define
		'(define (id x) x y)
		'(define (foo x y z) (if x y z))
		'(define (foo x y . z) (if x y z) #t)
		'(define (list . args) args)
	)
)


(define assTests
	(list 
		'(set! x 3)
		'(set! v (f x))

	)
)
(define appTests
	(list
		'(a)
		'(a b c)
		'((a b) (a c))
    	'((lambda (x y z . e) (e (f x y z123))))
	)
)
(define seqTests
(list
    '(begin)
    '(begin 1)
    '(begin (or 1 2 3))
    '(begin (or 1 2) (if 1 2 3))   
    '(begin (begin a b) a)
    '(begin (begin a (begin c (begin d e f g))) a (a b c))
    '(begin (begin a b c) (begin d e f) (begin e f) g)
    '(begin (begin a c) b (begin d) g (begin e f) (or 1 2 3))
    '(begin (if (if 1 2 3) (and 2 "a" #f) #t) (begin d e) (lambda args a b c) (begin e f) (or 1 2 3))
    '(begin a (begin b (begin c (begin d e (begin f g h) "Akuna Matata"))))
)
)

(define letTests
(list
    '(let () body)
    '(let ((x 1)) body)
    '(let ((x 1) (y 21)) body)
    '(let ((x 1) (y 21) (abcde fghijKlmnOP123)) body (if (> x 5) 4 3) #t)
    
    '(let* () body)
    '(let* ((x 1) (y 21)) body)
    '(let* ((x 1) (x abv)) body)
    '(let* ((x 1) (y 21) (abcde fghijKlmnOP123)) body (if (> x 5) 4 3) #t)
    
    '(letrec () body)
    '(letrec ((x 1)) b)
    '(letrec ((x 1) (y 2)) b1 b2)
    '(letrec ((x 1) (y 2) (a 5)) b1 b2 (or 1 2 3) (if 1 2 3) #t)
)
)
(define letStarTests)
(define letrecTests)
(define andTests
	(list
	`(and)
    `(and 1)
    `(and 1 2)
    `(and 1 2 3 4)
    `(and 1 2 3 abc)
    `(and 1 2 3 13 4 5 4 7 3)
    `(and (or "abc" '123) (if a b c))
    `(and 'shaham "naimark")
    `(and "naimark")
	)
)
(define condTests
	(list 
		;without sequences     
     `(cond (x 1))
     `(cond (x 1) (c 12))
     `(cond (x 1) (b 2) (c 3))
     
     ; with sequences
     `(cond (x 1 2))
     `(cond (x 1 2 abc) (c 12))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t) (else #f))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t) (else 112))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t) (else (display "I'm in Else") (f a b c)))
	)
)
(define defMITExpTests)
(define quasiTests
(list
    `(quasiquote (a b c))
    `(quasiquote (a ,b c))
    `(quasiquote (a ,b ,@c))    
    `(quasiquote (,@a ,b c))
    `(quasiquote (,@a ,@b ,@c))    
    `(quasiquote (,a ,b ,c))  
    `(quasiquote (,a ,((f a) x) ,c))  
)
)


(define parserTests
  '(4 
	-3 
	#t 
	#f 
	#\a 
	#\A 
	#\space 
	#\tab
	'3 
	3 
	'"abc" 
	"abc" 
	'#\a 
	#\a 
	''a 
	''''''a
	'abc 
	abc 
	'#(a b c) 
	'#() 
	'#(() 
	(#()))
	(a b c)
	(if (zero? n) 1 (* n (fact (- n 1))))
	(cond ((foo? x) foo-x)
		((goo? x) goo-x)
		((boo? x) boo-x)
		(else poo-x))
	(begin e1)
	(begin e1 e2 e3)
	(lambda a b c)
	(lambda a b)
	(lambda (a b . c) (list a b c))
	(lambda (a b c) (list a b c))
	(let ((a 1) (b 2) (c 3))
	  (+ a b c))
	(let* ((a 1)
		   (a (+ a 1))
		   (a (+ a a))
		   (a (* a a)))
	  (set-car! x a)
	  a)
	(define a 3)
	(define a (lambda (x) x))
	(define (fact n) (if (zero? n) 1 (* n (fact (- n 1)))))
	(define (foo a b . c) (list a b c))
	(define (foo . a) (list a))
	(define (foo a b c) (list a b c))
))

(define negativeTests
  (list
    '(if)
    '(cond)
    '(let)
    '(lambda)
    '(letrec)
    '(let*)
    '(lambda (a b c a) (f x))
    '(let ((AbC 5) (Sym123 "abc") (AbC 12)) (if (= AbC 12) #t (begin (display "WOW") #f)))
    '(letrec ((AbC (lambda (x) (AbC x))) (Sym123 "abc") (AbC 12)) (if (= AbC 12) #t (begin (display "WOW") #f)))
))

(define LidanHifiAndKenSaggyTests
  (list
    #f
    #\a
    34
    "abc"
    '(quote a)
    '(quote (a b c))
    '(quote (quote a b c))
    'abc
    '123x
    '(if a b c)
    '(if (if a b c) 'x '(x y z))
    '(if a b)
    '(if a 4)
    '(if #t 'abc)
    '(lambda (x y z) (if x y z))
    '(lambda () a)
    '(lambda () 'ok)
    '(lambda (a b c) b1 b2 b3)
    '(lambda (x y z . rest) (if x y z))
    '(lambda (x . rest) rest)
    '(lambda (a b . c) b1 b2 b3)
    '(lambda args (if x y z))
    '(lambda args args)
    '(lambda v b1 b2 b3)
    '(lambda (x . rest) rest)
    '(define x 5)
    '(define x (lambda (x) x))
    '(define (id x) x)
    '(define (foo x y z) (if x y z))
    '(define (foo x y . z) (if x y z))
    '(define (list . args) args)
    '(a)
    '(a b c)
    '((a b) (a c))
    '(begin)
    '(begin e1 e2 e3 e4)
    '(begin (quote a) abc (quote b))
    '(lambda (x) (quote a) x (quote b))
    '(lambda a a a)
    '(lambda (a . b) a b)
    '(lambda (a b) a b)
    '(let ((v1 e1) (v2 e2)) b1 b2 b3)
    '(let ((v1 e1)) (* v1 2))
    '(let ((x 3) (y 4) (z 5)) (+ x y) #t #f)
    '(let ((a 1)) 5 a)
    '(let () 5)
    '(let ((a 1) (b 2)) (+ a b))
    '(letrec ((f1 (lambda (x) (+ 1 x))) (f2 (lambda (y) (* 1 y)))) 1)
    '(let* ((v1 e1) (v2 (+ v1 1))) b1 b2 b3)
    '(let* ((v1 e1) (v2 e2)) b1 b2 b3)
    '(let* ((a 1) (a 2)) a)
    '(let* ((a 1)) a b)
    '(let* ((a 1) (b 2)) a)
    '(let* () a b)
    '(and)
    '(and a)
    '(and a b)
    '(and a b c)
    '(and e1 e2 e3 e4)
    `(and a b c d e f)
    '(and 1 (if #f #f) 4)
    '(cond (else x))
    '(cond (else 3))
    '(cond (1 2 3) (else 4))
    '(cond (#f 2))
    '(cond (#f 2) (else 3))
    '(cond (x y))
    '(cond (x y) (else z))
    '(cond (a b) (c d))
    '(cond (a b) (c d) (else e))
    '(cond ((p1? x) e1 e2 e3) ((p2? y) e4 e5) ((p3? z) e6 e7 e8 e9) (else e10))
    '(cond ((zero? n) 1))
    '(cond ((zero? n) 1) ((positive? n) 2))
    '(cond ((zero? n) 1) (else 5))
    '(cond ((zero? n) 1) ((positive? n) 2) (else 5))
    '(cond (1 2 3) (else 4))
    '(define (fact n) (if (zero? n) 1 (* n (fact (- n 1)))))
    '(quasiquote (a b c))
    '(quasiquote (a ,b c))
    '(quasiquote (a ,b c))
    '(quasiquote (a ,b ,@c))
    '(quasiquote (,@a ,b c))
    
    
))



(runAllTests
  (list
      (cons "Constants" constTests)
      (cons "Variables" varTests)
      (cons "Conditionals" conditionalsTests)
     ; (cons "Disjunctions" disjTests)
     ; (cons "Lambda" lambdaTests)
     ; (cons "Define" defTests)
      (cons "Assignments" assTests)
     ; (cons "Applications" appTests)
     ; (cons "Sequences" seqTests)
      ;(cons "let" letTests)
      ;(cons "let*" letStarTests)
      ;(cons "letrec" letrecTests)
      ;(cons "and" andTests)        
     ; (cons "cond" condTests) 
      ;(cons "Mit define" defMITExpTests)              
      ;(cons "quasiquoted" quasiTests)    



	;  (cons "Parser" parserTests)
     ; (cons "Negative" negativeTests)
     ; (cons "Comp151 - Lidan Hifi and Ken Saggy tests" LidanHifiAndKenSaggyTests)              
))


