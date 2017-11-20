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
	(list ''() #f #\a 34 "abc" '(quote a) '(quote (a b c)) '(quote (quote a b c)))
)
(define varTests
	(list 'abc '123x)
)
(define conditionalsTests
	(list 	'(if a b c)
			'(if (if a b c)
				'x
					'(x y z))
			'(if a b)
			'(if a 4)
			'(if #t 'abc)

	 )
)
(define disjTests
	(list 
		'(or (zero? x) (zero? y) (zero? z))
		'(or (or (f1 x) (f2 y)))
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
	)
)
(define seqTests)
(define letTests)
(define letStarTests)
(define letrecTests)
(define andTests)
(define condTests)
(define defMITExpTests)
(define quasiTests)

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
))