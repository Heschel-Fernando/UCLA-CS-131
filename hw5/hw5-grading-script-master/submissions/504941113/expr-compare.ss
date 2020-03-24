#lang racket
(provide (all-defined-out))

; check if symbol x is lambda or λ
(define (lambda? x)
    (if (member x '(lambda λ)) #t #f)
)

; combine two symbols with !
(define (combine arg1 arg2)
	(string->symbol (string-append (symbol->string arg1) "!" (symbol->string arg2)))
)

; combine two lambda arguments
; x-lambda-args and y-lambda-args must be the same length
; otherwise this function does undefined behavior
(define (combine-lambda-args x-lambda-args y-lambda-args)
	(cond
		[(null? x-lambda-args) x-lambda-args]
		[(equal? (car x-lambda-args) (car y-lambda-args))
			(cons (car x-lambda-args) 
				  (combine-lambda-args (cdr x-lambda-args) (cdr y-lambda-args))
			)
		]
		[else
			(cons (combine (car x-lambda-args) (car y-lambda-args))
				  (combine-lambda-args (cdr x-lambda-args) (cdr y-lambda-args))
			)
		]
	)
)

; replace old-args in body with new-args and return new-body
; only bind surface level (does not go inside nested list)
; note body is either a list or a symbol
(define (bind-args-to-body body old-args new-args)
	; (fprintf (current-output-port) "body: ~a \n" body)
	; (fprintf (current-output-port) "old-args: ~a \n" old-args)
	; (fprintf (current-output-port) "new-args: ~a \n" new-args)
	(cond
		; body is a symbol
		[(not (list? body))
			(let ([index (index-of old-args body)])
				(cond
					; argument not found
					[(equal? index #f) body]
					; argument found
					[else
						(list-ref new-args index)
					]
				)
			)
		]
		; body is a list of symbols
		[else
			(cond
				; empty list
				[(null? body) '()]
				[else
					; (car body) could be a list 
					; in that case, argument is always not found
					(let ([index (index-of old-args (car body))])
						; (fprintf (current-output-port) "body: ~a \n" body)
						(cond
							; body is if statement
							; don't override if macro with args
							[(equal? (car body) 'if)
								(cons (car body) 
									  (bind-args-to-body (cdr body) old-args new-args)
								)
							]
							; argument not found
							[(equal? index #f)
								(cons (car body) 
									  (bind-args-to-body (cdr body) old-args new-args)
								)
							]
							; argument found
							[else
								(cons (list-ref new-args index) 
									  (bind-args-to-body (cdr body) old-args new-args)
								)
							]
						)
					)
				]
			)
		]
	)
)

; wrapper of bind-args-to-body
; perform binding when we encounter first element of the list
; only bind surface level
; return new-body iff fst flag is true or body is lambda function
; otherwise return original body
(define (expr-compare-bind body old-args new-args fst)
	(cond
		[fst
			(cond
				[(and (list? body) (lambda? (car body)))
					; body is lambda function
					; don't bind now, we will bind later
					body
				]
				[else
					; bind args to body
					(bind-args-to-body body old-args new-args) 
				]
			)
			
		]
		[else
			; already binded
			body
		]
	)
	
)

; bind args to body recursively
; return new-body where arguments are binded
; use this when you know the variable names won't change due to combined arguments
; fst is true if we encounter the first element of a list
; fst is useful to know whether lambda is macro or just a variable name
; (expr-compare '(lambda (x) (f (lambda (x) x))) '(lambda (y) (lambda (a b) (+ a b))))
; => '(lambda (x!y) (if % (f (lambda (x) x)) (lambda (a b) (+ a b))))
(define (rec-bind-args-to-body body old-args new-args fst)
	; (fprintf (current-output-port) "body: ~a \n" body)
	(cond
		; body is a symbol
		[(not (list? body))
			(bind-args-to-body body old-args new-args)
		]
		; body is a list of symbols
		[else
			(cond
				; empty list
				[(null? body) '()]
				; head body is a list
				[(list? (car body))
					; recursively bind arguments
					(cons 
						(rec-bind-args-to-body (car body) old-args new-args #t)
						(rec-bind-args-to-body (cdr body) old-args new-args #f)
					)
				]
				; head body is not a list
				[else
					(cond
						; head body is first element of the list and lambda 
						[(and fst (lambda? (car body)))
							(let ([lambda-args (car (cdr body))]
								  [lambda-body (car (cdr (cdr body)))])
								(list (car body) 
			        				  lambda-args 
			        				  (rec-bind-args-to-body 
			        				  	lambda-body 
			        				  	(append lambda-args old-args) ; append stack s.t. latest variables are accessed first
			        				  	(append lambda-args new-args) ; append stack s.t. latest variables are accessed first
			        				  	#t
			        				  ) 
			        			)
							)
						]
						; head body is first element of the list and quote
						[(and fst (equal? (car body) 'quote))
							; do not bind args after quote is encountered
							body
						]
						; head body is not first element of the list or not lambda or not quote
						[else
							(let ([index (index-of old-args (car body))])
								; (fprintf (current-output-port) "body: ~a \n" body)
								(cond
									; argument not found
									[(equal? index #f)
										(cons (car body) 
											  (rec-bind-args-to-body (cdr body) old-args new-args #f)
										)
									]
									; argument found
									[else
										(cons (list-ref new-args index) 
											  (rec-bind-args-to-body (cdr body) old-args new-args #f)
										)
									]
								)
							)
						]
					)
					
				]
			)
		]
	)
)

; cs is current combined variable stack
; xos is original x variable stack
; yos is original y variable stack
; cs, xos, and yos have the same size
; old variable names are stored in xos and yos.
; new variable names are stored in cs.
; fst is true if we are currently examining beggining of the list
; otherwise fst is false
(define (expr-compare-helper x y xos yos cs fst)
	; (fprintf (current-output-port) "x: ~a \n" x)
	; (fprintf (current-output-port) "y: ~a \n" y)
	; bind args to body
	(let ([binded-x (expr-compare-bind x xos cs fst)]
		  [binded-y (expr-compare-bind y yos cs fst)])
		; (fprintf (current-output-port) "binded-x: ~a \n" binded-x)
		; (fprintf (current-output-port) "binded-y: ~a \n" binded-y)
		; (fprintf (current-output-port) "fst: ~a \n" fst)
	    (cond
	    	; x and y refers to the same object
	    	; (expr-compare 12 12) => 12
	    	; (expr-compare #t #t) => #t
	    	; (expr-compare #f #f) => #f
	    	; (expr-compare '(cons a b) '(cons a b)) => (cons a b)
	    	; (expr-compare '''''''(a b) '''''''(a b)) => '''''''(a b)
	    	[(equal? binded-x binded-y) 
	    		; (fprintf (current-output-port) "binded-x: ~a \n" binded-x)
	    		(if (list? binded-x)
	    			(rec-bind-args-to-body binded-x xos cs #t) ; fst is true becasue x is list
	    			binded-x
	    		)
	    	]
	    	; x and y are booleans
	    	; (expr-compare #t #f) => %
	    	; (expr-compare #f #t) => (not %)
	        [(and (boolean? binded-x) (boolean? binded-y)) 
	        	(if binded-x
	            	(if binded-y #t '%)
	            	(if binded-y '(not %) #f)
	            )
	        ]
	        ; if one of them is not list - which means that not function
	        ; (expr-compare 'a '(cons a b)) => (if % a (cons a b))
	        [(or (not (list? binded-x)) (not (list? binded-y)))
	        	(cond
	        		; x is list
	        		[(list? binded-x)
	        			(list 
			            	'if 
			            	'% 
			            	(rec-bind-args-to-body binded-x xos cs #t) ; fst is true becasue x is list
			            	binded-y
			            )
	        		]
	        		; y is list
	        		[else
	        			(list 
			            	'if 
			            	'% 
			            	binded-x 
			            	(rec-bind-args-to-body binded-y yos cs #t) ; fst is true because y is list
			            )
	        		]
	        	)
	        ]
	        ; everything below are when both x and y are lists
	        ; x and y are lists of different length
	        ; (expr-compare '(list) '(list a)) => (if % (list) (list a))
	        [(not (= (length binded-x) (length binded-y)))
	        	(list 
    				'if 
    				'% 
    				(rec-bind-args-to-body binded-x xos cs #t) ; fst is true becasue x is list
    				(rec-bind-args-to-body binded-y yos cs #t) ; fst is true because y is list
    			)
	        ]
	        ; x and y are lists of same length
	        [else
	        	(cond
	        		; head x and head y are variables (even if they are named lambda or if)
	        		; everything after this case is when fst is true (i.e. lambda or if are macros/keywords)
	        		[(not fst)
	        			(cons (expr-compare-helper (car binded-x) (car binded-y) xos yos cs #t) 
			        		  (expr-compare-helper (cdr binded-x) (cdr binded-y) xos yos cs #f))
	        		]
	        		; head x and head y are 'quote
			        ; (expr-compare ''(a b) ''(a c)) => (if % '(a b) '(a c))
			        ; (expr-compare '(quote (a b)) '(quote (a c))) => (if % '(a b) '(a c))
			        ; (expr-compare '''''''(a b) '''''''(a c)) => (if % ''''''(a b) ''''''(a c))
			        ; (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
			        ; => (if % '((λ (a) a) c) '((lambda (b) b) d))
			        [(and (equal? (car binded-x) 'quote) (equal? (car binded-y) 'quote))
			        	(cond
			        		[(equal? binded-x binded-y) binded-x] ; do not bind args inside quote
			        		[(and (boolean? binded-x) (boolean? binded-y))
					        	(if binded-x
					            	(if binded-y #t '%)
					            	(if binded-y '(not %) #f)
					            )
					        ]
					        [else
					        	(list 'if '% binded-x binded-y) ; do not bind args inside quote
					        ]
			        	)
			        ]
			        ; either head x or head y (exclusive) is 'if
			        ; (expr-compare '(if x y z) '(g x y z)) => (if % (if x y z) (g x y z))
			        ; (expr-compare '(if x y z) '(g x z z)) => (if % (if x y z) (g x z z))
			        [(xor (equal? (car binded-x) 'if) (equal? (car y) 'if))
			        	(list 
			        		'if 
			        		'% 
			        		(rec-bind-args-to-body binded-x xos cs #t) ; fst is true
				        	(rec-bind-args-to-body binded-y yos cs #t) ; fst is true 
			        	)
			        ]
			        ; either head x or head y (exclusive) is 'lambda
			        ; (expr-compare '(lambda (a) b) '(cons (c) b))
			        ; => '(if % (lambda (a) b) (cons (c) b))
			        [(xor (lambda? (car binded-x)) (lambda? (car binded-y)))
			        	(list 
			        		'if 
			        		'% 
			        		(rec-bind-args-to-body binded-x xos cs #t) ; fst is true
				        	(rec-bind-args-to-body binded-y yos cs #t) ; fst is true
			        	)
			        ]
			        ; both head x and head y are lambda
			        ; (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
			        ; => ((lambda (a) ((if % f g) a)) (if % 1 2))
			        ; (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
			        ; => ((λ (a) ((if % f g) a)) (if % 1 2))
			        ; (expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
			        ; => ((lambda (a!b) a!b) (if % c d))
			        ; (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
	                ;               '(+ #t ((lambda (a c) (f a c)) 1 2)))
	                ; => (+ (not %) ((λ (a b!c) (f a b!c)) 1 2))
	                ; (expr-compare '((λ (a b) (f a b)) 1 2)
	                ;               '((λ (a b) (f b a)) 1 2))
	                ; => ((λ (a b) (f (if % a b) (if % b a))) 1 2)
	                ; (expr-compare '((λ (a b) (f a b)) 1 2)
	                ;               '((λ (a c) (f c a)) 1 2))
	                ; => ((λ (a b!c) (f (if % a b!c) (if % b!c a))) 1 2)
			        [(and (lambda? (car binded-x)) (lambda? (car binded-y)))
			        	; (fprintf (current-output-port) "lambda!\n")
			        	(let ([x-lambda-args (car (cdr binded-x))] 
				        	  [y-lambda-args (car (cdr binded-y))])
			        		; (fprintf (current-output-port) "x-lambda-args: ~a \n" x-lambda-args)
			        		; (fprintf (current-output-port) "y-lambda-args: ~a \n" y-lambda-args)
			        		(cond
				        		; number of arguments of lambda functions are the same
				        		[(equal? (length x-lambda-args) (length y-lambda-args))
				        			(let ([combined-lambda-args (combine-lambda-args x-lambda-args y-lambda-args)])
				        				; (fprintf (current-output-port) "combined-lambda-args: ~a \n" combined-lambda-args)
				        				; for this assignment, we can assume lambda body has only one statement
				        				(let ([x-lambda-body (car (cdr (cdr binded-x)))] 
				        					  [y-lambda-body (car (cdr (cdr binded-y)))])
				        					; (fprintf (current-output-port) "x-lambda-body: ~a \n" x-lambda-body)
				        					; (fprintf (current-output-port) "y-lambda-body: ~a \n" y-lambda-body)
				        					(cond
						        				; lambda symbols are the same
						        				[(equal? (car binded-x) (car binded-y))
								        			(list (car binded-x) 
								        				  combined-lambda-args 
								        				  (expr-compare-helper 
								        				  	x-lambda-body 
								        				  	y-lambda-body 
								        				  	(append x-lambda-args xos) ; append stack s.t. latest variables are accessed first
								        				  	(append y-lambda-args yos) ; append stack s.t. latest variables are accessed first
								        				  	(append combined-lambda-args cs) ; append stack s.t. latest variables are accessed first
								        				  	#t) 
								        			)
								        		]
								        		; lambda symbols are different (one is 'lambda and the other is 'λ)
								        		[else
								        			(list 'λ 
								        				  combined-lambda-args 
								        				  (expr-compare-helper 
								        				  	x-lambda-body 
								        				  	y-lambda-body 
								        				  	(append x-lambda-args xos) ; append stack s.t. latest variables are accessed first
								        				  	(append y-lambda-args yos) ; append stack s.t. latest variables are accessed first
								        				  	(append combined-lambda-args cs) ; append stack s.t. latest variables are accessed first
								        				  	#t) 
								        			)
								        		]
						        			)
				        				)
				        				
				        			)
				        			
				        		]
				        		; number of arguments of lambda functions are different
				        		; (expr-compare '(lambda (a b) a) '(λ (b) b)) 
				        		; => (if % (lambda (a b) a) (λ (b) b))
				        		[else
				        			(list 
				        				'if 
				        				'% 
				        				(rec-bind-args-to-body binded-x xos cs #t) ; fst is true
				        				(rec-bind-args-to-body binded-y yos cs #t) ; fst is true
				        			)
				        		]
				        		
				        	)
				        )
			        	
			        ]
			        ; x and y are lists of same length, but does not match criteria above
			        ; (expr-compare 
			        ;     '(cons (cons a b) (cons b c))
			        ;     '(cons (cons a c) (cons a c))
			        ; )
			        ; => (cons (cons a (if % b c)) (cons (if % b a) c))
			        ; (expr-compare '(cons a b) '(list a b)) => ((if % cons list) a b)
			        ; (expr-compare '(quoth (a b)) '(quoth (a c))) => (quoth (a (if % b c)))
			        ; (expr-compare '(if x y z) '(if x z z)) => (if x (if % y z) z)
			        ; (expr-compare '(cons a lambda) '(cons a λ)) => (cons a (if % lambda λ))
			        ; (expr-compare '(f x y z) '(g x z z)) => ((if % f g) x (if % y z) z)
			        ; (expr-compare '(f x y z) '(g x y z)) => ((if % f g) x y z)
			        [else
			        	(cons (expr-compare-helper (car binded-x) (car binded-y) xos yos cs #t) 
			        		  (expr-compare-helper (cdr binded-x) (cdr binded-y) xos yos cs #f))
			        ]
	        	)
	        ]
	        
	    )
	)
)

(define (expr-compare x y)
	(expr-compare-helper x y '() '() '() #t)
)

(define (test-expr-compare x y)
    (and
        (equal? (eval x)
                (eval `(let ((% #t)) ,(expr-compare x y))))
        (equal? (eval y)
                (eval `(let ((% #f)) ,(expr-compare x y))))
    )
)

(define test-expr-x
	'(list 
		1
		"a"
		#t
		#t
		1
		((lambda (a) a) "a")
		((λ (if) (+ if 1)) 3)
		((lambda (lambda) lambda) 1)
		(let ([a 1]) ''a)
		((lambda (a b) a) 1 2)
		((λ (x) ((λ (x) x) x)) 1) 
		(cons 1 2)
		(list)
		'(a b)
		(quote (a b))
		(if 1 2 3) 
		(if 1 2 3)
		((lambda (lambda) (+ lambda 2 (+ lambda 1))) 3) 
		'''''(a b) 
		'''''(a b)
		((lambda (x) (+ ((lambda (x) x) 1) 2)) 3)
		(list 1 2 3)
		((lambda (a) (quote (1 a))) 1) 
	)
)

(define test-expr-y
	'(list 
		2
		"b"
		#t
		#f
		(cons 1 2)
		((lambda (b) b) "b")
		((lambda (fi) (+ fi 1)) 3)
		((λ (λ) λ) 1)
		(let ([a 1]) '(quote a))
		((λ (b) b) 1)
		((λ (y) ((λ (x) y) 1)) 2)
		(list 1 2)
		(list 1)
		'(a c)
		(quote (a c))
		(if 1 2 4)
		(+ 1 2 3)
		((lambda (if) (+ if if (+ if 1))) 3)
		'''''(a c)
		'''''(a b) 
		((lambda (y) ((lambda (a b) (+ a b)) 1 2)) 3)
		(list 1 2)
		((lambda (b) (quote (1 b))) 2)
	)
)

; exports
(provide expr-compare)
(provide test-expr-compare)
(provide test-expr-x)
(provide test-expr-y)


; test 0
(fprintf (current-output-port) "test0: ~a \n" 
	(equal? 
		(expr-compare '(cons a lambda) '(cons a λ)) 
		'(cons a (if % lambda λ))
	)
)

; test 1
(fprintf (current-output-port) "test1: ~a \n" 
	(equal? 
		(expr-compare '(lambda (a) a) '(lambda (b) b)) 
		'(lambda (a!b) a!b)
	)
)

; test 2
(fprintf (current-output-port) "test2: ~a \n" 
	(equal? 
		(expr-compare '(lambda (a) b) '(cons (c) b)) 
		'(if % (lambda (a) b) (cons (c) b))
	)
)

; test 3
(fprintf (current-output-port) "test3: ~a \n" 
	(equal? 
		(expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3))
		'((λ (if!fi) (+ if!fi 1)) 3)
	)
)

; test 4
(fprintf (current-output-port) "test4: ~a \n" 
	(equal? 
		(expr-compare '(lambda (lambda) lambda) '(λ (λ) λ))
		'(λ (lambda!λ) lambda!λ)
	)
)

; test 5
(fprintf (current-output-port) "test5: ~a \n" 
	(equal? 
		(expr-compare ''lambda '(quote λ))
		'(if % 'lambda 'λ)
	)
)

; test 6
(fprintf (current-output-port) "test6: ~a \n" 
	(equal? 
		(expr-compare '(lambda (a b) a) '(λ (b) b))
		'(if % (lambda (a b) a) (λ (b) b))
	)
)

; test 7
(fprintf (current-output-port) "test7: ~a \n" 
	(equal? 
		(expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b)))
		'(if % (λ (a b) (lambda (b) b)) (lambda (b) (λ (b) b)))
	)
)

; test 8
(fprintf (current-output-port) "test8: ~a \n" 
	(equal? 
		(expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y)))
		'(λ (let) (let (((if % x y) 1)) (if % x y)))
	)
)

; test 9
(fprintf (current-output-port) "test9: ~a \n" 
	(equal? 
		(expr-compare '(λ (x) ((λ (x) x) x)) '(λ (y) ((λ (x) y) x)))
		'(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x)))
	)
)

; test 10
(fprintf (current-output-port) "test10: ~a \n" 
	(equal? 
		(expr-compare '(((λ (g)
                   ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
                    (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
                 (λ (r)                               ; Here (r) will be the function itself
                   (λ (n) (if (= n 0)
                              1
                              (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
                10)
              '(((λ (x)
                   ((λ (n) (x (λ () (n n))))
                    (λ (r) (x (λ () (r r))))))
                 (λ (g)
                   (λ (x) (if (= x 0)
                              1
                              (* x ((g) (- x 1)))))))
                9))

		'(((λ (g!x)
                    ((λ (x!n) (g!x (λ () (x!n x!n))))
                     (λ (x!r) (g!x (λ () (x!r x!r))))))
                  (λ (r!g)
                    (λ (n!x) (if (= n!x 0)
                                 1
                                 (* n!x ((r!g) (- n!x 1)))))))
                 (if % 10 9))
	)
)

; test 11
(fprintf (current-output-port) "test11: ~a \n" 
	(equal? 
		(expr-compare 12 12)
		'12
	)
)

; test 12
(fprintf (current-output-port) "test12: ~a \n" 
	(equal? 
		(expr-compare 12 20)
		'(if % 12 20)
	)
)

; test 13
(fprintf (current-output-port) "test13: ~a \n" 
	(equal? 
		(expr-compare #t #t)
		'#t
	)
)

; test 14
(fprintf (current-output-port) "test14: ~a \n" 
	(equal? 
		(expr-compare #f #f) 
		'#f
	)
)

; test 15
(fprintf (current-output-port) "test15: ~a \n" 
	(equal? 
		(expr-compare #t #f)
		'%
	)
)

; test 16
(fprintf (current-output-port) "test16: ~a \n" 
	(equal? 
		(expr-compare #f #t)
		'(not %)
	)
)

; test 17
(fprintf (current-output-port) "test17: ~a \n" 
	(equal? 
		(expr-compare 'a '(cons a b))
		'(if % a (cons a b))
	)
)

; test 18
(fprintf (current-output-port) "test18: ~a \n" 
	(equal? 
		(expr-compare '(cons a b) '(cons a b))
		'(cons a b)
	)
)

; test 19
(fprintf (current-output-port) "test19: ~a \n" 
	(equal? 
		(expr-compare '(cons a lambda) '(cons a λ))
		'(cons a (if % lambda λ))
	)
)

; test 20
(fprintf (current-output-port) "test20: ~a \n" 
	(equal? 
		(expr-compare '(cons (cons a b) (cons b c)) '(cons (cons a c) (cons a c)))
		'(cons (cons a (if % b c)) (cons (if % b a) c))
	)
)

; test 21
(fprintf (current-output-port) "test21: ~a \n" 
	(equal? 
		(expr-compare '(cons a b) '(list a b))
		'((if % cons list) a b)
	)
)

; test 22
(fprintf (current-output-port) "test22: ~a \n" 
	(equal? 
		(expr-compare '(list) '(list a)) 
		'(if % (list) (list a))
	)
)

; test 23
(fprintf (current-output-port) "test23: ~a \n" 
	(equal? 
		(expr-compare ''(a b) ''(a c))
		'(if % '(a b) '(a c))
	)
)

; test 24
(fprintf (current-output-port) "test24: ~a \n" 
	(equal? 
		(expr-compare '(quote (a b)) '(quote (a c)))
		'(if % '(a b) '(a c))
	)
)

; test 25
(fprintf (current-output-port) "test25: ~a \n" 
	(equal? 
		(expr-compare '(quoth (a b)) '(quoth (a c)))
		'(quoth (a (if % b c)))
	)
)

; test 26
(fprintf (current-output-port) "test26: ~a \n" 
	(equal? 
		(expr-compare '(if x y z) '(if x z z))
		'(if x (if % y z) z)
	)
)

; test 27
(fprintf (current-output-port) "test27: ~a \n" 
	(equal? 
		(expr-compare '(if x y z) '(g x y z))
		'(if % (if x y z) (g x y z))
	)
)


; test 28
(fprintf (current-output-port) "test28: ~a \n" 
	(equal? 
		(expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
		'((lambda (a) ((if % f g) a)) (if % 1 2))
	)
)


; test 29
(fprintf (current-output-port) "test29: ~a \n" 
	(equal? 
		(expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
		'((λ (a) ((if % f g) a)) (if % 1 2))
	)
)

; test 30
(fprintf (current-output-port) "test30: ~a \n" 
	(equal? 
		(expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
		'((lambda (a!b) a!b) (if % c d))
	)
)


; test 31
(fprintf (current-output-port) "test31: ~a \n" 
	(equal? 
		(expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
		'(if % '((λ (a) a) c) '((lambda (b) b) d))
	)
)

; test 32
(fprintf (current-output-port) "test32: ~a \n" 
	(equal? 
		(expr-compare '(+ #f ((λ (a b) (f a b)) 1 2)) '(+ #t ((lambda (a c) (f a c)) 1 2)))
		'(+ (not %) ((λ (a b!c) (f a b!c)) 1 2))
	)
)

; test 33
(fprintf (current-output-port) "test33: ~a \n" 
	(equal? 
		(expr-compare '((λ (a b) (f a b)) 1 2) '((λ (a b) (f b a)) 1 2))
		'((λ (a b) (f (if % a b) (if % b a))) 1 2)
	)
)


; test 34
(fprintf (current-output-port) "test34: ~a \n" 
	(equal? 
		(expr-compare '((λ (a b) (f a b)) 1 2) '((λ (a c) (f c a)) 1 2))
		'((λ (a b!c) (f (if % a b!c) (if % b!c a))) 1 2)
	)
)

; test 35
(fprintf (current-output-port) "test35: ~a \n" 
	(equal? 
		(expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3) '((lambda (if) (+ if if (f λ))) 3))
		'((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3)
	)
)

; test 36
(fprintf (current-output-port) "test36: ~a \n" 
	(equal? 
		(expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))
		'((λ (a)
	      ((if % eq? eqv?)
	       a
	       ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
	        a (λ (a!b) (if % a!b a)))))
	     (lambda (b!a a!b) (b!a a!b)))
	)
)

; test 37
(fprintf (current-output-port) "test37: ~a \n" 
	(equal? 
		(expr-compare '''''''(a b) '''''''(a c))
		'(if % ''''''(a b) ''''''(a c))
	)
)

; test 38
(fprintf (current-output-port) "test38: ~a \n" 
	(equal? 
		(expr-compare '''''''(a b) '''''''(a b))
		'''''''(a b)
	)
)

; test 39
(fprintf (current-output-port) "test39: ~a \n" 
	(equal? 
		(expr-compare '(if x y z) '(g x z z))
		'(if % (if x y z) (g x z z))
	)
)

; test 40
(fprintf (current-output-port) "test40: ~a \n" 
	(equal? 
		(expr-compare '(f x y z) '(g x z z))
		'((if % f g) x (if % y z) z)
	)
)

; test 41
(fprintf (current-output-port) "test41: ~a \n" 
	(equal? 
		(expr-compare '(f x y z) '(g x y z))
		'((if % f g) x y z)
	)
)

; test 42
(fprintf (current-output-port) "test42: ~a \n" 
	(equal? 
		(expr-compare '(cons a lambda) '(cons a lambda))
		'(cons a lambda)
	)
)

; test 43
(fprintf (current-output-port) "test43: ~a \n" 
	(equal? 
		(expr-compare '(lambda (x) (f (g x))) '(lambda (y) (g a y)))
		'(lambda (x!y) (if % (f (g x!y)) (g a x!y)))
	)
)

; test 44
(fprintf (current-output-port) "test44: ~a \n" 
	(equal? 
		(expr-compare '(lambda (x) (f (lambda (y) (+ x y)))) '(lambda (y) (lambda (x) (+ x y))))
		'(lambda (x!y) (if % (f (lambda (y) (+ x!y y))) (lambda (x) (+ x x!y))))
	)
)

; test 45
(fprintf (current-output-port) "test45: ~a \n" 
	(equal? 
		(expr-compare '(lambda (x) (f (g x))) '(lambda (y) (g a b)))
		'(lambda (x!y) (if % (f (g x!y)) (g a b)))
	)
)

; test 46
(fprintf (current-output-port) "test46: ~a \n" 
	(equal? 
		(expr-compare '(lambda (x) (lambda (x) (+ x x)) x) '(lambda (y) (lambda (x) (+ x y)) x))
		'(lambda (x!y) (lambda (x) (+ x (if % x x!y))))
	)
)

; test 47
(fprintf (current-output-port) "test47: ~a \n" 
	(equal? 
		(expr-compare '(lambda (x) (f (lambda (x) x))) '(lambda (y) (g (lambda (x) y))))
		'(lambda (x!y) ((if % f g) (lambda (x) (if % x x!y))))
	)
)

; test 48
(fprintf (current-output-port) "test48: ~a \n" 
	(equal? 
		(expr-compare '(lambda (x) (f (lambda (x) x))) '(lambda (y) (lambda (a b) (+ a b))))
		'(lambda (x!y) (if % (f (lambda (x) x)) (lambda (a b) (+ a b))))
	)
)

; test 49
(fprintf (current-output-port) "test49: ~a \n" 
	(equal? 
		(expr-compare '((lambda (a) (lambda (b) (+ a b))) f) '((lambda (b) (cons b f)) g))
		'((lambda (a!b) (if % (lambda (b) (+ a!b b)) (cons a!b f))) (if % f g))
	)
)

; test 50
(fprintf (current-output-port) "test50: ~a \n" 
	(equal? 
		(expr-compare '(lambda (a) (if c (k (a)) g)) '(lambda (b) (h c (k (b)) b)))
		'(lambda (a!b) (if % (if c (k (a!b)) g) (h c (k (a!b)) a!b)))
	)
)

; test 51
(fprintf (current-output-port) "test51: ~a \n" 
	(equal? 
		(expr-compare '(a b c) '(a b)) 
		'(if % (a b c) (a b))
	)
)

; test 52
(fprintf (current-output-port) "test52: ~a \n" 
	(equal? 
		(expr-compare '(a b) '(a c))
		'(a (if % b c))
	)
)

; test 53
(fprintf (current-output-port) "test53: ~a \n" 
	(equal? 
		(expr-compare '(a b) '(a c))
		'(a (if % b c))
	)
)

; test 54
; note ((lambda (a) (quote a)) 3) => 'a
(fprintf (current-output-port) "test54: ~a \n" 
	(equal? 
		(expr-compare '(lambda (a) (quote (quote (a b)))) '(lambda (b) (quote (quote (a b)))))
		'(lambda (a!b) ''(a b))
	)
)

; test 55
(fprintf (current-output-port) "test55: ~a \n" 
	(equal? 
		(expr-compare '(lambda (a) (quote (a b))) '(lambda (b) (quote (a b))))
		'(lambda (a!b) '(a b))
	)
)

; test 56
(fprintf (current-output-port) "test56: ~a \n" 
	(equal? 
		(expr-compare '(lambda (list) (f (list 1 2) list)) '(lambda (h) (h (list 1 2) list)))
		'(lambda (list!h) ((if % f list!h) ((if % list!h list) 1 2) (if % list!h list)))
	)
)

; test 57
(fprintf (current-output-port) "test57: ~a \n" 
	(equal? 
		(expr-compare '(lambda (a) (quote (a b))) '(lambda (b) (f a b)))
		'(lambda (a!b) (if % '(a b) (f a a!b)))
	)
)

; This case won't appear in test case
; (fprintf (current-output-port) "test: ~a \n" 
; 	(equal? 
; 		(expr-compare '(lambda (quote) (quote (a b))) '(lambda (a) (quote (a b))))
; 		...
; 	)
; )

; This case won't appear in test case
; (fprintf (current-output-port) "test: ~a \n" 
; 	(equal? 
; 		(expr-compare '(lambda (if) (if a b c)) '(lambda (f) (if a b c)))
; 		...
; 	)
; )

; This case won't appear in test case
; (fprintf (current-output-port) "test: ~a \n" 
; 	(equal? 
; 		(expr-compare '(lambda a a) '(lambda b b))
; 		...
; 	)
; )

; This case won't appear in test case
; (fprintf (current-output-port) "test: ~a \n" 
; 	(equal? 
; 		(expr-compare '(lambda (if) (+ (if a b c) d)) '(lambda (f) (+ (if a b c) d)))
; 		...
; 	)
; )

; This case won't appear in test case
; (fprintf (current-output-port) "test: ~a \n" 
; 	(equal? 
; 		(expr-compare '(lambda (if) (+ (if a b) d)) '(lambda (f) (+ (if a b) d)))
; 		...
; 	)
; )

; This case won't appear in test case
; (fprintf (current-output-port) "test: ~a \n" 
; 	(equal? 
; 		(expr-compare '(lambda (a) (lambda (a!b) (+ a b))) '(lambda (b) (lambda (c) (+ c b))))
; 		'(lambda (a!b) (lambda (a!b!c) (+ a!b (if % b a!b))))
; 	)
; )
