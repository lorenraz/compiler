(load "project/qq.scm")

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define isInList?
	(lambda (a lst)
		(cond
			((null? lst) (= 1 2))
			((equal? (car lst) a) (= 1 1))
			(else (isInList? a (cdr lst)))
			)))
(define isReserveWord?
	(lambda (a)
		(isInList? a *reserved-words*)))

(define improper-list-first-part
	(lambda (imp-lst)
		(letrec ([rec 
			(lambda (lst s)
				(if 
					(pair? lst)
					(rec (cdr lst) (cons (car lst) s))
					s
					))])
		(reverse! (rec imp-lst '())))))

(define improper-list-last-part
	(lambda (imp-lst)
		(cdr (last-pair imp-lst))))

(define improper-list?
	(lambda (a)
		(cond
			((and (pair? a) (not (list? a))) #t)
			(else #f)
			)))

(define let-to-levels ;toDO gets the list of the let definitions, exprs are the expresions. Returns a line without let* that is similar
	(lambda (toDo exprs)
		(if (null? toDo) 
			'()
			(if (null? (cdr toDo))
				`(let (,(car toDo)) ,@exprs)
				`(let (,(car toDo)) ,(let-to-levels (cdr toDo) exprs) )))))

(define let-star-to-let
	(lambda (in)
		(let-to-levels (cadr in) (cddr in))
		))
(define let-to-lambda
	(lambda (in)
		`((lambda ,(map (lambda (x) (car x)) (cadr in)) ,@(cddr in))  ,@(map (lambda (x) (cadr x)) (cadr in)))))

(define and-to-or
	(lambda (in)
		`(not ,@(map (lambda (x) `(not ,x)) (cdr in)))
		))
		
		
(define letParam
	(lambda(lst)
		(if(= (length lst) 1)
			(list (caar lst))
			(append (list (caar lst)) (letParam (cdr lst)))
		)
	)
)
(define letVal
	(lambda(lst)
		(if(= (length lst) 1)
			(list (cadar lst))
			(append (list (cadar lst)) (letVal (cdr lst)))
		)
	)
)
(define beginFunc
	(lambda(exp)
	(cond ((null? exp) *void-object*)
		  ((null? (cdr exp)) (car exp))
		  (else `(begin ,@exp))
		  )))

(define letrec-to-let
	; (lambda (in)
		; (let ((func (lambda (x) `(,x 'dontCare))))
		; (append (append `(let) 
			; (cons (map func (map car (cadr in)))
		; (map (lambda (x) (cons 'set! x)) (cadr in))))
		; (cddr in) ))))
		

	(lambda (sexp)(let ((params (if(null? (car sexp)) '()  (letParam (car sexp))))
			(vals (if(null? (car sexp)) '()  (letVal (car sexp)))))

		 `,(parse `((lambda ,params ,(beginFunc (append (map (lambda(var val)`(set!  ,var ,val) ) params vals)  `((let () ,@(cdr sexp)))))) ,@(map (lambda (x) #f) vals)))

	)
)
)

(define letrec?
	(lambda (sexp)(and (list? sexp) (equal? (car sexp) 'letrec)  (list? (cadr sexp)) (> (length  sexp) 2)))
)
(define begin-start
	(lambda (lst)
		(cond
			((null? lst) void-obj)
			((null? (cdr lst)) (car lst))
			(else `(begin ,@lst))
		)
	)
)


(define cond-to-if
	(lambda (exprs)
		(if (eq? (caar exprs) 'else)  ;;last condition or else clause
			`(begin ,@(cdar exprs))
			(if (null? (cdr exprs))
				`(if ,(caar exprs) (begin ,@(cdar exprs)))
				`(if ,(caar exprs) (begin ,@(cdar exprs)) ,(cond-to-if (cdr exprs))))
		)
))



(define and-to-if
    (lambda(in)
        (if (null? in) #t
            (and-two-pars in))))
            
(define and-two-pars
    (lambda (in)
            (if (null? (cdr in)) (car in)
            `(if ,(car in) ,(and-two-pars (cdr in)) #f))
))



(define de-beginify
	(lambda (in soFar)
			(if (null? in) 

				soFar
				(if (and (list? (car in)) (equal? 'begin (caar in)))
			 		(de-beginify (cdr in) (append soFar (de-beginify (cdar in) '())))
			 		(de-beginify (cdr in) (append soFar (list (car in))))

		))))


(define parse
	(lambda (input)
    	(let ((in input))
		(cond
			;var
			((and (symbol? in) (not (isReserveWord? in))) `(var ,in))
			;const
			((equal? in (void)) `(const ,in))
			((boolean? in) `(const ,in))
			((null? in) `(const ,in))
			((vector? in) `(const ,in))
			((char? in) `(const ,in))
			((number? in) `(const ,in))
			((string? in) `(const ,in))
			((and (list? in) (not (null? in)) (equal? (car in) 'quote)) `(const ,@(cdr in)))
			
			;if3
			((and (list? in) (= (length in) 4) (equal? 'if (car in))) `(if3 ,(parse (cadr in)) ,(parse (caddr in)) ,(parse (cadddr in))))
			;if3 (2 variables)
			((and (list? in) (= (length in) 3) (equal? 'if (car in))) `(if3 ,(parse (cadr in)) ,(parse (caddr in)) ,(parse (void))))			
			;applic
			((and (list? in) (not (null? in)) (not (isReserveWord? (car in)))) `(applic ,(parse (car in)) ,(map parse (cdr in))));TODO:maybe add the @
			;or
			((and (list? in) (= (length in) 1) (equal? 'or (car in))) (parse '#f))
			((and (list? in) (> (length in) 1) (equal? 'or (car in))) `(or ,(map parse (cdr in))))
			;lambda-simple
			((and (list? in) (> (length in) 1) (equal? 'lambda (car in)) (list? (cadr in)) (not (improper-list? (cadr in)))) `(lambda-simple ,(cadr in) ,(parse `(begin ,@(cddr in)))));add the @?
			;lambda-opt (optional arguments)
			((and (list? in) (> (length in) 1) (equal? 'lambda (car in)) (improper-list? (cadr in))) `(lambda-opt ,(improper-list-first-part (cadr in)) ,(improper-list-last-part (cadr in)) ,(parse `(begin ,@(cddr in)))))
			;lambda-opt (variadic lambda)
			((and (list? in) (> (length in) 1) (equal? 'lambda (car in)) (not (list? (cadr in)))) `(lambda-opt () ,(cadr in) ,(parse `(begin ,@(cddr in)))))
		    ;Regular-Define
			((and (list? in) (> (length in) 2) (equal? 'define (car in)) (not (improper-list? (cadr in))) (not (list? (cadr in)))) `(define ,(parse (cadr in)) ,@(map parse (cddr in))))
            ;MIT-style-Define
            ((and (list? in) (> (length in) 2) (equal? 'define (car in)) (or (improper-list? (cadr in)) (list? (cadr in)))) `(define ,(parse (caadr in)) ,(parse `(lambda ,(cdadr in) (begin ,@(cddr in))))))
			;begin
			((and (list? in) (> (length in) 2) (equal? 'begin (car in))) `(seq ,(map parse (de-beginify (cdr in) '()))))
			((and (list? in) (= (length in) 2) (equal? 'begin (car in))) (parse (cadr in))) ;its a list of one item
			((and (list? in) (= (length in) 1) (equal? 'begin (car in))) (parse (if #f #f))) ;its a list of one item


			;set!
			((and (list? in) (= (length in) 3) (equal? 'set! (car in))) `(set ,(parse (cadr in)) ,(parse (caddr in))))
			;seq
			;TODO:find out of this needs to be added in other parts aside the lambdas
			
			;let
			((and (list? in) (> (length in) 1) (equal? 'let (car in)))   (parse (let-to-lambda in)))
			;let*
			((and (list? in) (> (length in) 1) (equal? 'let* (car in)))  (parse (let-star-to-let in)))
			;letrec
			;((and (list? in) (> (length in) 1) (equal? 'letrec (car in)))  (parse (letrec-to-let (cdr in))))
			((letrec? in) (letrec-to-let (cdr in)))
			;and
			((and (list? in) (>= (length in) 1) (equal? 'and (car in))) (parse (and-to-if (cdr in))))
			;cond
			((and (list? in) (> (length in) 1) (equal? 'cond (car in))) (parse (cond-to-if (cdr in))))  ;TODO:the case of 0 arguments
			;quasiquote
			((and (list? in) (^quote? 'quasiquote)) (parse (expand-qq (cadr in))))

			(else 2)
		
		





  ;; fill in the definition of the tag parser here
  ))))






