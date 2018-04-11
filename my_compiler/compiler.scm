(load "project/tag-parser.scm")
(load "project/sexpr-parser.scm")
(load "project/semantic-analyzer.scm")


(define pipeline
	(lambda (s)
		((star <sexpr>) s
			(lambda (m r)
				(map (lambda (e)
					(annotate-tc
						(pe->lex-pe
							(box-set
								(remove-applic-lambda-nil
									(parse e))))))
					m))
	(lambda (f) 'fail))))


(define file->list
	(lambda (in-file)
		(let ((in-port (open-input-file in-file)))
			(letrec ((run
			(lambda ()
				(let ((ch (read-char in-port)))
					(if (eof-object? ch)
						(begin
							(close-input-port in-port)
								'())
						(cons ch (run)))))))
				(run)))))



(define new-line (list->string  (list #\newline)))
(define label-counter 0)
(define global-counter 0)
(define constant-table (list))
(define global-table (list))
(define const-table-str "")
(define glob-table-str "")
(define symbol-table (list))

(define lookup-in-table
	(lambda (ex table)
		(if (or (null? table) (not (list? table)))
			#f
			(if (equal? (cadr table) ex)
				(car table)
				(lookup-in-table ex (cddr table))
			)
		)
	)
)

(define includeStart
	(lambda ()
		(string-append "%include \"project/scheme.s\"" new-line)
	)
)   


(define endLabel
	(lambda ()
		("L_program_end:" new-line)
	)
)  


(define label-num
    (lambda (l)
        (let ((n 0))
            (lambda ()
                (set! n (+ n 1))
                (string-append l (number->string n))))))



(define label_if3_else (label-num "L_if3_else"))
(define label_if3_exit (label-num "L_if3_exit"))
(define label_closure_body_start (label-num "L_closure_body_start"))
(define label_closure_body_exit (label-num "L_closure_body_exit"))
(define lable-start-or (label-num "L_or_start"))
(define lable-exit-or (label-num "L_or_exit"))
(define lable-start-seq (label-num "L_seq_start"))

(define lable-start-loop (label-num "L_loop_start"))
(define lable-exit-loop (label-num "L_loop_exit"))
(define lable-start-loop-opt (label-num "L_loop_start_opt"))
(define lable-exit-loop-opt (label-num "L_loop_exit_opt"))
(define lable-lambda-start (label-num "L_start_lambda"))
(define lable-lambda-exit (label-num "L_exit_lambda"))
(define lable-lambda-start-opt (label-num "L_start_lambda_opt"))
(define lable-lambda-exit-opt (label-num "L_exit_lambda_opt"))

(define label_proc (label-num "L_proc"))
(define label_error_closure (label-num "L_error_closure"))
(define label_tc_applic_loop_start (label-num "L_tc_applic_loop_start"))
(define label_tc_applic_loop_exit (label-num "L_tc_applic_loop_exit"))
(define lable_wrong_arg_num (label-num "L_wrong_arg_num"))

(define lable-lambda-opt-params-loop (label-num "L_lambda_opt_param_loop"))				
(define lable-lambda-opt-params-exit (label-num "L_lambda_opt_param_exit"))

	
;;;;;;;;;;;;;; CONSTANT TABLE ;;;;;;;;;;;;;;;;;;;;;;

(define init-val
	(lambda (str x)
		(add-to-const-table (string-append "constLabel" (number->string label-counter)) x)
		(set! const-table-str (string-append const-table-str (string-append "constLabel" (number->string label-counter) ":" new-line str new-line)))
	)
)


 (define init-const-table
	(lambda ()
		(init-val "   dq SOB_NIL" '())
		(increase-label-counter)
		(init-val "   dq SOB_VOID" (if #f #f))
		(increase-label-counter)
		(init-val "   dq SOB_FALSE" #f)
		(increase-label-counter)
		(init-val "   dq SOB_TRUE" #t)
	)
)


(define build-const-table
	(lambda (pes)
		(for-each put-const pes)
	)
)

(define put-const
    (lambda (e)
		(if (list? e)
			(cond ((is-const? e) (label-create (cadr e)))
				  ((has-list? e) (for-each put-const e))))))

			  
(define is-const?
  (lambda (pe)
    (and (not (null? pe))
         (equal? (car pe) 'const))))

		 
(define has-list?
  (lambda (e)
    (not (null? (filter list? e)))))	
	
	
(define increase-label-counter
	(lambda ()
		(set! label-counter (+ 1 label-counter))
	)
)

(define lable-create-aux
	(lambda (ex str str-ex)
		(begin 
			(increase-label-counter)
			(add-to-const-table (string-append "constLabel" (number->string label-counter)) ex)
			(set! const-table-str (string-append  const-table-str "constLabel" (number->string label-counter) ":" new-line str str-ex ")" new-line))
		)
	)	
)

(define label-create
	(lambda (ex)
		(if (not (lookup-in-table ex constant-table))
			(cond
				((integer? ex) 
					(lable-create-aux ex "   dq MAKE_LITERAL(T_INTEGER," (number->string ex)))
				((number? ex)
					(begin
						(label-create (numerator ex))
						(label-create (denominator ex))
						(increase-label-counter)
						(add-to-const-table (string-append "constLabel" (number->string label-counter)) ex)
						(set! const-table-str (string-append  const-table-str "constLabel" (number->string label-counter) ":" new-line "  dq MAKE_LITERAL_FRACTION(" (lookup-in-table  (numerator ex) constant-table) " , "  (lookup-in-table  (denominator ex) constant-table) ")" new-line))))
				((char? ex) 
					(lable-create-aux ex "   dq MAKE_LITERAL(T_CHAR, " (number->string (char->integer ex))))
				((symbol? ex)
					(label-create (symbol->string ex))
					(lable-create-aux ex "   dq MAKE_LITERAL_SYMBOL(" (lookup-in-table  (symbol->string ex) constant-table)) 
					(set! symbol-table (cons (string-append "constLabel"(number->string label-counter)) symbol-table)))
				((string? ex)
					(begin
						(increase-label-counter)
						(add-to-const-table (string-append "constLabel" (number->string label-counter)) ex)
						(set! const-table-str (string-append  const-table-str "constLabel" (number->string label-counter) ":" new-line "  MAKE_LITERAL_STRING "  "\""  (const-string-aux (string->list ex))  new-line))))
				((vector? ex)
					(begin   
						(map (lambda (c) (if (not (lookup-in-table c constant-table)) (label-create c))) (vector->list ex)) 
						(increase-label-counter)
						(add-to-const-table (string-append "constLabel" (number->string label-counter)) ex)
						(set! const-table-str (string-append  const-table-str "constLabel" (number->string label-counter) ":" new-line "   dq  MAKE_LITERAL_VECTOR " (lookup-in-table  (car (vector->list ex)) constant-table)  (const-vector-aux (cdr (vector->list ex) ))  new-line))))
				((pair? ex)
					(let* ((ca (car ex))
						   (cd (cdr ex)))
						(begin
							(if (not (lookup-in-table ca constant-table)) (label-create ca))
							(if (not (lookup-in-table cd constant-table)) (label-create cd))
							(increase-label-counter)
							(add-to-const-table (string-append "constLabel" (number->string label-counter)) ex)
							(set! const-table-str (string-append  const-table-str "constLabel" (number->string label-counter) ":" new-line "   dq  MAKE_LITERAL_PAIR(" (lookup-in-table  (car ex) constant-table) " , "  (lookup-in-table   (cdr ex) constant-table) ")" new-line)))))
			)
		)
	)
)
  
  
(define const-vector-aux
	(lambda (ex)
		(if(null? ex)
			""
			(string-append ","  (lookup-in-table  (car ex) constant-table) (const-vector-aux (cdr ex)) )
		)
	)
)


(define const-string-aux
	(lambda (str)
		(if (null? str) 
			"\"" 
			(if (not (list? str )) 
				""	
				(if (< (char->integer (car str)) 11)
					(if (null? (cdr str)) 
						(string-null str)
						(string-not-null str))
					(string-append  (string (car str))  (const-string-aux (cdr str))))))))

(define string-null
	(lambda (str)
		(string-append  "\"" ", "  (number->string (char->integer (car str))))))

		
(define string-not-null
	(lambda (str)
		(string-append  "\"" ", "  (number->string (char->integer (car str)))  ", " "\"" (const-string-aux (cdr str)) )))

		
(define add-to-const-table
  (lambda (lable item)
  		(begin
  			(set! constant-table (append constant-table (list lable item)))
  		)
    )
)
	
;;;;;;;;;;;;;; GLOBAL TABLE ;;;;;;;;;;;;;;;;;;;;;;

(define increase-global-counter
	(lambda ()
		(set! global-counter (+ 1 global-counter))
	)
)

(define build-ge
  (lambda (pes)
    (for-each put-fvar pes)))


(define put-fvar
  (lambda (e)
    (if (list? e)
        (cond ((is-fvar? e) (add-to-glob-table (cadr e)))
              ((has-list? e) (for-each put-fvar e))))))

(define is-fvar?
  (lambda (pe)
    (and (not (null? pe))
         (equal? (car pe) 'fvar))))


(define add-to-glob-table
   (lambda (item)
	    (if (not (lookup-in-table item global-table))
			(begin
			  (set! glob-table-str (string-append glob-table-str "fvar" (number->string global-counter) ":" new-line "   dq SOB_UNDEFINED" new-line))
			  (set! global-table (append global-table (list (string-append "fvar" (number->string global-counter)) item)))
			  (increase-global-counter)
			)
		)
	)     
)


;;;;;;;;;;;;;; SYMBOL TABLE ;;;;;;;;;;;;;;;;;;;;;;

(define build-runtime-symbol-table 
	(lambda (lst)
			(if(null? lst)
				(string-append "" new-line) 
				(string-append 
					(make-symbol-table (car lst)) 
					(build-runtime-symbol-table (cdr lst))))
	)
)


(define make-symbol-table
	(lambda (sym)
		(string-append 
			"mov rcx, rax" new-line 
			"mov r8, rcx" new-line
			"mov rcx, qword [rcx]" new-line 
			"my_malloc 8" new-line
			"mov r12, rax" new-line
			"sub r12, start_of_data" new-line    
			"sar rcx, TYPE_BITS" new-line   
			"add rcx, r12" new-line     
			"sal rcx, TYPE_BITS" new-line 
			"mov qword [r8], rcx" new-line 
			"mov rcx, " sym  new-line    
			"sub rcx, start_of_data" new-line 
			"sal rcx, 34"	new-line 		
			"mov qword [rax], rcx" new-line			
		)
	)
)


;;;;;;;;;;;; RUNTIME SUPPORT ;;;;;;;;;;;;;;;
	
(define runtime-support-list
	(list
		'append 'apply '< '= '> '+ '/ '* '- 'boolean? 'car 'cdr 'char->integer 'char? 'cons 'denominator
		'eq? 'integer? 'integer->char 'list 'make-string 'make-vector 'map 'not
		'null? 'number? 'numerator 'pair? 'procedure? 'rational? 'remainder 'set-car! 'set-cdr!
		'string-length 'string-ref 'string-set! 'string->symbol 'string? 'symbol? 'symbol->string
		'vector 'vector-length 'vector-ref 'vector-set! 'vector? 'reverse 'zero? 'apply-helper))
  
  

(define init-fvar-table-runtime
	(lambda (runtime-support-list)
		(if (null? runtime-support-list) 
			global-table
			(begin 
				(add-to-glob-table (car runtime-support-list))
				(init-fvar-table-runtime (cdr runtime-support-list))))))
			   
		
;;;;; ASSEMBLY IMPLEMENTATION ;;;;

(define assembly-runtime-func
	(lambda ()
		(string-append	
			(zero?-assembly) new-line
			(boolean?-assembly) new-line
			(integer?-assembly) new-line
			(null?-assembly)new-line
			(string?-assembly)new-line
			(symbol?-assembly)new-line
			(vector?-assembly)new-line
			(pair?-assembly)new-line
			(char?-assembly)new-line
			(procedure?-assembly)new-line
			(car-assembly) new-line
			(cdr-assembly) new-line
			(set-cdr!-assembly) new-line
			(set-car!-assembly) new-line
			(numerator-assembly) new-line
			(denominator-assembly) new-line
			(equality-assembly) new-line
			(bigger-assembly) new-line
			(smaller-assembly) new-line
			(char->integer-assembly) new-line
			(integer->char-assembly)new-line
			(cons-assembly) new-line
			(rational?-assembly) new-line
			(number?-assembly) new-line
			(not-assembly) new-line
			(remainder-assembly) new-line
			(vector-assembly) new-line
			(make-vector-assembly) new-line
			(vector-length-assembly) new-line
			(vector-ref-assembly) new-line
			(vector-set!-assembly) new-line
			(make-string-assembly) new-line
			(string-length-assembly)new-line
			(string-ref-assembly) new-line
			(string-set!-assembly) new-line
			(eq?-assembly) new-line
			(plus-assembly) new-line
			(minus-assembly) new-line
			(multiple-assembly) new-line
			(divide-assembly) new-line
			(apply-helper-assembly) new-line
			(string->symbol-assembly) new-line
			(symbol->string-assembly) new-line
		)
	)
)



(define symbol->string-assembly
	(lambda () 
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			
			(string-append 
				"my_malloc 16 " new-line
				"mov rbx, 0 " new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp " new-line
				"mov rbp, rsp" new-line
				"mov r15 , qword[rbp+4*8]" new-line 
				
				"my_malloc 16" new-line
				"mov r10, rax" new-line
				"mov qword[r10], r15" new-line
				
				"push SOB_VOID " new-line 
				"call write_sob_if_not_void" new-line
				"add rsp, 8" new-line
				"SYMBOL_DATA_FROM_STRING r15" new-line

				"leave" new-line
				"ret" new-line
		 
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'symbol->string global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)




(define string->symbol-assembly
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
				
			(string-append 
				
				"my_malloc 16 " new-line
				"mov rbx, 0 " new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp " new-line
				"mov rbp, rsp" new-line
				"mov r15 , qword[rbp+4*8]" new-line 
				"mov r8, symbol_table" new-line  
				"mov rax, qword [r8]"	new-line	
				"str2sym_loop:" new-line
				"DATA_LOWER rax" new-line 
				"cmp rax, 0" new-line
				"je  str2sym_fail" new-line
				"add rax, start_of_data" new-line
				"mov r14, rax" new-line
				"mov rax, qword [rax]" new-line  
				"mov rbx, rax" new-line  
				"DATA_UPPER rbx" new-line  
				"add rbx, start_of_data" new-line
				"mov rbx, qword [rbx]"	 new-line		
				"mov rcx, rbx" new-line
				"mov r9, rax" new-line
				"mov rax,rcx" new-line
				"DATA rax" new-line
				"add rax, start_of_data" new-line
				"mov rax, qword [rax]"  new-line
				"mov rcx, rax" new-line
				"mov rax, r9" new-line
				"cmp rcx, r15"  new-line 
				"je  str2sym_success" new-line
				"jmp str2sym_loop" new-line
				"str2sym_fail:" new-line 
				"my_malloc 8" new-line
				"mov qword [rax], r15" new-line
				"sub rax, start_of_data" new-line
				"sal rax, TYPE_BITS" new-line
				"or  rax, T_SYMBOL" new-line
				"mov r13, rax" new-line
				"mov r12,r13"	new-line		
				"my_malloc 8" new-line
				"mov qword [rax], r13" new-line
				"mov r13, rax" new-line
				"sub r13, start_of_data"	 new-line
				"sal r13, 34" new-line
				"my_malloc 8" new-line
				"mov qword [rax], r13" new-line
				"mov r10, rax"		new-line	
				"sub r10, start_of_data" new-line  
				"mov r13, qword [r14]" new-line
				"sar r13, TYPE_BITS" new-line
				"add r13, r10" new-line
				"sal r13, TYPE_BITS" new-line
				"mov qword [r14], r13" new-line
				"mov rax,r12"		new-line
				"jmp str2sym_exit" new-line
				"str2sym_success: " new-line
				"mov rax,rbx"  new-line
				"str2sym_exit:" new-line
				"leave" new-line
				"ret" new-line
		 
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'string->symbol global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)

(define apply-helper-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp " new-line
				"mov rbp, rsp" new-line
				"mov rcx , qword[rbp+4*8]" new-line 
				"mov r13, qword[rbp+5*8]" new-line 
				"mov rbx, r13" new-line
				;"TYPE r13" new-line
				;"cmp r13, T_PAIR" new-line
				;"jne param_not_list" new-line
				"my_malloc 16" new-line
				"mov r10, rax" new-line
				"mov qword[r10], rbx" new-line
				"my_malloc 16" new-line
				"mov r11, rax" new-line
				"mov qword[r11], rcx" new-line
				
				"mov rdx,0" new-line
				"apply_loop: " new-line
				"mov r14,rbx" new-line
				"cmp rbx, ["(lookup-in-table  '() constant-table)"]" new-line
				"je apply_the_apply" new-line
				"CAR r14" new-line 
				"push r14" new-line
				"CDR rbx" new-line 
				"inc rdx" new-line  
				"jmp apply_loop" new-line
				"apply_the_apply:" new-line
				"push rdx" new-line
				"mov rbx, rcx" new-line
				"CLOSURE_ENV rbx" new-line
				"CLOSURE_CODE rcx" new-line
				"push rbx" new-line
				"call rcx" new-line
				"inc rdx" new-line
				"shl rdx,3" new-line
				"add rsp, rdx" new-line
				;"param_not_list:" new-line
				"leave" new-line
				"ret" new-line
				
				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'apply-helper global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define plus-assembly
     (lambda()
          (let ((closure-body-start (label_closure_body_start))
                (closure-body-exit (label_closure_body_exit)))
               (string-append
                    "my_malloc 16" new-line 
                    "mov rbx, 0" new-line 
                    "MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line                
                    "jmp " closure-body-exit new-line

                    closure-body-start ":" new-line
                    "push rbp" new-line 
                    "mov rbp, rsp" new-line 

                    "mov rbx, qword[rbp+3*8]" new-line ;n -> num of loops
                    "mov r9, rbx" new-line ;r9=num of loops
                    "mov r12, 0" new-line ;r12=iterator for num of args 
                    "mov r10, 1" new-line ;r10=mechane of ans
                    "mov r11, 0" new-line ;r11=mone of ans
                    
                    "plus_loop_lable:" new-line
                    "cmp r9, r12" new-line ;did we add all args?
                    "je end_of_plus_lable" new-line
                                   
                    "mov rax, qword[rbp+8*(r12+4)]" new-line ;next arg to add
                    "mov r14, rax" new-line         
                    "mov r8, rax" new-line

                    "TYPE rax" new-line
                    "cmp rax, T_INTEGER" new-line ;next arg is frag?
                    "jne not_int_second_lable" new-line

                    "DATA r14" new-line ;r14=mone of next arg
                    "mov r8, 1" new-line ;r8=mechane of next arg
                    "jmp plus_second_int_lable" new-line

                    "not_int_second_lable:" new-line ; x / y -> x=r14, y=r8
                    "PLUS_NUMERATOR r14" new-line
                    "DATA r14" new-line ;r14=mone 

                    "PLUST_DENOMINATOR r8" new-line
                    "DATA r8" new-line ;r14=mechane

                    "plus_second_int_lable:" new-line
                    "mov rax, r11" new-line ;r11=first arg or mone first arg(x1)
                    "mul r8" new-line ;r8=1 or mechane second arg(y2)
                    "mov rbx, rax" new-line

                    "mov rax, r14" new-line ;r14=second arg or mone second arg(x2)
                    "mul r10" new-line ;r10=1 or mechane first arg(y1)
                    "add rbx, rax" new-line ;rbx=add two args or just mone (after common mechane)

                    "mov rax, r10" new-line ;r10=1 or mechane first arg(y1)
                    "mul r8" new-line ;r8=1 or mechane second arg(y2) rax=y1*y2
                    
                    "mov rcx, rax" new-line ;rcx=y1*y2
                    "mov rax, rbx" new-line ;rax=add two args or just mone (after common mechane)
                    "mov rbx, rcx" new-line ;rbx=y1*y2
                    "mov r11, rax" new-line ; x / y -> x=r11    (r14)r11 = final mone
                    "mov r10, rbx" new-line ;y=r10              (r15)r10 = final mechane

                    "add r12, 1" new-line                   
                    "jmp plus_loop_lable" new-line

                    "end_of_plus_lable:" new-line
                    "mov r14, r11" new-line
                    "mov r15, r10" new-line

                    "cmp rbx, 1" new-line ;mechane=1 ?
                    "je finish_plus_lable" new-line

                    "gcd_lable:" new-line
                    "GCD r14, r15" new-line
                    "mov rsi, rax" new-line ;rsi=gcd
                    "mov rax, r14" new-line
                    "div rsi" new-line ;rax=r14/gcd
                    "mov r14, rax" new-line
                    "mov rax, r15" new-line
                    "div rsi" new-line ;rax=r15/gcd
                    "mov r15, rax" new-line

                    "cmp r15, 1" new-line
                    "je temp1" new-line

                    "lable1:" new-line
                    "MAKE_LITERAL_INT r14" new-line
                    "MAKE_LITERAL_INT r15" new-line
                    "my_malloc 8" new-line
                    "mov qword[rax], r14" new-line
                    "sub rax , start_of_data" new-line
                    "mov r8, rax" new-line
                    
                    "my_malloc 8" new-line
                    "mov qword[rax], r15" new-line
                    "sub rax , start_of_data" new-line
                    "mov r9, rax" new-line

                    "shl r8, 30" new-line
                    "or r8, r9" new-line
                    "shl r8, 4" new-line
                    "or r8, T_FRACTION" new-line

                    "mov rax, r8" new-line
                    "jmp plus_exit" new-line

                    "finish_plus_1_lable:" new-line
                    "mov rax, 1" new-line
                    "jmp finish_plus_lable" new-line

                    "temp1:" new-line
                    "mov rax, r14" new-line

                    "finish_plus_lable:" new-line
                    "MAKE_LITERAL_INT rax" new-line ;possible r14 instead of rax 
                    "jmp plus_exit" new-line

                    "plus_exit:"new-line
                    "leave" new-line
                    "ret" new-line

                    closure-body-exit ":" new-line
                     "mov rax, qword[rax]" new-line 
                     "mov qword ["  (lookup-in-table '+ global-table) "], rax" new-line
                     "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
               ) 
          )
     )
)

(define minus-assembly
     (lambda()
          (let ((closure-body-start (label_closure_body_start))
                (closure-body-exit (label_closure_body_exit)))
               (string-append
                    "my_malloc 16" new-line 
                    "mov rbx, 0" new-line 
                    "MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line                
                    "jmp " closure-body-exit new-line

                    closure-body-start ":" new-line
                    "push rbp" new-line 
                    "mov rbp, rsp" new-line 

                    "mov rbx, qword[rbp+3*8]" new-line ;n -> num of loops
                    "mov r9, rbx" new-line ;r9=num of loops
                    "mov r12, 0" new-line ;r12=iterator for num of args 
                    "mov r10, 1" new-line ;r10=mechane of ans
                    "mov r11, 0" new-line ;r11=mone of ans

                    "cmp r9, 1" new-line ;one arg
                    "jne minus_loop_lable" new-line
                    "mov rax, qword[rbp+4*8]" new-line ;rax=first arg
                    "mov r14, rax" new-line         
                    "mov r15, rax" new-line
                    "TYPE rax" new-line
                    "cmp rax, T_INTEGER" new-line ;arg is frag?
                    "jne arg_not_int" new-line
                    "DATA r14" new-line
                    "mov r15, 1" new-line
                    "mov rax, r14" new-line
                    "neg rax" new-line
                    "jmp finish_minus_lable" new-line

                    "arg_not_int:" new-line
                    "PLUS_NUMERATOR r14" new-line
                    "DATA r14" new-line
                    "neg r14" new-line

                    "PLUST_DENOMINATOR r15" new-line
                    "DATA r15" new-line
                    "jmp minus_gcd_lable" new-line
                    
                    "minus_loop_lable:" new-line
                    "cmp r9, r12" new-line ;did we sub all args?
                    "je end_of_minus_lable" new-line
                                   
                    "mov rax, qword[rbp+8*(r12+4)]" new-line ;next arg to sub
                    "mov r14, rax" new-line         
                    "mov r8, rax" new-line

                    "TYPE rax" new-line
                    "cmp rax, T_INTEGER" new-line ;next arg is frag?
                    "jne minus_not_int_second_lable" new-line

                    "DATA r14" new-line ;r14=mone of next arg
                    "mov r8, 1" new-line ;r8=mechane of next arg
                    "jmp minus_second_int_lable" new-line

                    "minus_not_int_second_lable:" new-line ; x / y -> x=r14, y=r8e
                    "PLUS_NUMERATOR r14" new-line
                    "DATA r14" new-line

                    "PLUST_DENOMINATOR r8" new-line
                    "DATA r8" new-line

                    "minus_second_int_lable:" new-line
                    "mov rax, r11" new-line ;r11=first arg or mone first arg(x1)
                    "mul r8" new-line ;r8=1 or mechane second arg(y2)
                    "mov rbx, rax" new-line ;rbx=x1*y2

                    "mov rax, r14" new-line ;r14=second arg or mone second arg(x2)
                    "mul r10" new-line ;r10=1 or mechane first arg(y1)
                    ;rax=x2*y1

                    "cmp r12, 0" new-line ;is this the first arg?
                    "jne minus_not_first_arg_lable" new-line
                    "add rbx, rax" new-line ;first arg counts as positive (as is)
                    "jmp continue_after_first_arg_lable" new-line

                    "minus_not_first_arg_lable:" new-line
                    "sub rbx, rax" new-line ;rbx=sub two args or just mone (after common mechane)

                    "continue_after_first_arg_lable:" new-line
                    "mov rax, r10" new-line ;r10=1 or mechane first arg(y1)
                    "mul r8" new-line ;r8=1 or mechane second arg(y2)
                    ;rax=y1*y2

                    "mov rcx, rax" new-line ;rcx=y1*y2
                    "mov rax, rbx" new-line ;rax=sub two args or just mone (after common mechane)
                    "mov rbx, rcx" new-line ;rbx=y1*y2
                    "mov r11, rax" new-line ; x / y -> x=r11    (r14)r11 = final mone
                    "mov r10, rbx" new-line ;y=r10              (r15)r10 = final mechane

                    "add r12, 1" new-line                   
                    "jmp minus_loop_lable" new-line

                    "end_of_minus_lable:" new-line
                    "mov r14, r11" new-line
                    "mov r15, r10" new-line

                    "cmp rbx, 1" new-line ;mechane=1 ?
                    "je finish_minus_lable" new-line
                    ;;is mone or mechane negative?
                    "cmp r14, 0" new-line ;is mone<0
                    "jg minus_gcd_lable" new-line ;mone>0
                    "mov r10, -1" new-line
                    "neg r14" new-line

                    "minus_gcd_lable:" new-line
                    "GCD r14, r15" new-line
                    "mov rsi, rax" new-line ;rsi=gcd
                    "mov rax, r14" new-line
                    "div rsi" new-line ;rax=r14/gcd
                    "mov r14, rax" new-line
                    "mov rax, r15" new-line
                    "div rsi" new-line ;rax=r15/gcd
                    "mov r15, rax" new-line

                    "cmp r10, 0" new-line
                    "jg pos_continue" new-line ;ans is positive
                    "neg r14" new-line

                    "pos_continue:" new-line
                    "cmp r15, 1" new-line
                    "je minus_temp1" new-line

                    "minus_lable1:" new-line
                    "MAKE_LITERAL_INT r14" new-line
                    "MAKE_LITERAL_INT r15" new-line
                    "my_malloc 8" new-line
                    "mov qword[rax], r14" new-line
                    "sub rax , start_of_data" new-line
                    "mov r8, rax" new-line
                    
                    "my_malloc 8" new-line
                    "mov qword[rax], r15" new-line
                    "sub rax , start_of_data" new-line
                    "mov r9, rax" new-line

                    "shl r8, 30" new-line
                    "or r8, r9" new-line
                    "shl r8, 4" new-line
                    "or r8, T_FRACTION" new-line

                    "mov rax, r8" new-line
                    "jmp minus_exit" new-line

                    "minus_temp1:" new-line
                    "mov rax, r14" new-line

                    "finish_minus_lable:" new-line
                    "MAKE_LITERAL_INT rax" new-line ;possible r14 instead of rax 
                    "jmp minus_exit" new-line

                    "minus_exit:"new-line
                    "leave" new-line
                    "ret" new-line

                    closure-body-exit ":" new-line
                     "mov rax, qword[rax]" new-line 
                     "mov qword ["  (lookup-in-table '- global-table) "], rax" new-line
                     "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
               ) 
          )
     )
)

(define multiple-assembly
     (lambda()
          (let ((closure-body-start (label_closure_body_start))
                 (closure-body-exit (label_closure_body_exit)))
               (string-append
                    "my_malloc 16" new-line 
                    "mov rbx, 0" new-line 
                    "MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line                
                    "jmp " closure-body-exit new-line                 
                    
                    closure-body-start ":" new-line
                    "push rbp" new-line 
                    "mov rbp, rsp" new-line 

                    "mov rbx, qword[rbp+3*8]" new-line ;n -> num of loops
                    "mov r9, rbx" new-line ;r9=num of loops
                    "mov r12, 0" new-line ;r12=iterator for num of args 
                    "mov r10, 1" new-line ;r10=mechane of ans
                    "mov r11, 1" new-line ;r11=mone of ans

                    "multi_loop_lable:" new-line
                    "cmp r9, r12" new-line ;did we multi all args?
                    "je end_of_multi_lable" new-line
                                   
                    "mov rax, qword[rbp+8*(r12+4)]" new-line ;next arg to multi
                    "mov r14, rax" new-line         
                    "mov r8, rax" new-line

                    "TYPE rax" new-line
                    "cmp rax, T_INTEGER" new-line ;next arg is frag?
                    "jne multi_not_int_second_lable" new-line

                    "DATA r14" new-line ;r14=mone of next arg(x2)
                    "mov r8, 1" new-line ;r8=mechane of next arg(y2)
                    "jmp multi_second_int_lable" new-line

                    "multi_not_int_second_lable:" new-line ; x / y -> x=r14, y=r8
                    "PLUS_NUMERATOR r14" new-line
                    "DATA r14" new-line

                    "PLUST_DENOMINATOR r8" new-line
                    "DATA r8" new-line

                    "multi_second_int_lable:" new-line
                    "mov rax, r11" new-line ;r11=first arg or mone first arg(x1)
                    "mul r14" new-line ;r14= second arg or mone of second arg
                    "mov rbx, rax" new-line ;rbx=x1*x2

                    "mov rax, r10" new-line ;r10=1 or mechane of first arg(y1)
                    "mul r8" new-line ;r8=1 or mechane second arg(y2)
                    ;rax=y1*y2

                    "mov rcx, rax" new-line ;rcx=y1*y2
                    "mov rax, rbx" new-line ;rax=x1*x2
                    "mov rbx, rcx" new-line ;rbx=y1*y2
                    "mov r11, rax" new-line ; x / y -> x=r11    (r14)r11 = final mone
                    "mov r10, rbx" new-line ;y=r10              (r15)r10 = final mechane

                    "add r12, 1" new-line                   
                    "jmp multi_loop_lable" new-line

                    "end_of_multi_lable:" new-line
                    "mov r14, r11" new-line
                    "mov r15, r10" new-line

                    "cmp rbx, 1" new-line ;mechane=1 ?
                    "je finish_multi_lable" new-line

                    "multi_gcd_lable:" new-line
                    "GCD r14, r15" new-line
                    "mov rsi, rax" new-line ;rsi=gcd
                    "mov rax, r14" new-line
                    "div rsi" new-line ;rax=r14/gcd
                    "mov r14, rax" new-line
                    "mov rax, r15" new-line
                    "div rsi" new-line ;rax=r15/gcd
                    "mov r15, rax" new-line

                    "cmp r15, 1" new-line
                    "je multi_temp1" new-line

                    "multi_lable1:" new-line
                    "MAKE_LITERAL_INT r14" new-line
                    "MAKE_LITERAL_INT r15" new-line
                    "my_malloc 8" new-line
                    "mov qword[rax], r14" new-line
                    "sub rax , start_of_data" new-line
                    "mov r8, rax" new-line
                    
                    "my_malloc 8" new-line
                    "mov qword[rax], r15" new-line
                    "sub rax , start_of_data" new-line
                    "mov r9, rax" new-line

                    "shl r8, 30" new-line
                    "or r8, r9" new-line
                    "shl r8, 4" new-line
                    "or r8, T_FRACTION" new-line

                    "mov rax, r8" new-line
                    "jmp multi_exit" new-line

                    "multi_temp1:" new-line
                    "mov rax, r14" new-line

                    "finish_multi_lable:" new-line
                    "MAKE_LITERAL_INT rax" new-line ;possible r14 instead of rax 
                    "jmp multi_exit" new-line

                    "multi_exit:"new-line
                    "leave" new-line
                    "ret" new-line

                    closure-body-exit ":" new-line
                     "mov rax, qword[rax]" new-line 
                     "mov qword ["  (lookup-in-table '* global-table) "], rax" new-line
                     "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
               )
          )
     )
)
(define divide-assembly
     (lambda()
          (let ((closure-body-start (label_closure_body_start))
                 (closure-body-exit (label_closure_body_exit)))
               (string-append
                    "my_malloc 16" new-line 
                    "mov rbx, 0" new-line 
                    "MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line                
                    "jmp " closure-body-exit new-line                 
                    
                    closure-body-start ":" new-line
                    "push rbp" new-line 
                    "mov rbp, rsp" new-line 

                    "mov rbx, qword[rbp+3*8]" new-line ;n -> num of loops
                    "mov r9, rbx" new-line ;r9=num of loops
                    "mov r12, 0" new-line ;r12=iterator for num of args 
                    "mov r10, 1" new-line ;r10=mechane of ans
                    "mov r11, 1" new-line ;r11=mone of ans

                     "cmp r9, 1" new-line ;if only one arg
                     "jne div_loop_lable" new-line ;more than one arg
                     "mov rax, qword[rbp+8*4]" new-line
                     "mov r14, rax" new-line ;r14=mone
                     "mov r15, rax" new-line ;r15=mechane
                     "TYPE rax" new-line
                     "cmp rax, T_INTEGER" new-line
                     "jne only_arg_not_int" new-line
                     "DATA r15" new-line ;arg is int, so 1/arg. r15=arg (mechane)
                     "mov r14, 1" new-line ;r14=1 (mone)
                     "cmp r15, 0" new-line ;is arg<0 ??
                     "jg div_lable1" new-line ;arg>0 
                     "neg r15" new-line
                     "neg r14" new-line
                     "jmp div_lable1" new-line ;goto make frag and finish 

                     "only_arg_not_int:" new-line ;arg is frag
                     "PLUST_DENOMINATOR r14" new-line
                     "DATA r14" new-line ;r14=mone of only arg (mechane of arg)
                     "PLUS_NUMERATOR r15" new-line
                     "DATA r15" new-line ;r15=mechane of only arg (mone of arg)
                     "cmp r15, 0" new-line ;is mechane>0 ??
                     "jg div_gcd_lable" new-line
                     "neg r14" new-line
                     "neg r15" new-line
                     "jmp div_gcd_lable" new-line

                    "div_loop_lable:" new-line
                    "cmp r9, r12" new-line ;did we divide all args?
                    "je end_of_div_lable" new-line
                                   
                    "mov rax, qword[rbp+8*(r12+4)]" new-line ;next arg to divide
                    "mov r14, rax" new-line         
                    "mov r8, rax" new-line

                    "TYPE rax" new-line
                    "cmp rax, T_INTEGER" new-line ;next arg is frag?
                    "jne div_not_int_second_lable" new-line

                    "DATA r14" new-line ;r14=mone of next arg(x2)
                    "mov r8, 1" new-line ;r8=mechane of next arg(y2)
                    "jmp div_second_int_lable" new-line

                    "div_not_int_second_lable:" new-line ; x / y -> x2=r14, y2=r8
                    "PLUS_NUMERATOR r14" new-line
                    "DATA r14" new-line

                    "PLUST_DENOMINATOR r8" new-line
                    "DATA r8" new-line

                    "div_second_int_lable:" new-line
                    "mov rax, r11" new-line ;r11=first arg or mone first arg(x1)
                    "mul r8" new-line ;r8=mechane of second arg(y2)
                    "mov rbx, rax" new-line ;rbx=x1*y2

                    "mov rax, r10" new-line ;r10=1 or mechane of first arg(y1)
                    "mul r14" new-line ;r14=mone of second arg(x2)
                    ;rax=y1*x2
                    
                    "cmp r12, 0" new-line ;is this the first arg?
                    "jne continue" new-line

                    "mov rcx, rax" new-line ;rcx=y1*x2
                    "mov rax, rbx" new-line ;rax=x1*y2
                    "mov rbx, rcx" new-line ;rbx=y1*x2
                    "mov r10, rax" new-line ; x / y -> x=r11    (r14)r11 = final mone
                    "mov r11, rbx" new-line ;y=r10              (r15)r10 = final mechane
                    
                    "continue:" new-line
                    "mov rcx, rax" new-line ;rcx=y1*y2
                    "mov rax, rbx" new-line ;rax=x1*x2
                    "mov rbx, rcx" new-line ;rbx=y1*y2
                    "mov r11, rax" new-line ; x / y -> x=r11    (r14)r11 = final mone
                    "mov r10, rbx" new-line ;y=r10              (r15)r10 = final mechane

                    "add r12, 1" new-line                   
                    "jmp div_loop_lable" new-line

                    "end_of_div_lable:" new-line
                    "mov r14, r11" new-line
                    "mov r15, r10" new-line

                    "cmp rbx, 1" new-line ;mechane=1 ?
                    "je finish_div_lable" new-line
                    ;;;;is mone or mechane negative
                    "cmp r14, 0" new-line
                    "jg denom_check" new-line ;mone>0
                    "mov r11, -1" new-line ;indicates mone<0
                    "neg r14" new-line

                    "denom_check:" new-line
                    "cmp r15, 0" new-line
                    "jg div_gcd_lable" new-line ;mechane>0
                    "mov r10, -1" new-line ;indicates mechane<0
                    "neg r15" new-line

                    "div_gcd_lable:" new-line
                    "GCD r14, r15" new-line
                    "mov rsi, rax" new-line ;rsi=gcd
                    "mov rax, r14" new-line
                    "div rsi" new-line ;rax=r14/gcd
                    "mov r14, rax" new-line
                    "mov rax, r15" new-line
                    "div rsi" new-line ;rax=r15/gcd
                    "mov r15, rax" new-line

                    "cmp r11, 0" new-line ;back to starting sign
                    "jg denom_check_after" new-line ;mona was positive
                    "neg r14" new-line

                    "denom_check_after:" new-line
                    "cmp r10, 0" new-line ;mechane was positive??
                    "jg div_continue" new-line ;mechane was positive
                    "neg r14" new-line ;mechane was negtive, so change mone sign

                    "div_continue:" new-line
                    "cmp r15, 1" new-line
                    "je div_temp1" new-line

                    "div_lable1:" new-line
                    "MAKE_LITERAL_INT r14" new-line
                    "MAKE_LITERAL_INT r15" new-line
                    "my_malloc 8" new-line
                    "mov qword[rax], r14" new-line
                    "sub rax , start_of_data" new-line
                    "mov r8, rax" new-line
                    
                    "my_malloc 8" new-line
                    "mov qword[rax], r15" new-line
                    "sub rax , start_of_data" new-line
                    "mov r9, rax" new-line

                    "shl r8, 30" new-line
                    "or r8, r9" new-line
                    "shl r8, 4" new-line
                    "or r8, T_FRACTION" new-line

                    "mov rax, r8" new-line
                    "jmp div_exit" new-line

                    "div_temp1:" new-line
                    "mov rax, r14" new-line

                    "finish_div_lable:" new-line
                    "MAKE_LITERAL_INT rax" new-line ;possible r14 instead of rax 
                    "jmp div_exit" new-line

                    "div_exit:"new-line
                    "leave" new-line
                    "ret" new-line

                    closure-body-exit ":" new-line
                     "mov rax, qword[rax]" new-line 
                     "mov qword ["  (lookup-in-table '/ global-table) "], rax" new-line
                     "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
               )
          )
     )
)


(define eq?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov r8, qword [rbp + 4*8]" new-line
				"mov r9, qword [rbp + 5*8]" new-line
				"my_malloc 16" new-line
				"mov r10, rax" new-line
				"mov qword[r10], r8" new-line
				"my_malloc 16" new-line
				"mov r11, rax" new-line
				"mov qword[r11], r9" new-line
				"cmp r8, r9"new-line
				"je indeed_eq" new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"jmp eq_exit" new-line 
				"indeed_eq:"new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"eq_exit:" new-line
				"leave" new-line
				"ret"new-line

				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'eq? global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)



(define string-set!-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov r8, qword [rbp + 4*8]" new-line
				"mov r9, qword [rbp + 5*8]" new-line
				"mov rbx, qword [rbp + 6*8]" new-line
				"my_malloc 16" new-line
				"mov r10, rax" new-line
				"mov qword[r10], r8" new-line
				"my_malloc 16" new-line
				"mov r11, rax" new-line
				"mov qword[r11], r9" new-line
				"my_malloc 16" new-line
				"mov r13, rax" new-line
				"mov qword[r13], rbx" new-line
				"DATA r9" new-line
				"STRING_ELEMENTS r8" new-line
				"add r8, r9" new-line
				"DATA rbx" new-line
				"mov byte[r8], bl" new-line 
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
				"add rsp, 8" new-line
				"leave" new-line
				"ret"new-line
				
				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'string-set! global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define string-ref-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov r8, qword [rbp + 4*8]" new-line
				"mov r9, qword [rbp + 5*8]" new-line
				"my_malloc 16" new-line
				"mov r10, rax" new-line
				"mov qword[r10], r8" new-line
				"my_malloc 16" new-line
				"mov r11, rax" new-line
				"mov qword[r11], r9" new-line
				"DATA r9" new-line
				"STRING_ELEMENTS r8" new-line
				"add r8, r9" new-line
				"movzx rax, byte[r8]" new-line
				"shl rax, TYPE_BITS" new-line
				"or rax, T_CHAR" new-line
				"leave" new-line
				"ret"new-line

				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'string-ref global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)





(define string-length-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rcx, qword [rbp + 4*8]" new-line
				"my_malloc 16" new-line
				"mov r8, rax" new-line
				"mov qword[r8], rcx" new-line
				"STRING_LENGTH rcx" new-line
				"sal rcx, 4" new-line
				"or rcx, T_INTEGER" new-line
				"mov rax, rcx" new-line
				"leave" new-line
				"ret"new-line
				
				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'string-length global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)



(define make-string-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line
				"mov r8, qword [rbp + 4*8]" new-line
				"mov r14, qword [rbp + 3*8]" new-line
				"cmp r14, 1" new-line
				"jne makestring_2args" new-line
				"mov rdx, SOB_VOID" new-line
				"jmp makeString_continue" new-line
				
				"makestring_2args:" new-line
				"mov rdx, qword [rbp + 5*8]" new-line
				
				"makeString_continue:"
				"my_malloc 16" new-line
				"mov r10, rax" new-line
				"mov qword[r10], r8" new-line
				"my_malloc 16" new-line
				"mov r11, rax" new-line
				"mov qword[r11], rdx" new-line
				"DATA r8" new-line	
				"DATA rdx" new-line	

				"my_malloc 16" new-line
				"mov rcx,rax" new-line
				"mov r9, 0" new-line
				
				"make_string_items_loop:" new-line
				"cmp r9, r8" new-line
				"je  make_string_comp" new-line
				"mov byte[rcx + r9], dl" new-line
				"inc r9" new-line
				"jmp make_string_items_loop" new-line
				
				"make_string_comp:" new-line
				"mov r13, r8" new-line
				"sal r13, ((WORD_SIZE - TYPE_BITS) >> 1)" new-line
				"sub rax, start_of_data" new-line
				"add r13, rax" new-line
				"sal r13, TYPE_BITS" new-line
				"or  r13, T_STRING" new-line
				"mov rax, r13" new-line
				"leave" new-line
				"ret"new-line

				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'make-string global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define vector-set!-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov r8, qword [rbp + 4*8]" new-line
				"mov r9, qword [rbp + 5*8]" new-line
				"mov r12, qword [rbp + 6*8]" new-line
				; "my_malloc 16" new-line
				; "mov r10, rax" new-line
				; "mov qword[r10], r8" new-line
				; "my_malloc 16" new-line
				; "mov r11, rax" new-line
				; "mov qword[r11], r9" new-line
				; "my_malloc 16" new-line
				; "mov r13, rax" new-line
				; "mov qword[r13], r12" new-line
				
				"DATA r9" new-line
				"sal r9, 3" new-line
				"VECTOR_ELEMENTS r8" new-line
				
				"add r8, r9" new-line
				"mov rax, qword [r8]"new-line
				"mov qword[rax], r12" new-line
				"mov rax, SOB_VOID" new-line
				"add rsp, 8" new-line
				"leave" new-line
				"ret"new-line
				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'vector-set! global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define vector-ref-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov r8, qword [rbp + 4*8]" new-line
				"mov r9, qword [rbp + 5*8]" new-line
				"my_malloc 16" new-line
				"mov r10, rax" new-line
				"mov qword[r10], r8" new-line
				"my_malloc 16" new-line
				"mov r11, rax" new-line
				"mov qword[r11], r9" new-line
				"DATA r9" new-line
				"sal r9, 3" new-line
				"VECTOR_ELEMENTS r8" new-line
				"add r8, r9" new-line
				"mov rax, qword [r8]"new-line
				"push qword [rax]" new-line
				"call write_sob_if_not_void" new-line
				"add rsp, 8" new-line
				"leave" new-line
				"ret"new-line

				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'vector-ref global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)




(define vector-length-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rcx, qword [rbp + 4*8]" new-line
				"my_malloc 16" new-line
				"mov r8, rax" new-line
				"mov qword[r8], rcx" new-line
				"DATA_UPPER rcx" new-line
				"shl rcx, TYPE_BITS" new-line
				"or rcx, T_INTEGER" new-line
				"mov rax, rcx" new-line
				"leave" new-line
				"ret"new-line

				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'vector-length global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)

(define make-vector-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16 " new-line
				"mov rbx, 0 " new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx, "closure-body-start new-line
				"jmp " closure-body-exit  new-line
				
				closure-body-start":" new-line
				"push rbp " new-line
				"mov rbp, rsp" new-line
				"mov r9,0" new-line
				"mov rcx , qword[rbp+4*8]" new-line ;size
				"mov r15, qword [rbp + 3*8]" new-line
				"cmp r15, 1" new-line
				"jne vec_continue" new-line
				"mov r8, 3" new-line
				"jmp vec_continue2" new-line
				
				"vec_continue:" new-line
				"mov r8, qword[rbp+5*8]" new-line  ;value
				"vec_continue2:" new-line
				"mov r12,rcx" new-line
				"DATA r12" new-line
				"imul r12, 8 " new-line
				"my_malloc r12" new-line 
				"mov rsi, rax " new-line ;rsi= address of vector
				"DATA rcx" new-line
				"make_vector_loop:" new-line
				"cmp rcx,r9" new-line
				"je make_vector_loop_exit" new-line
				"my_malloc 8 " new-line
				"mov qword[rax], r8 "  new-line
				"mov [rsi +r9*8], rax " new-line
				"inc r9" new-line
				"jmp make_vector_loop " new-line

				"make_vector_loop_exit: " new-line
				"mov rax, rcx " new-line
				"sal rax, ((WORD_SIZE - TYPE_BITS) >> 1) " new-line
				"sub rsi, start_of_data " new-line
				"add rax, rsi "  new-line
				"sal rax, TYPE_BITS " new-line
				"or rax, T_VECTOR " new-line

				"pop rbp " new-line
				"ret " new-line 
				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'make-vector global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)




(define vector-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov r12, qword [rbp + 3*8]" new-line
				"mov rdi, r12" new-line
				"shl rdi, 3" new-line
				"my_malloc rdi"  new-line
				"mov r14, rax" new-line
				"mov rcx, rax" new-line
				"mov rdx, 0" new-line
				
				"vector_items_loop:" new-line
				"cmp rdx, r12" new-line
				"je  vector_comp" new-line
				"push rcx" new-line
				"push rdx" new-line
				"my_malloc 8" new-line
				"pop rdx" new-line
				"mov r15, qword [rbp + (4 + rdx)*8]" new-line
				"mov qword[rax], r15" new-line
				"mov r15, rax" new-line
				"pop rcx" new-line
				"mov qword[rcx], r15" new-line
				"add rcx, 8" new-line
				"inc rdx" new-line
				"jmp vector_items_loop" new-line
				
				"vector_comp:" new-line
				"mov r13, r12" new-line
				"sal r13, ((WORD_SIZE - TYPE_BITS) >> 1)" new-line
				"sub r14, start_of_data" new-line
				"add r13, r14" new-line
				"sal r13, TYPE_BITS" new-line
				"or  r13, T_VECTOR" new-line
				"mov rax, r13" new-line
				"pop rbp" new-line
				"ret"new-line

				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'vector global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)

(define remainder-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line				
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rdx, 0" new-line
				"mov rax, qword[rbp+4*8]" new-line 
				"DATA rax" new-line
				"mov rcx, qword[rbp+5*8]" new-line
				"DATA rcx" new-line

				"cmp rax, 0" new-line ;is first arg<0 ??
				"jg posi_first" new-line
				"mov r10, -1" new-line ;indicates first arg<0
				"neg rax" new-line
				"posi_first:" new-line
				"cmp rcx, 0" new-line ;is second arg>0 ??
				"jg posi_second" new-line
				;"mov r11, -1" new-line ;indicates second arg<0
				"neg rcx" new-line

				"posi_second:" new-line
				"div rcx" new-line
				"cmp r10, 0" new-line
				"jg remainder_continue" new-line
				"neg rdx" new-line
				"remainder_continue:" new-line
				"sal rdx, TYPE_BITS" new-line
				"or rdx, T_INTEGER" new-line
				"mov rax,rdx" new-line
				
				"remainder_exit:" new-line
				"pop rbp" new-line
				"ret" new-line
				
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'remainder global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)

(define not-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"mov rcx, rax" new-line
				"TYPE rax" new-line
				"cmp rax, T_BOOL" new-line
				"jne not_false_lable" new-line
				"DATA rcx" new-line
				"cmp rcx, 1" new-line
				"je not_false_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp not_exit" new-line
				"not_false_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"not_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'not global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define rational?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"mov rcx, rax" new-line
				"TYPE rax" new-line
				"cmp rax, T_INTEGER" new-line
				"jne try_fraction" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp rational_exit" new-line
				"try_fraction:"
				"TYPE rcx" new-line
				"cmp rax, T_FRACTION" new-line
				"jne not_rational_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp rational_exit" new-line
				"not_rational_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"rational_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'rational? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define number?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"mov rcx, rax" new-line
				"TYPE rax" new-line
				"cmp rax, T_INTEGER" new-line
				"jne try_fraction_in_number" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp number_exit" new-line
				"try_fraction_in_number:"
				"TYPE rcx" new-line
				"cmp rax, T_FRACTION" new-line
				"jne not_number_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp number_exit" new-line
				"not_number_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"number_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'number? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)




(define char->integer-assembly
	(lambda ()
		(string-append
			"my_malloc 16 " new-line
			"mov rbx, 0 " new-line 
			"MAKE_LITERAL_CLOSURE rax,rbx,char2int_body_lable " new-line
			"jmp char2int_exit " new-line
			
			"char2int_body_lable: " new-line 
			"push rbp" new-line 
			"mov rbp, rsp " new-line 
			"mov rcx, qword[rbp+4*8]" new-line 
			"mov rbx, T_CHAR" new-line
			"xor rcx, rbx" new-line
			"mov rbx, T_INTEGER" new-line
			"xor rcx, rbx" new-line
			"mov rax, rcx" new-line
			"pop rbp "  new-line
			"ret "  new-line

			"char2int_exit: " new-line
			"mov rax, qword[rax]" new-line 
			"mov qword ["  (lookup-in-table 'char->integer global-table) "], rax" new-line
			"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
		)
	)
)


(define integer->char-assembly
	(lambda ()
		(string-append
			"my_malloc 16 " new-line
			"mov rbx, 0 " new-line 
			"MAKE_LITERAL_CLOSURE rax,rbx,int2char_body_lable " new-line
			"jmp int2char_exit " new-line
			
			"int2char_body_lable: " new-line 
			"push rbp" new-line 
			"mov rbp, rsp " new-line 
			"mov rcx, qword[rbp+4*8]" new-line 
			"mov rbx, T_CHAR" new-line
			"xor rcx, rbx" new-line
			"mov rbx, T_INTEGER" new-line
			"xor rcx, rbx" new-line
			"mov rax, rcx" new-line
			"pop rbp "  new-line
			"ret "  new-line

			"int2char_exit: " new-line
			"mov rax, qword[rax]" new-line 
			"mov qword ["  (lookup-in-table 'integer->char global-table) "], rax" new-line
			"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
		)
	)
)

(define cons-assembly
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			 (closure-body-exit (label_closure_body_exit)))
			(string-append
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax, rbx, " closure-body-start new-line 			
				"jmp " closure-body-exit new-line

				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line
				"mov rax, qword[rbp+3*8]" new-line ;args num
				"cmp rax, 2" new-line
				"jne cons_arg_num_error_label" new-line 

				"mov rdx, qword[rbp+4*8]" new-line ;first arg=car
				"my_malloc 8" new-line
				"mov r8, rax" new-line
				"mov qword [r8], rdx" new-line ;first arg adress
				"mov rcx, qword[rbp+5*8]" new-line ;second arg=cdr
				"my_malloc 8" new-line
				"mov r9, rax" new-line
				"mov qword [r9], rcx" new-line ;second arg adress
				"my_malloc 8" new-line 
				"MAKE_MALLOC_LITERAL_PAIR rax, r8, r9" new-line
				"mov rax, qword [rax]" new-line

				"jmp cons_exit" new-line

				"cons_arg_num_error_label:" new-line
				"mov rax, ["(lookup-in-table  #f constant-table)"]" new-line
				"jmp cons_exit" new-line

				"cons_exit:" new-line
				"pop rbp" new-line
				"ret" new-line

				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'cons global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)

(define bigger-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov r10, qword [rbp + 3*8]" new-line 
				"mov rax, qword [rbp + 4*8]" new-line 
				"mov r9,0" new-line
				"bigger_body_lable:" new-line
				"inc r9" new-line
				"mov rbx,rax" new-line
				"cmp r10, r9" new-line
				"je indeed_bigger" new-line
				"mov r12,4" new-line
				"add r12,r9" new-line
				"shl r12,3" new-line 
				"mov rcx,[rbp+r12]" new-line
				"mov r14, rcx" new-line
				"NUMERATOR rax" new-line
				"mov rsi, rax"new-line
				"DENOMINATOR rcx" new-line
				"mov r15, rax" new-line
				"mul rsi" new-line
				"mov rcx, rax" new-line
				"DENOMINATOR rbx" new-line
				"mov rsi, rax" new-line
				"NUMERATOR r14" new-line
				"mul rsi" new-line
				"mov rbx, rax" new-line
				"sub rcx, rbx" new-line
				"cmp rcx, 0" new-line
				"jle not_bigger" new-line
				"mov rax, [rbp+r12]" new-line
				"jmp bigger_body_lable" new-line
				"indeed_bigger:" new-line
				"mov rax, " "["(lookup-in-table  #t constant-table)"]" new-line
				"jmp bigger_exit"  new-line
				"not_bigger:" new-line
				"mov rax, " "["(lookup-in-table  #f constant-table)"]" new-line
				"bigger_exit:" new-line
				"pop rbp" new-line
				"ret" new-line
				
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table '> global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)



(define smaller-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov r10, qword [rbp + 3*8]" new-line 
				"mov rax, qword [rbp + 4*8]" new-line 
				"mov r9,0" new-line
				"smaller_body_lable:" new-line
				"inc r9" new-line
				"mov rbx,rax" new-line
				"cmp r10, r9" new-line
				"je indeed_smaller" new-line
				"mov r12,4" new-line
				"add r12,r9" new-line
				"shl r12,3" new-line 
				"mov rcx,[rbp+r12]" new-line
				"mov r14, rcx" new-line
				"NUMERATOR rax" new-line
				"mov rsi, rax"new-line
				"DENOMINATOR rcx" new-line
				"mov r15, rax" new-line
				"mul rsi" new-line
				"mov rcx, rax" new-line
				"DENOMINATOR rbx" new-line
				"mov rsi, rax" new-line
				"NUMERATOR r14" new-line
				"mul rsi" new-line
				"mov rbx, rax" new-line
				"sub rcx, rbx" new-line
				"cmp rcx, 0" new-line
				"jge not_smaller" new-line
				"mov rax, [rbp+r12]" new-line
				"jmp smaller_body_lable" new-line
				"indeed_smaller:" new-line
				"mov rax, " "["(lookup-in-table  #t constant-table)"]" new-line
				"jmp smaller_exit"  new-line
				"not_smaller:" new-line
				"mov rax, " "["(lookup-in-table  #f constant-table)"]" new-line
				"smaller_exit:" new-line
				"pop rbp" new-line
				"ret" new-line
				
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table '< global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)



(define equality-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov r10, qword [rbp + 3*8]" new-line 
				"mov rax, qword [rbp + 4*8]" new-line 
				"mov r9,0" new-line
				"equal_body_lable:" new-line
				"inc r9" new-line
				"mov rbx,rax" new-line
				"cmp r10, r9" new-line
				"je indeed_equal" new-line
				"mov r12,4" new-line
				"add r12,r9" new-line
				"shl r12,3" new-line 
				"mov rcx,[rbp+r12]" new-line
				"mov r14, rcx" new-line
				"NUMERATOR rax" new-line
				"mov rsi, rax"new-line
				"DENOMINATOR rcx" new-line
				"mov r15, rax" new-line
				"mul rsi" new-line
				"mov rcx, rax" new-line
				"DENOMINATOR rbx" new-line
				"mov rsi, rax" new-line
				"NUMERATOR r14" new-line
				"mul rsi" new-line
				"mov rbx, rax" new-line
				"sub rcx, rbx" new-line
				"cmp rcx, 0" new-line
				"jne not_equal" new-line
				"mov rax, [rbp+r12]" new-line
				"jmp equal_body_lable" new-line
				"indeed_equal:" new-line
				"mov rax, " "["(lookup-in-table  #t constant-table)"]" new-line
				"jmp equal_exit"  new-line
				"not_equal:" new-line
				"mov rax, " "["(lookup-in-table  #f constant-table)"]" new-line
				"equal_exit:" new-line
				"pop rbp" new-line
				"ret" new-line
				
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table '= global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define numerator-assembly
    (lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit))
			  (arg_num_error_label (lable_wrong_arg_num)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax, rbx, " closure-body-start new-line 			
				"jmp " closure-body-exit new-line

				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+3*8]" new-line ;args num
				"cmp rax, 1" new-line
				"jne " arg_num_error_label new-line 
				
				"mov rax, qword[rbp+4*8]" new-line 
				"NUMERATOR rax" new-line
				"sal rax, TYPE_BITS" new-line
				"or  rax, T_INTEGER" new-line
				
				arg_num_error_label ":" new-line
				"jmp numerator_exit" new-line

				"numerator_exit:" new-line
				"pop rbp" new-line
				"ret" new-line

				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'numerator global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define denominator-assembly
    (lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit))
			  (arg_num_error_label (lable_wrong_arg_num)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax, rbx, " closure-body-start new-line 			
				"jmp " closure-body-exit new-line

				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+3*8]" new-line ;args num
				"cmp rax, 1" new-line
				"jne " arg_num_error_label new-line 
				
				"mov rax, qword[rbp+4*8]" new-line 
				"DENOMINATOR rax" new-line
				"sal rax, TYPE_BITS" new-line
				"or  rax, T_INTEGER" new-line
				
				arg_num_error_label ":" new-line
				"jmp denominator_exit" new-line

				"denominator_exit:" new-line
				"pop rbp" new-line
				"ret" new-line

				closure-body-exit ":" new-line
				"mov rax, qword[rax]" new-line 
				"mov qword ["  (lookup-in-table 'denominator global-table) "], rax" new-line
				"mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)

(define car-assembly
	(lambda()
		(let ((closure-body-start (label_closure_body_start))
			 (closure-body-exit (label_closure_body_exit)))
		   (string-append
		   		"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax, rbx, " closure-body-start new-line 			
				"jmp " closure-body-exit new-line

				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+3*8]" new-line ;args num
				"cmp rax, 1" new-line
				"jne car_arg_num_error_label" new-line 

				"mov rax, qword[rbp+4*8]" new-line 
				"mov rbx, rax" new-line
				"TYPE rbx" new-line
				"cmp rbx, T_PAIR" new-line
				"jne car_not_pair_lable" new-line
				"CAR rax" new-line
				"jmp car_exit" new-line

				"car_not_pair_lable:" new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line ;;maybe instead of #f -> NIL ??
				"jmp car_exit" new-line
				"car_arg_num_error_label:" new-line
				"jmp car_exit" new-line

				"car_exit:" new-line
				"pop rbp" new-line
				"ret" new-line

				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'car global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
		   )
		)
	)
)


(define cdr-assembly
	(lambda()
		(let ((closure-body-start (label_closure_body_start))
			 (closure-body-exit (label_closure_body_exit)))
		   (string-append
		   		"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax, rbx, " closure-body-start new-line 			
				"jmp " closure-body-exit new-line

				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+3*8]" new-line ;args num
				"cmp rax, 1" new-line
				"jne cdr_arg_num_error_label" new-line 

				"mov rax, qword[rbp+4*8]" new-line 
				"mov rbx, rax" new-line
				"TYPE rbx" new-line
				"cmp rbx, T_PAIR" new-line
				"jne cdr_not_pair_lable" new-line
				"CDR rax" new-line
				"jmp cdr_exit" new-line

				"cdr_not_pair_lable:" new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line ;;maybe instead of #f -> NIL ??
				"jmp cdr_exit" new-line
				"cdr_arg_num_error_label:" new-line
				"jmp car_exit" new-line

				"cdr_exit:" new-line
				"pop rbp" new-line
				"ret" new-line

				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'cdr global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
		   )
		)
	)
)


(define set-car!-assembly
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			 (closure-body-exit (label_closure_body_exit)))
			(string-append

				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax, rbx, " closure-body-start new-line 			
				"jmp " closure-body-exit new-line

				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+3*8]" new-line ;args num
				"cmp rax, 2" new-line
				"jne set_car_arg_num_error_label" new-line 
				"mov rax, qword[rbp+4*8]" new-line
				"mov rbx, rax" new-line ;rbx<-the list
				"TYPE rbx" new-line
				"cmp rbx, T_PAIR" new-line
				"jne set_car_not_pair_lable" new-line

				"mov rax, qword[rbp+4*8]" new-line
				"mov rbx, rax" new-line ;rbx<-the list
				"mov rax, qword[rbp+5*8]" new-line
				"mov rcx, rax" new-line ;rcx<-new arg for car

				"DATA_UPPER rbx" new-line
				"add rbx, start_of_data" new-line
				"mov qword [rbx], rcx" new-line
				"mov rax, ["(lookup-in-table  (if #f #f) constant-table)"]" new-line

				"set_car_arg_num_error_label:" new-line
				"jmp set_car_exit" new-line

				"set_car_not_pair_lable:" new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line 
				"jmp set_car_exit" new-line

				"set_car_exit:" new-line
				"pop rbp" new-line
				"ret" new-line

				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'set-car! global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define set-cdr!-assembly
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			 (closure-body-exit (label_closure_body_exit)))
			(string-append
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax, rbx, " closure-body-start new-line 			
				"jmp " closure-body-exit new-line

				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+3*8]" new-line ;args num
				"cmp rax, 2" new-line
				"jne set_cdr_arg_num_error_label" new-line 
				"mov rax, qword[rbp+4*8]" new-line
				"mov rbx, rax" new-line ;rbx<-the list
				"TYPE rbx" new-line
				"cmp rbx, T_PAIR" new-line
				"jne set_cdr_not_pair_lable" new-line

				"mov rax, qword[rbp+4*8]" new-line
				"mov rbx, rax" new-line ;rbx<-the list
				"mov rax, qword[rbp+5*8]" new-line
				"mov rcx, rax" new-line ;rcx<-new arg for car
				"DATA_LOWER rbx" new-line
				"add rbx, start_of_data" new-line
				"mov qword [rbx], rcx" new-line
				"mov rax, ["(lookup-in-table  (if #f #f) constant-table)"]" new-line

				"set_cdr_arg_num_error_label:" new-line
				"jmp set_cdr_exit" new-line

				"set_cdr_not_pair_lable:" new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line ;;maybe instead of #f -> NIL ??
				"jmp set_cdr_exit" new-line

				"set_cdr_exit:" new-line
				"pop rbp" new-line
				"ret" new-line

				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'set-cdr! global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)




(define zero?-assembly
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"DATA rax" new-line
				"cmp rax, 0" new-line
				"jne not_zero_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp zero_exit" new-line
				"not_zero_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"zero_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'zero? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)	
)	


(define boolean?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"TYPE rax" new-line
				"cmp rax, T_BOOL" new-line
				"jne not_bool_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp bool_exit" new-line
				"not_bool_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"bool_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'boolean? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define integer?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"TYPE rax" new-line
				"cmp rax, T_INTEGER" new-line
				"jne not_int_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp int_exit" new-line
				"not_int_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"int_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'integer? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)
	
	
	

(define char?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"TYPE rax" new-line
				"cmp rax, T_CHAR" new-line
				"jne not_char_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp char_exit" new-line
				"not_char_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"char_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'char? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)



(define null?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"TYPE rax" new-line
				"cmp rax, T_NIL" new-line
				"jne not_null_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp null_exit" new-line
				"not_null_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"null_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'null? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define string?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"TYPE rax" new-line
				"cmp rax, T_STRING" new-line
				"jne not_str_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp str_exit" new-line
				"not_str_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"str_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'string? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)



(define symbol?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"TYPE rax" new-line
				"cmp rax, T_SYMBOL" new-line
				"jne not_sym_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp sym_exit" new-line
				"not_sym_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"sym_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'symbol? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define pair?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"TYPE rax" new-line
				"cmp rax, T_PAIR" new-line
				"jne not_pair_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp pair_exit" new-line
				"not_pair_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"pair_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'pair? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)



(define vector?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"TYPE rax" new-line
				"cmp rax, T_VECTOR" new-line
				"jne not_vec_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp vec_exit" new-line
				"not_vec_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"vec_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'vector? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


(define procedure?-assembly 
	(lambda ()
		(let ((closure-body-start (label_closure_body_start))
			  (closure-body-exit (label_closure_body_exit)))
			(string-append 
				"my_malloc 16" new-line 
				"mov rbx, 0" new-line 
				"MAKE_LITERAL_CLOSURE rax,rbx," closure-body-start new-line 			
				"jmp " closure-body-exit new-line
				
				closure-body-start ":" new-line
				"push rbp" new-line 
				"mov rbp, rsp" new-line 
				"mov rax, qword[rbp+4*8]" new-line 
				"TYPE rax" new-line
				"cmp rax, T_CLOSURE" new-line
				"jne not_proc_lable" new-line
				"mov rax, [" (lookup-in-table #t constant-table) "]" new-line
				"jmp proc_exit" new-line
				"not_proc_lable:"new-line
				"mov rax, [" (lookup-in-table #f constant-table) "]" new-line
				"proc_exit:"new-line
				"pop rbp" new-line
				"ret" new-line
				closure-body-exit ":" new-line
				 "mov rax, qword[rax]" new-line 
				 "mov qword ["  (lookup-in-table 'procedure? global-table) "], rax" new-line
				 "mov rax, [" (lookup-in-table (if #f #f) constant-table)"]" new-line
			)
		)
	)
)


 ;;;;;;;;;;;;;; CODE GEN ;;;;;;;;;;;;;;;;;;;;;;


  
(define code-gen-const 
	(lambda (exp)
		(string-append 
			"mov rbx, " "["(lookup-in-table exp constant-table) "]" new-line
			"mov rax, rbx" new-line) 
	)
)

(define code-gen-pvar 
	(lambda (exp env)
		(string-append
			"mov rbx, qword [rbp + (4 + " (number->string (cadr exp)) " ) * 8]" new-line
			"mov rax, rbx" new-line)
	)
)

(define code-gen-bvar 
	(lambda (exp env)
		 (string-append 
			"mov rbx, qword [rbp + 2 * 8]" new-line
			"mov rbx, qword [rbx + " (number->string (cadr exp)) "* 8]" new-line
			"mov rbx, qword [rbx + " (number->string (caddr exp)) "* 8]" new-line
			"mov rax, rbx" new-line	
		)
	)
)

(define code-gen-fvar 
	(lambda (exp env)
		(string-append 
			"mov rbx, qword ["  (lookup-in-table  exp global-table) "]" new-line
			"mov rax, rbx" new-line)
	)
)
(define code-gen-define 
	(lambda (exp env)
		(string-append 
			(code-gen (cadr exp) env) new-line 
			"mov qword [" (lookup-in-table  (cadar exp) global-table) "] , rax" new-line 
			"mov rax, ["(lookup-in-table (if #f #f) constant-table)"]" new-line  )))
	



(define code-gen-if
	(lambda (exp env)
		(let ((else-label(label_if3_else))
			  (end-label (label_if3_exit))
			  (test (car exp))
			  (dit (cadr exp))
			  (dif (caddr exp)))
			  
			(string-append 
				(code-gen test env) new-line 
				"cmp rax, " "["(lookup-in-table #f constant-table)"]" new-line 
				"je " else-label new-line
				(code-gen dit env) new-line
				"jmp " end-label new-line
				else-label ":" new-line
				(code-gen dif env) new-line 
				end-label ":"

			)
		)
	)
)


(define code-gen-lambda
	(lambda (sexpr env)
		(let((body (cadr sexpr))
			 (next-env (+ env 1))
			 (firstStartLoop (lable-start-loop))
			 (firstExitLoop (lable-exit-loop))
			 (secondStartLoop (lable-start-loop))
			 (secondExitLoop (lable-exit-loop))
			 (startLambda (lable-lambda-start))
			 (exitLambda (lable-lambda-exit)))
		
		    (cond ((= env 0)
				(string-append 
						 
					"my_malloc 8" new-line 
					"mov qword [rax], 0" new-line
					"mov rbx, rax"  new-line   
					"my_malloc 16" new-line 
					"MAKE_LITERAL_CLOSURE rax,rbx," startLambda new-line
					"mov rax, [rax]" new-line
					"jmp " exitLambda new-line
					startLambda ":" new-line
					"push rbp" new-line
					"mov rbp, rsp" new-line
					(code-gen body next-env) new-line
					"mov rcx, qword [rbp+3*8]"  new-line   
					"pop rbp" new-line
					"ret" new-line
					exitLambda":" new-line))

				(else 
				(string-append 

					"my_malloc 16" new-line 
					"mov rdx,rax" new-line
					"my_malloc " (number->string (* 8 next-env))  new-line
					"mov rbx,rax" new-line
					"mov rcx, [rbp+8*2]" new-line
					"mov r9," (number->string  next-env) new-line
					"mov r8,0" new-line
					
					
					firstStartLoop ":" new-line
					"cmp r8, r9"  new-line
					"je " firstExitLoop new-line 
					"mov r12,r8" new-line
					"shl r12,3" new-line
					"add r12,rcx" new-line
					"mov rdi, qword [r12]" new-line
					"inc r8" new-line
					"mov qword [rbx+r8*8],rdi" new-line
					"jmp " firstStartLoop new-line
					firstExitLoop ":" new-line 
				
					
					"mov rcx, [rbp+8*3]" new-line
					"shl rcx, 3"	new-line				
					"my_malloc rcx"   new-line 
					"mov rcx,rax" new-line
					"mov rax, [rbp+8*3]"  new-line
					"mov r8,0" new-line
					
					secondStartLoop ":" new-line
					"cmp r8, rax" new-line
					"je " secondExitLoop new-line
					"mov rdi,[rbp+8*(r8+4)]" new-line
					"mov [rcx+8*r8],rdi " new-line
					"inc r8" new-line
					"jmp " secondStartLoop new-line new-line
					secondExitLoop ":" new-line
					
					"mov [rbx], rcx" new-line
					"mov rax,rdx" new-line
					"MAKE_LITERAL_CLOSURE rax,rbx," startLambda new-line
					"mov rax, [rax]" new-line
					"jmp " exitLambda new-line new-line
					
					startLambda ":" new-line
					"push rbp" new-line
					"mov rbp, rsp" new-line
					(code-gen body next-env) new-line
					"leave" new-line
					"ret" new-line
					exitLambda ":" new-line
				))
			)
		)
	)
)


(define code-gen-lambda-opt
	(lambda (sexpr env)
		(let((body (caddr sexpr))
			 (next-env (+ env 1))
			 (params-num (length (append (car sexpr) (list (cadr sexpr)))))
			 (firstStartLoop (lable-start-loop-opt))
			 (firstExitLoop (lable-exit-loop-opt))
			 (secondStartLoop (lable-start-loop-opt))
			 (secondExitLoop (lable-exit-loop-opt))
			 (paramsLoop (lable-lambda-opt-params-loop))
			 (paramsExit (lable-lambda-opt-params-exit))
			 (startLambda (lable-lambda-start-opt))
			 (exitLambda (lable-lambda-exit-opt)))
		
		    (cond ((= env 0)
				(string-append 
						 
					"my_malloc 8" new-line 
					"mov qword [rax], 0" new-line
					"mov rbx, rax"  new-line   
					"my_malloc 16" new-line 
					"MAKE_LITERAL_CLOSURE rax,rbx," startLambda new-line
					"mov rax, [rax]" new-line
					"jmp " exitLambda new-line
					startLambda ":" new-line
					"push rbp" new-line
					"mov rbp, rsp" new-line
					
					"mov r8, qword [rbp + 3*8]" new-line
					"cmp r8," (number->string params-num) new-line
					"jle " paramsExit new-line

					"mov r14, r8" new-line
					"add r14, 3" new-line
					"shl r14, 3" new-line				
					"add r14, rbp"  new-line			
					"mov r13, qword [r14]" new-line	
					"my_malloc 8" new-line
					"mov qword[rax], r13" new-line
					"mov r13, rax" new-line
					"mov r15, " "["(lookup-in-table '() constant-table)"]" new-line 
					"my_malloc 8" new-line
					"mov qword[rax], r15" new-line
					"mov r15, rax" new-line
					"my_malloc 8" new-line
					"MAKE_MALLOC_LITERAL_PAIR rax, r13, r15" new-line
					"mov rax, [rax]" new-line 
					"mov [r14],rax" new-line 
					"mov r9, r8" new-line
					"sub r9," (number->string params-num) new-line		
					"mov r10, 0" new-line
					paramsLoop   ":" new-line
					"cmp r10, r9" new-line
					"je "  paramsExit new-line		
					"mov r13, qword [r14]" new-line	
					"my_malloc 8" new-line
					"mov qword[rax], r13" new-line
					"mov r13, rax" new-line
					"sub r14, 8"	new-line			
					"mov r12, qword [r14]"	new-line
					"my_malloc 8" new-line
					"mov qword[rax], r12" new-line
					"mov r12, rax" new-line
					"my_malloc 8" new-line
					"MAKE_MALLOC_LITERAL_PAIR rax, r12, r13" new-line
					"mov rax, qword [rax]" new-line
					"mov qword [r14], rax" new-line
					"dec r8" new-line
					"inc r10" new-line
					"jmp " paramsLoop new-line
					paramsExit ":" new-line
					
					
					(code-gen body next-env) new-line
					"mov rcx, qword [rbp+3*8]"  new-line   
					"pop rbp" new-line
					"ret" new-line
					exitLambda":" new-line))

				(else 
				(string-append 

					"my_malloc 16" new-line 
					"mov rdx,rax" new-line
					"my_malloc " (number->string (* 8 next-env))  new-line
					"mov rbx,rax" new-line
					"mov rcx, [rbp+8*2]" new-line
					"mov r9," (number->string  next-env) new-line
					"mov r8,0" new-line
					
					
					firstStartLoop ":" new-line
					"cmp r8, r9"  new-line
					"je " firstExitLoop new-line 
					"mov r12,r8" new-line
					"shl r12,3" new-line
					"add r12,rcx" new-line
					"mov rdi, qword [r12]" new-line
					"inc r8" new-line
					"mov qword [rbx+r8*8],rdi" new-line
					"jmp " firstStartLoop new-line
					 
					
					; firstStartLoop ":" new-line
					; "cmp r8, r9"  new-line
					; "je " firstExitLoop new-line 
					; "mov rdi, [rcx+8*r8]" new-line
					; "inc r8" new-line
					; "mov [rbx+r8*8],rdi" new-line
					; "jmp " firstStartLoop new-line new-line
					; firstExitLoop ":" new-line 
					
					
					firstExitLoop ":" new-line
					"mov rcx, [rbp+8*3]" new-line
					"shl rcx, 3"	new-line				
					"my_malloc rcx"   new-line 
					"mov rcx,rax" new-line
					"mov rax, [rbp+8*3]"  new-line
					"mov r8,0" new-line
					
					secondStartLoop ":" new-line
					"cmp r8, rax" new-line
					"je " secondExitLoop new-line
					"mov rdi,[rbp+8*(r8+4)]" new-line
					"mov [rcx+8*r8],rdi " new-line
					"inc r8" new-line
					"jmp " secondStartLoop new-line new-line
					secondExitLoop ":" new-line
					
					"mov [rbx], rcx" new-line
					"mov rax,rdx" new-line
					"MAKE_LITERAL_CLOSURE rax,rbx," startLambda new-line
					"mov rax, [rax]" new-line
					"jmp " exitLambda new-line new-line
					
					startLambda ":" new-line
					"push rbp" new-line
					"mov rbp, rsp" new-line
					
					
					"mov r8, qword [rbp + 3*8]" new-line
					"cmp r8," (number->string params-num) new-line
					"jle " paramsExit new-line

					"mov r14, r8" new-line
					"add r14, 3" new-line
					"shl r14, 3" new-line				
					"add r14, rbp"  new-line			
					"mov r13, qword [r14]" new-line	
					"my_malloc 8" new-line
					"mov qword[rax], r13" new-line
					"mov r13, rax" new-line
					"mov r15, " "["(lookup-in-table '() constant-table)"]" new-line 
					"my_malloc 8" new-line
					"mov qword[rax], r15" new-line
					"mov r15, rax" new-line
					"my_malloc 8" new-line
					"MAKE_MALLOC_LITERAL_PAIR rax, r13, r15" new-line
					"mov rax, [rax]" new-line 
					"mov [r14],rax" new-line 
					"mov r9, r8" new-line
					"sub r9," (number->string params-num) new-line		
					"mov r10, 0" new-line
					paramsLoop   ":" new-line
					"cmp r10, r9" new-line
					"je "  paramsExit new-line		
					"mov r13, qword [r14]" new-line	
					"my_malloc 8" new-line
					"mov qword[rax], r13" new-line
					"mov r13, rax" new-line
					"sub r14, 8"	new-line			
					"mov r12, qword [r14]"	new-line
					"my_malloc 8" new-line
					"mov qword[rax], r12" new-line
					"mov r12, rax" new-line
					"my_malloc 8" new-line
					"MAKE_MALLOC_LITERAL_PAIR rax, r12, r13" new-line
					"mov rax, qword [rax]" new-line
					"mov qword [r14], rax" new-line
					"dec r8" new-line
					"inc r10" new-line
					"jmp " paramsLoop new-line
					paramsExit ":" new-line
					
					
					(code-gen body next-env) new-line
					"leave" new-line
					"ret" new-line
					exitLambda ":" new-line
				))
			)
		)
	)
)
					
(define code-gen-or
	(lambda (expr env)
		(let* ((start-or-lable (lable-start-or))
			   (exit-or-lable (lable-exit-or)))
			(letrec ((helper (lambda (e)
						(if (null? e) 
							(string-append exit-or-lable ":" new-line )
                            (string-append 
								(code-gen (car e) env) new-line
								"cmp rax, " "["(lookup-in-table #f constant-table)"]" new-line 
								"jne " exit-or-lable new-line
								(helper (cdr e)))))))
				(string-append start-or-lable ":" new-line (helper expr))))))


(define code-gen-seq
	(lambda (e env)
		(code-gen-seq-helper (cadr e) env)))

		
(define code-gen-seq-helper
	(lambda (e env)
		(if (null? e)
			""
			(string-append
				new-line
				(code-gen (car e) env)
				(code-gen-seq-helper (cdr e) env)))))	
		 
		 
(define code-gen-set
  (lambda (expr env)
    (let* ((type (caadr expr)))
		(cond ((equal? type 'pvar) (code-gen-set-pvar expr env))
			  ((equal? type 'bvar) (code-gen-set-bvar expr env))
			  ((equal? type 'fvar) (code-gen-set-fvar expr env))
			  (else (string-append "jmp L_program_end" new-line))))))

			  
(define code-gen-set-fvar
	(lambda (expr env)
		(let* ((fvar (cadr expr))
			   (set-expr (caddr expr)))
			(string-append            
                (code-gen set-expr env)
				"mov qword [" (lookup-in-table  (cadr fvar) global-table) "] , rax" new-line 
				"mov rax, ["(lookup-in-table (if #f #f) constant-table)"]" new-line  
			   ))))

(define code-gen-set-pvar
    (lambda (expr env)
        (let* ((pvar (cadr expr))
			   (set-expr (caddr expr)))
			(string-append 
				(code-gen set-expr env)
				"mov qword [rbp + (4 + " (number->string (caddr pvar)) " ) * 8], rax" new-line
				"mov rax, ["(lookup-in-table (if #f #f) constant-table)"]" new-line 
				))))

				
(define code-gen-set-bvar
    (lambda (expr env)
        (let* ((bvar (cadr expr))
			   (set-expr (caddr expr)))
			(string-append 
				(code-gen set-expr env)
				"mov rbx, qword [rbp + 2 * 8]" mynew-line
				"mov rbx, qword [rbx + " (number->string (caddr bvar)) " * 8]" new-line
				"mov qword [rbx + " (number->string (cadddr value)) " * 8], rax" new-line
				"mov rax, ["(lookup-in-table (if #f #f) constant-table)"]" new-line 
				))))
				
				
(define code-gen-box
	(lambda (sexpr env)
		(string-append 
			(code-gen (car sexpr) env) new-line
	 	    "mov r8, rax" new-line
			"my_malloc 8" new-line
	 	    "mov qword [rax], r8" new-line
		)
	)
)

(define code-gen-box-get
	(lambda (sexpr env)
		(string-append 
			(code-gen (car sexpr) env) new-line
	 	    "mov qword rax, [rax]" new-line
		)
	)
)

(define code-gen-box-set
	(lambda (sexpr env)
		(string-append 
			(code-gen (cadr sexpr) env) new-line
			"mov qword rbx, rax" new-line
			(code-gen (car sexpr) env) new-line
			"mov [rax],rbx" new-line
			"mov rax," "["(lookup-in-table  (if #f #f) constant-table)"]" new-line 
		)
	)
)		 


(define code-gen-applic
	(lambda (expr args args-num env)
		(let ((labelError (label_error_closure)))
			(cond 
				((not (null? args))
					(string-append 
					(code-gen (car args) env) new-line
					"push rax" new-line
					(code-gen-applic expr (cdr args) args-num env) new-line))

				(else 
					(code-gen-applic-aux expr args args-num env) ;labelError)
				)
			)
		)
	)
)


(define code-gen-applic-aux
	(lambda (expr args args-num env) ;labelError)
		(string-append 
			"mov rdx, " (number->string args-num) new-line
			"push rdx" new-line
			(code-gen expr env) new-line
			"mov rdi,rax" new-line
			"mov r8,rdi" new-line
			"CLOSURE_ENV r8" new-line
			"push r8" new-line
			"CLOSURE_CODE rdi" new-line
			"call rdi" new-line
			"mov rcx,[rsp+ 8]" new-line
			"add rcx, 2" new-line
			"shl rcx , 3"  new-line
			"mov r8, rcx" new-line
			"add rsp, r8"  new-line
			
		)
	)
)


(define code-gen-tc-applic
	(lambda (expr args args-num env)
		(let (;(labelError (label_error_closure))
			  (tc-app-loop-start (label_tc_applic_loop_start))
			  (tc-app-loop-exit (label_tc_applic_loop_exit)	))
			(cond ((not (null? args))
					(string-append 
						(code-gen (car args) env) new-line			 
						"push rax" new-line
						(code-gen-tc-applic expr (cdr args) args-num env) new-line))
					(else 
						(code-gen-tc-applic-aux expr args args-num env tc-app-loop-start tc-app-loop-exit)) ;labelError))
			)
		)
	)
)



(define code-gen-tc-applic-aux
	(lambda (expr args args-num env sLable eLable) ;labelError)
		(string-append 
			"mov rdx, " (number->string args-num) new-line
			"push rdx" new-line
			(code-gen expr env) new-line
			"mov rdi,rax" new-line
			"mov r9,rdi" new-line
			;"TYPE rax" new-line
			;"cmp rax, T_CLOSURE" new-line
			;"JNE error"  new-line ;need to generate a new Lables for error
			"CLOSURE_ENV r9" new-line
			"push r9" new-line
			"mov r13,rdi" new-line
			;"error:" new-line
			"mov r8,rbp" new-line ;r8 old rbp
			"mov r10, qword [rbp+8*3]" new-line  
			"shl r10,3" new-line
			"add r10,3*8" new-line
			"add r10,r8" new-line
			"push qword [rbp+8]" new-line
			"mov rbp,qword [rbp]" new-line
			"mov r15,qword [rsp+2*8]" new-line
			"add r15,3" new-line
			"mov rsi,0" new-line

			sLable ":" new-line
			"cmp rsi,r15" new-line
			"je " eLable new-line
			"mov rcx,r8" new-line
			"shl rsi,3" new-line
			"sub rcx, rsi" new-line
			"sub rcx,8" new-line
			"mov rcx,qword [rcx]" new-line
			"mov rdx,r10" new-line
			"sub rdx,rsi" new-line
			"mov qword [rdx],rcx" new-line
			"shr rsi,3" new-line
			"inc rsi" new-line
			"jmp " sLable new-line
			eLable ":" new-line
			"sub rsi,1" new-line
			"shl rsi,3" new-line
			"sub r10,rsi" new-line
			"mov rsp,r10" new-line
			 "CLOSURE_CODE rdi" new-line
			"jmp rdi " new-line
		)
	)
)


(define code-gen
	(lambda (sexpr env)
		(cond ((null? sexpr)(list))
			  ((equal? (car sexpr) 'const) (code-gen-const (cadr sexpr)))
			  ((equal? (car sexpr) 'if3) (code-gen-if (cdr sexpr) env))
			  ((equal? (car sexpr) 'or) (code-gen-or (cadr sexpr) env))
			  ((equal? (car sexpr) 'seq) (code-gen-seq sexpr env))
			  ((equal? (car sexpr) 'pvar) (code-gen-pvar (cdr sexpr) env))
			  ((equal? (car sexpr) 'bvar) (code-gen-bvar (cdr sexpr) env))
			  ((equal? (car sexpr) 'fvar) (code-gen-fvar (cadr sexpr) env))
			  ((equal? (car sexpr) 'define) (code-gen-define (cdr sexpr) env))
			  ((equal? (car sexpr) 'set) (code-gen-set sexpr env))
			  ((equal? (car sexpr) 'box) (code-gen-box (cdr sexpr) env))
			  ((equal? (car sexpr) 'box-get) (code-gen-box-get (cdr sexpr) env))
			  ((equal? (car sexpr) 'box-set) (code-gen-box-set (cdr sexpr) env))
			  ((equal? (car sexpr) 'lambda-simple) (code-gen-lambda (cdr sexpr) env))
			  ((equal? (car sexpr) 'lambda-opt) (code-gen-lambda-opt (cdr sexpr) env))
			  ((equal? (car sexpr) 'applic) (code-gen-applic (cadr sexpr)(reverse (caddr sexpr))(length (caddr sexpr)) env))
			  ((equal? (car sexpr) 'tc-applic) (code-gen-tc-applic (cadr sexpr)(reverse (caddr sexpr))(length (caddr sexpr)) env))
			  ;((equal? (car sexpr) 'tc-applic) (code-gen-applic (cadr sexpr)(reverse (caddr sexpr))(length (caddr sexpr)) env))
			(else 'FAIL)
		)
	)
)

(define my-code-gen
  (lambda (pes)
        (letrec 
            ((helper (lambda (pe)
                  (if (null? pe) ""
                      (string-append
					  new-line
                        (code-gen (car pe) 0) new-line
                        "push rax" new-line
                        "call write_sob_if_not_void" new-line 
						"add rsp, 8" new-line
					    ;"ret" 
                        (helper (cdr pe)))))))
            (helper pes))))



(define compile-scheme-file
	(lambda (src dest)
		(begin (init-const-table)
			(let* ((pes1 (pipeline (file->list src)))
					(rt (pipeline (file->list "project/runtime.scm")))
					(pes (append rt pes1))
				   (out (open-output-file dest 'truncate)))
				(begin 
					(build-const-table pes)
					(init-fvar-table-runtime runtime-support-list)
					(build-ge pes)
					;(display pes)
					(display
						(string-append (includeStart)
							
							new-line new-line
							"section .data"
							new-line
							"start_of_data:"
							new-line new-line
							const-table-str
							new-line new-line

							"symbol_table: " new-line
							"      dq 0       " new-line

							new-line new-line
							glob-table-str
							

							new-line new-line
							"section .text"
							new-line
							"main:"
							new-line
							"	mov rax, malloc_pointer" new-line
							"	mov qword [rax], start_of_malloc" new-line
							"	mov rax, symbol_table" new-line
							(build-runtime-symbol-table symbol-table)
							
							(assembly-runtime-func)
							(my-code-gen pes)
							new-line new-line
							;endLabel
						)
						;)
					out)
				(close-output-port out))))))		

