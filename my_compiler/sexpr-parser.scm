(load "project/pc.scm")



(define <Boolean>
  (new (*parser (char #\#))
       (*parser (char-ci #\t))
       (*caten 2)
       (*pack (lambda (_)
                #t))

       (*parser (char #\#))
       (*parser (char-ci #\f))
       (*caten 2)
       (*pack (lambda (_)
                #f))
       
       (*disj 2)
  done)
)

(define <CharPrefix>
  (new (*parser (word "#\\"))
       (*pack (lambda (ch) 
          	     (list->string ch)))
  done)
)


(define <VisibleSimpleChar>
  (new (*parser <any-char>)
       (*only-if (lambda (ch) 
                    (> (char->integer ch ) (char->integer #\space))
                  )
       )

       (*pack (lambda (ch) ch))
        
   done)
)


(define <NamedChar>
  (new (*parser (word-ci "newline"))
       (*pack (lambda(x) #\newline))
  	   (*parser (word-ci "nul"))
       (*pack (lambda(x) #\nul))
       (*parser (word-ci "page"))
       (*pack (lambda(x) #\page))
       (*parser (word-ci "return"))
       (*pack (lambda(x) #\return))
       (*parser (word-ci "space"))
       (*pack (lambda(x) #\space))
       (*parser (word-ci "tab"))
       (*pack (lambda(x) #\tab))
       (*parser (word-ci "lambda"))
       (*pack (lambda(x) (integer->char 955)))   
       (*disj 7)     
  done)
)

(define <HexChar>
   (new (*parser (range #\0 #\9))
 	      (*parser (range-ci #\a #\f))
 	      (*disj 2)
	 done)
)


(define <HexUnicodeChar>
  (new (*parser (char #\x))
       (*parser <HexChar>) *plus
       (*caten 2)
       (*pack-with (lambda (x hex) 
              (integer->char (string->number (list->string hex) 16))))  
   done)
)


(define <Char>
  (new (*parser <CharPrefix>)
       (*parser <HexUnicodeChar>)
       (*parser <NamedChar>)
       (*parser <VisibleSimpleChar>)
       (*disj 3)
       (*caten 2)
       (*pack-with (lambda (chp ch) 
                      ch))
  done)
)

(define <digit-1-9>
  (range #\1 #\9))

(define <digit-0-9>
  (range #\0 #\9))

(define <Natural>
   (new (*parser <digit-0-9>) *plus
        (*pack (lambda (x)
	  	            (string->number (list->string x))))
   done)
)


(define <Integer>
 (new (*parser (char #\+))
      (*parser <Natural>)
      (*caten 2)
      (*pack-with 
      (lambda (sign n) 
	           n))

     (*parser (char #\-))
     (*parser <Natural>)
     (*caten 2)
     (*pack-with
     (lambda (sign n) 
	          (- n)))

     (*parser <Natural>)
     (*disj 3)
  done)
)

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*caten 3)
       (*pack-with
           (lambda (int div nat)
	              (/ int nat)))
   done)
)

(define <SymbolChar>
  (new  (*parser <digit-0-9>)
        (*parser (range-ci #\a #\z) )
        (*pack (lambda(c) (if (< (char->integer c) 97)
                              (integer->char (+ 32 (char->integer c))) 
                              c)))
        (*parser (char #\!))
        (*parser (char #\$))
        (*parser (char #\^))
        (*parser (char #\*))
        (*parser (char #\-))
        (*parser (char #\_))
        (*parser (char #\=))
        (*parser (char #\+))
        (*parser (char #\<))
        (*parser (char #\>))
        (*parser (char #\?))
        (*parser (char #\/))
        (*disj 14)
  done)
)

(define <Symbol>
  (new  (*parser <SymbolChar>) *plus
        (*pack (lambda (symchar) 
                  (string->symbol (list->string symchar))))
  done)
)

(define <Number>
  (new  (*parser <Fraction>)
        (*parser <Integer>)
        (*disj 2)
        (*parser <Symbol>)
        *not-followed-by
       ; (*disj 2)
  done)
)


(define <StringHexChar>
  (new (*parser (char #\\))
       (*parser (char #\x))
       (*parser <HexChar>) *star
       (*pack (lambda (hc) 
                (integer->char(string->number (list->string hc) 16))))
       (*parser (char #\;))
       (*caten 4)
       (*pack-with (lambda(sym1 x hc sym2) hc))
    done)
)


(define <StringLiteralChar>
  (new (*parser <any-char>)
       (*parser (char #\\))
       *diff
       (*parser (char #\"))
       *diff
       (*pack (lambda (ch) ch))  
   done)
)

(define <StringMetaChar>
  (new  (*parser (word "\\\\"))
        (*pack (lambda(_) #\\))
        (*parser (word "\\\""))
        (*pack (lambda(_) #\"))
        (*parser (word "\\t"))
        (*pack (lambda(_) #\tab))
        (*parser (word "\\f"))
        (*pack (lambda(_) #\page))
        (*parser (word "\\n"))
        (*pack (lambda(_) #\newline))
        (*parser (word "\\r"))
        (*pack (lambda(_) #\return))
        (*disj 6)
  done)
)



(define <StringChar>
  (new  (*parser <StringLiteralChar>)
        (*parser <StringMetaChar>)
        (*parser <StringHexChar>)
        (*parser (char #\"))
        *diff 
        (*disj 3)
        (*pack (lambda (sc) 
                  sc))
  done)
)


(define <String>
  (new (*parser (char #\"))
       (*parser <StringChar>)
       *star
       (*parser (char #\")) 
       (*caten 3)
       (*pack-with (lambda (op str cl)
                        (list->string str)))
  done)
)


(define <Whitespace>
  (const
    (lambda (ch)
      (char<=? ch #\space)
    )
  )
)


(define <lineComment>
  (let ((<EOL_or_EOF>
    (new (*parser (char #\newline))
         (*parser <end-of-input>)
         (*disj 2)
    done)))
    (new (*parser (char #\;))
         (*parser <any-char>)
         (*parser <EOL_or_EOF>)
         *diff 
         *star
         (*parser <EOL_or_EOF>)
         (*caten 3)
    done)
  )
)

(define <SexprCommentStruct>
  (new (*parser (char #\#))
       (*parser (char #\;))
       (*delayed (lambda () <sexpr>))
       (*caten 3)
  done)
)


(define <InfixCommentStruct>
  (new (*parser (char #\#))
       (*parser (char #\;))
       (*delayed (lambda () <InfixExpression>))
       (*caten 3)
  done)
)

(define <Comment>
  (new (*parser <InfixCommentStruct>)
       (*parser <lineComment>)
       (*parser <SexprCommentStruct>)
       (*disj 3)
  done)
)

(define <Ignore>
  (disj <Comment>
        <Whitespace>)
)

(define IgnoreInExp
  (lambda (<ig>)
    (lambda (<exp>)
      (new (*parser <ig>)
           (*parser <exp>)
           (*parser <ig>)
           (*caten 3)
           (*pack-with
                (lambda (a expr b) expr))
      done)
    )
  )
)


(define <sexpr>
  ((IgnoreInExp (star <Ignore>))
    (new    
      (*parser <Boolean>)
      (*parser <Char>)
      (*parser <Number>)
      (*parser <String>)
      (*parser <Symbol>)
      (*delayed (lambda() <ProperList>))
      (*delayed (lambda() <ImproperList>))
      (*delayed (lambda() <Vector>))
      (*delayed (lambda() <Quoted>))
      (*delayed (lambda() <QuasiQuoted>))
      (*delayed (lambda() <Unquoted>))
      (*delayed (lambda() <UnquotedAndSpliced>))
      (*delayed (lambda() <CBName>))
      (*delayed (lambda() <InfixExtension>))
      (*disj 14)
    done)
  )
)


(define <ProperList>
   (new (*parser (char #\())
        (*parser <sexpr>) *star
        (*parser (char #\)))
        (*caten 3)
        (*pack-with (lambda (op ex cp) 
                        ex))
   done)
)

(define <ImproperList>
   (new (*parser (char #\())
        (*parser <sexpr>) 
        *plus
        (*parser (char #\.))
        (*parser <sexpr>)
        (*parser (char #\)))
        (*caten 5)
        (*pack-with (lambda (op ex dot ex2 cp) 
                         `(,@ex . ,ex2))) 
   done)
)

(define <Vector>
   (new (*parser (char #\#))
        (*parser (char #\())
        (*parser <sexpr>) *star
        (*parser (char #\)))
        (*caten 4)
        (*pack-with (lambda (h op ex cp) 
                        (list->vector ex)))
   done)
)

(define <Quoted>
   (new (*parser (char #\'))
        (*parser <sexpr>)
        (*caten 2)
        (*pack-with (lambda (q ex) 
                        (list 'quote ex)))
  done)
)

(define <QuasiQuoted>
  (new (*parser (char #\`))
       (*parser <sexpr>)
       (*caten 2)
       (*pack-with (lambda (qq ex) 
                        (list 'quasiquote ex)))
  done)
)

(define <Unquoted>
  (new (*parser (char #\,))
       (*parser <sexpr>)
       (*caten 2)
       (*pack-with (lambda (uq ex) 
                        (list 'unquote ex)))
  done)
)

(define <UnquotedAndSpliced>
  (new (*parser (word ",@"))
       (*parser <sexpr>)
       (*caten 2)
       (*pack-with (lambda (uqs ex) 
                        (list 'unquote-splicing ex)))
  done)
)


(define <CBNameSyntax1>
    (new (*parser (char #\@))
         (*parser <sexpr>)
         (*caten 2)
         (*pack-with (lambda (sht ex) 
                         (list 'cbname ex)))
    done)
)


(define <CBNameSyntax2>
    (new (*parser (char #\{))
         (*parser <sexpr>)
         (*parser (char #\}))
         (*caten 3)
         (*pack-with (lambda (br1 ex br2) 
                         (list 'cbname ex)))
    done)
)

(define <CBName>
    (new (*parser <CBNameSyntax1>)
         (*parser <CBNameSyntax2>)
         (*disj 2)
    done)
)



(define <InfixPrefixExtensionPrefix>
  ((IgnoreInExp (star <Ignore>))
    (new (*parser (word-ci "##"))
       (*parser (word-ci "#%"))
       (*disj 2)
       (*pack (lambda (x)
                  (list->string x)))
    done)
  )
)


(define <InfixSymbolExceptFor>
  (new  (*parser <digit-0-9>)
        (*parser (range-ci #\a #\z) )
        (*pack (lambda(c) 
                    (if(< (char->integer c) 97)
                        (integer->char (+ 32 (char->integer c))) 
                        c)))
        (*parser (char #\!))
        (*parser (char #\$))        
        (*parser (char #\_))
        (*parser (char #\=))          
        (*parser (char #\<))
        (*parser (char #\>))
        (*parser (char #\?))
        (*disj 9)
  done)
)


(define <InfixSymbol>
  (new (*parser <InfixSymbolExceptFor>) *plus
       (*pack (lambda (s) 
                  (string->symbol (list->string s))))
  done)
)

(define <InfixExpression>
  ((IgnoreInExp (star <Ignore>))
    (new (*delayed (lambda () 
                      <InfixAddInfixSub>))
    done)
  )
)


(define <InfixExtension>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <InfixExpression>)
       (*caten 2)
       (*pack-with (lambda (a b) 
                        b))
  done)
)

(define <InfixSexprEscape>
  (new
    ;(*delayed (lambda() <InfixPrefixExtensionPrefix>))
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <sexpr>)
    (*caten 2)
    (*pack-with (lambda (ext s) 
                    s))     
  done)
)



(define <InfixParen>
 ((IgnoreInExp (star <Ignore>))
     (new (*parser (char #\( ))
          (*parser <InfixExpression>)
          (*parser (char #\) ))
          (*caten 3)
          (*pack-with (lambda (op sexp cl)
                          sexp))
          (*parser <Fraction>)
          (*parser <Integer>)
          (*parser <InfixSymbol>)
          *not-followed-by
          (*disj 2)
          (*parser <InfixSymbol>)
          (*parser <InfixSexprEscape>)
          (*disj 3)
          (*disj 2)
     done)
   )
)


(define <PowerSymbol>
  (new (*parser (char #\^))
       (*parser (char #\*))
       (*parser (char #\*))
       (*caten 2)
       (*disj 2)
       (*pack (lambda (x) 
                  #\^))
  done)
)


(define <InfixArgList>
  ((IgnoreInExp (star <Ignore>))
    (new (*parser <InfixExpression>)
         (*parser (char #\,))
         (*parser <InfixExpression>)
         (*caten 2)
         (*pack-with (lambda (comma exp)
                        exp))
         *star
         (*caten 2)
         (*pack-with (lambda (x y)
                       `(,x ,@y)))
         (*parser <epsilon>)
         (*disj 2)
    done)
  )
)

(define <InfixVector>
    (lambda (a b)
          (fold-left
             (lambda (c d) 
                (list 'vector-ref c d))
                  a b)))

(define <InfixFunc>
  (lambda (a b)
        (fold-left
           (lambda (x y) 
              `(,x ,@y))
             a b)))


(define <ArrayParse>
  (new (*parser (char #\[))
       (*parser <InfixExpression>)
       (*parser (char #\]))
       (*caten 3)
  done)
)

(define <FuncParse>
  (new (*parser (char #\())
       (*parser <Ignore>) *star
       (*parser <InfixArgList>)
       (*parser <Ignore>) *star
       (*parser (char #\)))
       (*caten 5)
  done)
)

(define <InfixArrayGetOrFuncCall>
  ((IgnoreInExp (star <Ignore>))
    (new (*parser <InfixParen>)
         (*parser <ArrayParse>)
         (*pack-with (lambda (a b c) 
                        (list 'ArrayGet b)))
         (*parser <FuncParse>)
         (*pack-with (lambda (a b c d e) 
                        (list 'FuncCall c))) 
         (*disj 2)
         *star
         (*caten 2)
         (*pack-with
          (lambda (a b)
            (if (null? b) a
                (fold-left
                  (lambda (x y)
                    (if (eq? (car y) 'ArrayGet)
                        (<InfixVector> x (cdr y))
                        (<InfixFunc> x (cdr y))))
                         a b)
                )
          )
        )
      done)
  )
)


(define <ChoosePow>
  (new (*parser (char #\^))
       (*parser (word "**"))
       (*disj 2)
  done)
)

(define <InfixPow>
  ((IgnoreInExp (star <Ignore>))
    (new (*parser <InfixArrayGetOrFuncCall>)
         (*parser <ChoosePow>)
         (*pack (lambda (_) 
                      #\^))
         (*parser <InfixArrayGetOrFuncCall>)
         (*caten 2) *star
         (*caten 2)
         (*pack-with
          (lambda (a b)
            (if (null? b) a
                (fold-left
                 (lambda (x y) (list 'expt (cadr y) x))
                 (cadar (reverse b))
                 (cdr (append (reverse b) (list (list '#\^ a))))))))
    done)
  )
)


(define <InfixNeg>
  ((IgnoreInExp (star <Ignore>))
    (new (*parser <InfixPow>)
         (*parser (char #\-))
         (*parser <InfixPow>)
         (*caten 2)
         (*pack-with (lambda (sign a) 
                        `(- ,a)))                                  
         (*disj 2)
    done)
  )
) 


(define <SexprHelper>
  (lambda (p op1 op2)
      (new (*parser p)
           (*parser (char op1))
           (*parser (char op2))
           (*disj 2)
           (*pack (lambda (op) 
                      (string->symbol (list->string (list op)))))
           (*parser p)
           (*caten 2) *star
           (*caten 2)
           (*pack-with (lambda (a b)
                          (if (null? b) a
                              (fold-left (lambda (x y) 
                                    (list (car y) x (cadr y)))
                                    (list (caar b) a (cadar b))
                                    (cdr b)
                              )
                            )
                        )
         )
        done)
    )
)


(define <InfixMulInfixDiv> 
  (<SexprHelper> <InfixNeg> #\* #\/)  
)

(define <InfixAddInfixSub> 
  (<SexprHelper> <InfixMulInfixDiv> #\+ #\-)
)