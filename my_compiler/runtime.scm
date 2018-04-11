(define map
	(lambda (proc items)
		(if (null? items)
			items
			(cons (proc (car items)) (map proc (cdr items))))))

(define list (lambda x x))

		
(define two-list-append
	(lambda (l m)
		(if (null? l)
			m
			(cons (car l) (two-list-append (cdr l) m)))))
 
(define append-rec
	(lambda (x y)
		(if (null? y)
			x
			(two-list-append x (append-rec (car y) (cdr y))))))

(define append
	(lambda lst
	(let ((lstCdr (cdr lst)))
		(if (null? lst)
			'()
			(if (null? lstCdr)
				(car lst)
				(if (= 2 (cdr lstCdr))
					(two-list-append (car lst) (car lstCdr))
					(append-rec (car lst) lstCdr)))))))

					
(define reverse
  (lambda (l)
     (cond ((null? l) '())
           ((atom? (car l))
             (swap-till-end (car l) (reverse (cdr l))))
           (else
             (swap-till-end (reverse (car l)) (reverse (cdr l)))))))


(define swap-till-end
   (lambda (elm lst)
      (cond ((null? lst) (cons elm '()))
            (else
               (cons (car lst) (swap-till-end elm (cdr lst)))))))

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))
	

(define apply
  (lambda (f s)
     (apply-helper f (reverse s))))