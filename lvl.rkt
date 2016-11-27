#lang racket
;--------------------------------------------
;Programmeerproject 1 academiejaar 2014-2015
;Frogger
;Asma oualmakran (asoualma@vub.ac.be)
;-------------------------------------------

(provide level-vector)
(provide lvl)


(define (object-put y x vec tag)
 (vector-set! (vector-ref vec y) x tag))

(define (make-tag-vector length tag)
  (make-vector length tag))


(define (generate-nested-vec lst vector sub-length)
  (define (iter idx ctr res leng)
    (cond
      ((null? ctr)res)
      ((> idx (- (vector-length res)1))res)
      (else
       (vector-set! res idx (make-tag-vector leng (car ctr)))
       (iter (+ idx 1) (cdr ctr) res leng))))
  (iter 0 lst vector sub-length))


(define (objects-placer lst vec)
  (let
      ((tag car)
       (lane cadr)
       (section caddr))

    (if (null? lst)
        vec
    (begin
      (object-put (lane (car lst))
                  (section (car lst))
                  vec
                  (tag (car lst)))
      (objects-placer (cdr lst) vec)))))


(define (generate-level lst1 lst2 vec sub-length)
  (generate-nested-vec lst1 vec sub-length)
  (objects-placer lst2 vec))

(define lvl (make-vector 6))
(define lvl-lst (list 'finish 'road 'grass 'road 'road 'grass))
(define obj (list (list'coin 1 3)(list 'purple 4 3) (list 'bush 2 1) (list 'bush 2 5)(list 'power-up 3 6)))
(generate-level lvl-lst obj lvl 8)


(define lvl2 (make-vector 6))
(define lvl-lst2 (list 'finish 'road 'road 'grass 'road 'grass))
(define obj2 (list (list 'bush 3 1)(list 'bush 3 3) (list 'bush 5 3) (list 'coin 4 3)))
(generate-level lvl-lst obj2 lvl2 8)

(define level-vector (make-vector 5))

(define (place-level vec idx lst)
  (if (null? lst)
      vec
      (begin
        (vector-set! vec idx (car lst))
        (place-level vec (+ idx 1)(cdr lst)))))

(place-level level-vector 0 (list lvl lvl2) )

