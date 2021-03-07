
#lang racket

(provide (all-defined-out))

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: empty list")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

(define (stream-for-n-steps s n)
  (if (= n 0) null
      (cons (car (s))
            (stream-for-n-steps (cdr (s)) (- n 1)))))

(define Y
  (lambda (f)
    (f (lambda (x) ((Y f) x)))))

(define (inc n)
  (+ n 1))

(define (zero? n)
  (= 0 n))

(define funny-number-stream
  (lambda ()
    ((Y (lambda (f)
          (lambda (n)
            (if (zero? (remainder n 5))
                (cons (- n) (lambda () (f (+ 1 n))))
                (cons n (lambda () (f (+ 1 n)))))))) 1)))

(define dan-then-dog
  (lambda ()
    ((Y (lambda (f)
          (lambda (x)
            (if (string=? x "dog.jpg")
                (cons "dan.jpg" (lambda () (f "dan.jpg")))
                (cons "dog.jpg" (lambda () (f "dog.jpg"))))))) "dog.jpg")))

(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define cycle-lists
  (lambda (xs ys)
    (lambda ()
      ((Y (lambda (f)
            (lambda (n)
              (let ([x (list-nth-mod xs n)]
                    [y (list-nth-mod ys n)])
                (cons (cons x y) (lambda () (f (inc n)))))))) 0))))

(define vector-assoc
  (lambda (v vec)
    ((Y (lambda (f)
          (lambda (n)
            (if (> n (- (vector-length vec) 1))
                #f
                (if (pair? (vector-ref vec n))
                    (if (equal? (car (vector-ref vec n)) v)
                        (vector-ref vec n)
                        (f (+ n 1)))
                    (f (+ n 1))))))) 0)))

(define cached-assoc
  (lambda (xs n)
    (Y (lambda (f)
         (letrec ([memo (make-vector n #f)]
                  [pos 0])
           (lambda (v)
             (or (vector-assoc v memo)
                 (let ([new-ans (assoc v xs)])
                   (and new-ans
                        (begin
                          (vector-set! memo pos new-ans)
                          (set! pos (remainder (+ pos 1) n))
                          new-ans))))))))))

;; TODO: Yeah, writing macro in Racket is not fluent like in the Clojure. I miss Clojure.
;; (define-syntax while-less)