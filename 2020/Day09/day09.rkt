#lang racket

#|
  Aoc 2020, Day 9: Encoding Error
  Author: Chi-Kit Pao

  Commands:
  racket day09.rkt

  Outputs:
  Question 1: What is the first number that does not have this property?
  Answer: 18272118
  Question 2: What is the encryption weakness in your XMAS-encrypted list of numbers?
  Answer: 2186361

  Time usage shown via command "time".
  real	0m1,288s
  user	0m1,235s
  sys	0m0,053s
|#


(define (read-numbers filename)
  (map string->number
       (file->lines filename)))


;; Part 1
(define (make-bool-vector n)
  (vector-append (make-vector 25 #t) (make-vector (- n 25) #f)))


(define number-list (read-numbers "input.txt"))
(define bool-vector (make-bool-vector (length number-list)))

(define (fill-bool-vector nl bv)
  (for ([i (- (length nl) 2)])
    (for ([j (in-range (+ i 1) (min (- (length nl) 1) (+ i 25)))]
          #:unless (or (= (list-ref nl i) (list-ref nl j))))
      (for ([k (in-range (+ j 1) (min (length nl) (+ i 26)))]
          #:unless (< k 25))
        (when (= (+ (list-ref nl i) (list-ref nl j)) (list-ref nl k))
          (vector-set! bv k #t))
      )
    )
  )
)
(fill-bool-vector number-list bool-vector)
(define answer1
  (for/first ([i (in-range 25 (vector-length bool-vector))]
    #:when (not (vector-ref bool-vector i)))
    (list-ref number-list i)))

(printf "Question 1: What is the first number that does not have this property?\n")
(printf (format "Answer: ~a\n" answer1))


;; Part 2
(define (next-values j cl [nl number-list])
  (if (>= (+ j 1) (length nl))
    cl
    (cons (+ (car cl) (list-ref nl j)) (cons (list-ref nl j) (cdr cl)))))

(define (find-contiguous-set n i j cl [nl number-list])
  (cond [(>= i (length nl)) '()]
      [(and (>= (+ i 1) (length nl)) (>= j (length nl))) '()]
      [(>= j (length nl)) (find-contiguous-set n (+ i 1) (+ i 2) (list (list-ref nl (+ i 1)) (list-ref nl (+ i 1))))]
      [(> (+ (list-ref cl 0) (list-ref nl j)) n) (find-contiguous-set n (+ i 1) (+ i 2) (list (list-ref nl (+ i 1)) (list-ref nl (+ i 1))))]
      [(< (+ (list-ref cl 0) (list-ref nl j)) n) (find-contiguous-set n i (+ j 1) (next-values j cl nl))]
      [else (cdr (next-values j cl nl))]))
(define (answer2 l)
  (+ (apply min l) (apply max l)))

(printf "Question 2: What is the encryption weakness in your XMAS-encrypted list of numbers?\n")
(printf (format "Answer: ~a\n" (answer2 (find-contiguous-set answer1 0 1 (list (list-ref number-list 0) (list-ref number-list 0))))))
