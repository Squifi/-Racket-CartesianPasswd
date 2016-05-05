#lang racket
(require file/md5)

(define (a->z)
  (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(define (0->9)
  (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define (an)
  (append (a->z) (0->9)))

(define (single l1 l2)
  (cond
    ((empty? l1) '())
    (else (cons (string-append (car l2) (car l1)) (single (cdr l1) l2)))))

(define (singleTail l1 l2 acc)
  (cond
    ((empty? l2) acc)
    ((empty? l1) (singleTail (an) (cdr l2) acc))
    (else (singleTail (cdr l1) l2 (append  acc (list (string-append (car l2) (car l1))))))))

(define (3pw l1 l2 l3 acc)
  (cond
    ((empty? l3) acc)
    ((empty? l2) (3pw (an) (an) (cdr l3) acc))
    ((empty? l1) (3pw (an) (cdr l2) l3 acc))
    (else (3pw (cdr l1) l2 l3 (append acc (list (string-append (car l3) (car l2) (car l1))))))))

; Takes the Password List, adds a salt and converts it to md5 Hashes
(define (addSaltMd5 pwList salt)
  (map (lambda (pw)
         (md5 (string-append pw salt)))
         pwList))
