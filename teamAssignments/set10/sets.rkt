;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; sets

(require rackunit)
(require "extras.rkt")

(provide
 set-member?
 subset?
 set-equal?
 set-cons
 set-union
 set-minus
 set-diff)

;; A SetOfX is a ListOfX with no duplicates

;; note: empty is a SetOfX

;; set-member? : X SetOfX -> Boolean
;; GIVEN: an X and a set of X's
;; RETURNS: true iff the X is an element of the set
;; STRATEGY: Use HOF ormap on set1
(define (set-member? x set1)
  (ormap
   (lambda (elt) (equal? x elt))
   set1))

;; subset? : SetOfX SetOfX -> Boolean
;; STRATEGY: Use HOF andmap on set1
(define (subset? set1 set2)
  (andmap
   (lambda (elt) (set-member? elt set2))
   set1))

;; set-equal? : SetOfX SetOfX -> Boolean
;; STRATEGY: Call simpler functions
(define (set-equal? set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))

;; set-cons : X SetOfX -> SetOfX
;; STRATEGY: Cases on whether x is a member of set1
(define (set-cons x set1)
  (if (set-member? x set1)
      set1
      (cons x set1)))

;; set-union : SetOfX SetOfX -> SetOfX
;; STRATEGY: Use HOF foldr on set1
(define (set-union set1 set2)
  (foldr
   set-cons
   set2
   set1))

;; set-minus : SetOf<X> X -> SetOf<X>
(define (set-minus set1 x)
 (filter
   (lambda (elt) (not (equal? x elt)))
   set1))

;; set-diff : SetOf<X> SetOf<X> -> SetOf<X>
;; return all elements of set1 that are NOT elements of set2
(define (set-diff set1 set2)
 (filter
   (lambda (elt) (not (set-member? elt set2)))
   set1))