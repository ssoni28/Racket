;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A program which extracts the first 1String from a non-empty string.
;; q2.rkt

(require rackunit)
(require "extras.rkt")

(provide string-first)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS:
(define FIRST-INDEX 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-first : String -> String
;; PURPOSE :
;; GIVEN : a non empty string as inputString
;;         position of 1String as required
;; RETURNS : first 1String from a non empty string
;;
;; EXAMPLES :
;; (string-first "hello") -> "h"
;; (string-first "vishu") -> "v"
;;
;; DESIGN STRATEGY : combine simpler functions
(define (string-first inputString)
  (string-ith inputString FIRST-INDEX))
;; UNIT TEST :
(begin-for-test
  (check-equal?
   (string-first "\n")
   "\n"
   "The first 1String for a string new line is a new line")
  (check-equal?
   (string-first "hello")
   "h"
   "The first 1String for a string hello is h")
  (check-equal?
   (string-first "vishu")
   "v"
   "The first 1String for a string vishu is v")
  (check-equal?
   (string-first "   ")
   " "
   "The first 1String for a string 3 single spaces is a single space")
  )
                