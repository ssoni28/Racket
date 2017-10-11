;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; To insert "_" at the given ith position of a given string.
;; q4.rkt

(require rackunit)
(require "extras.rkt")

(provide string-insert)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS :
(define ZERO-LENGTH 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-insert : String Number -> String
;; PURPOSE :
;; GIVEN : a string as inputString.
;;         position i where "_" is to
;;         be inserted in the given
;;         inputString. i is a number
;;         between 0 and length of the
;;         given inputString(inclusive).
;; RETURNS : a string with a "_" inserted
;;           at the ith position of given
;;           inputString if it is a non
;;           empty string. Othewise returns
;;           a string with a "_" inserted
;;           at the first position of an
;;           empty string.
;;
;; EXAMPLES :
;; (string-insert "firstname" 5) -> "first_name"
;; (string-insert "nike" 0) -> "_nike"
;; (string-insert "" 0) -> "_"
;; (string-insert "" 5) -> "_"
;; (string-insert "miss" 4) -> "miss_"
;;
;; DESIGN STRATEGY : Divide into cases on length of string 
(define (string-insert inputString i)
  (cond
    [(= (string-length inputString) ZERO-LENGTH)
     (string-append inputString "_")]
    [(> (string-length inputString) ZERO-LENGTH)
     (string-append (substring inputString ZERO-LENGTH i)
                    "_"
                    (substring inputString i))]))
;; UNIT-TEST :
(begin-for-test
  (check-equal?
   (string-insert "firstname" 5)
   "first_name"
   "The output after inserting _ at 5th position in given string should be
    first_name")
  (check-equal?
   (string-insert "nike" 0)
   "_nike"
   "The output after inserting _ at 0th position in given string should be
    _nike")
  (check-equal?
   (string-insert "" 0)
   "_"
   "The output after inserting _ at zero position in the given empty string
    should be _")
  (check-equal?
   (string-insert "" 5)
   "_"
   "The output after inserting _ at 5th position in the given empty string
    should be _")
  (check-equal?
   (string-insert "miss" 4)
   "miss_"
   "The output after inserting _ at 4th position in given string should be
    miss_"))
