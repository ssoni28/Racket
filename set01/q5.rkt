;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; To delete the ith position from given string str.
;; q5.rkt

(require rackunit)
(require "extras.rkt")

(provide string-delete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS :
(define ZERO-LENGTH 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-delete : String Number -> String
;; PURPOSE :
;; GIVEN : a string named as str.
;;         a number i as the position to
;;         be deleted from string str. i
;;         is between 0 and length of the
;;         given string str(exclusive).
;; RETURNS : a new string of length less
;;           than str and having ith
;;           position deleted from str.
;;
;; EXAMPLES :
;; (string-delete "game" 1) -> "gme"
;; (string-delete "hello" 4) -> "hell"
;; (string-delete "" 1) -> "Given string is empty"
;; (string-delete "a" 0) -> ""
;;
;; DESIGN STRATEGY : Divide into cases on length of string
(define (string-delete str i)
  (cond
    [(= (string-length str) ZERO-LENGTH)
     (string-append str "Given string is empty")]
    [(> (string-length str) ZERO-LENGTH)
     (string-append (substring str ZERO-LENGTH i)
                    (substring str (+ i 1)))]))
;; UNIT-TEST :
(begin-for-test
  (check-equal?
   (string-delete "game" 1)
   "gme"
   "The result after deleting first position from game should be gme")
  (check-equal?
   (string-delete "hello" 4)
   "hell"
   "the result after deleting fourth position from hello should be hell")
  (check-equal?
   (string-delete "" 1)
   "Given string is empty"
   "The result after trying to delete first position from an empty string
    is a message Given string is empty")
  (check-equal?
   (string-delete "a" 0)
   ""
   "The result after deleting oth position from a 1String should be an
    empty string"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE :
;; In case of an empty string input, the user gets the message that Given
;; string is empty.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
