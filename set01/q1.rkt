;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; To determine the distance of a point(x,y) to the origin(0,0).
;; file saved as q1.rkt.

(require rackunit)
(require "extras.rkt")

(provide distance-to-origin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS:
(define ORIGIN-X 0)
(define ORIGIN-Y 0)
(define POWER 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; distance-to-origin : Real Real -> Real
;; PURPOSE:
;; GIVEN: point(x,y)
;;        x is a coordinate on the   x-axis.
;;        y is a coordinate on the   y-axis.
;;        origin(0,0) is the target to where
;;        we have to calculate the  distance
;;        of point(x,y).
;; RESULT: distance of point(x,y) to the origin(0,0).
;;
;; EXAMPLES:
;; (distance-to-origin 3 4) -> 5
;; (distance-to-origin 12 5) -> 13
;;
;; DESIGN STRATEGY : combine simpler functions
(define (distance-to-origin x y)
  (sqrt (+ (expt (- x ORIGIN-X) POWER) (expt (- y ORIGIN-Y) POWER))))
;; UNIT TEST:
(begin-for-test
  (check-equal?
   (distance-to-origin 3 4)
   5
   "distance of point(3,4) to the origin(0,0) should be 5")
  (check-equal?
   (distance-to-origin 12 5)
   13
   "distance of point(12,5) to the origin(0,0) should be 13")
  (check-equal?
   (distance-to-origin 0 0)
   0
   "distance of point(0,0) to the origin(0,0) should be 0")
  (check-equal?
   (distance-to-origin -5 -12)
   13
   "distance of point(-5,-12) to the origin(0,0) should be 13")
  (check-equal?
   (distance-to-origin 1.2 .5)
   1.3
   "distance of point(1.2,.5) to the origin(0,0) should be 1.3")
  (check-not-equal?
   (distance-to-origin 3 4)
   50
   "distance of point(3,4) to the origin(0,0) should not be 50"))