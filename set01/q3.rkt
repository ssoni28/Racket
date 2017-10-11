;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; To count the number of pixels in a given image.
;; q3.rkt

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(provide image-area)
;; image-area : Image -> Number
;; PURPOSE :
;; GIVEN : an image i
;; RETURNS : number of pixels in the given image i
;;
;; EXAMPLES :
;; (image-area (circle 30 "solid" "blue")) = 3600
;; (image-area (rectangle 100 50 "solid" "black")) = 5000
;;
;; DESIGN STRATEGY : combine simpler functions
(define (image-area i)
  (* (image-width i) (image-height i)))
;; UNIT-TEST :
(begin-for-test
  (check-equal?
   (image-area (circle 30 "solid" "blue"))
   3600
   "The number of pixels of the given circle of radius 30 should be 3600")
  (check-equal?
   (image-area (rectangle 100 50 "solid" "black"))
   5000
   "The number of pixels of given rectangle of dimensions 100x50 should be 5000")
  (check-equal?
   (image-area (circle 0 "solid" "blue"))
   0
   "The number of pixels of the given circle of radius 0 should be 0")
 (check-not-equal?
   (image-area (circle 10 "solid" "blue"))
   100
   "The number of pixels of the given circle of radius 10 should not be 100")
  )