#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

;;============================================================================;;
;;                              CONSTANTS                                     ;;
;;============================================================================;;
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define TARGET-INIT-X (/ CANVAS-WIDTH 2))
(define TARGET-INIT-Y (/ CANVAS-HEIGHT 2))
(define SEL-INIT-X 0)
(define SEL-INIT-Y 0)
(define TARGET-RADIUS 10)
(define TARGET-CIRCLE (circle TARGET-RADIUS "outline" "black"))
(define SIDE 40)
(define RIGHT-BOUNDARY (- CANVAS-WIDTH (/ SIDE 2)))
(define LEFT-BOUNDARY (/ SIDE 2))
(define TOP-BOUNDARY (/ SIDE 2))

;;============================================================================;;
;;                             Provide Statements                             ;;
;;============================================================================;;
(provide
 CANVAS-WIDTH
 CANVAS-HEIGHT
 EMPTY-CANVAS
 TARGET-INIT-X
 TARGET-INIT-Y
 SEL-INIT-X
 SEL-INIT-Y
 TARGET-RADIUS
 TARGET-CIRCLE
 SIDE
 RIGHT-BOUNDARY
 LEFT-BOUNDARY
 TOP-BOUNDARY
 Toy<%>
 PlaygroundState<%>)

;;============================================================================;;
;;                             DATA DEFINITIONS                               ;;
;;============================================================================;;
;; A PlaygroundState is a
;; (make-playground PosInt)

;; INTERP:
;; (make-playground speed)
;; represents a playground state containing the
;; speed - square speed at which the square toy moves towards right initially

;; A PlayGroundState is an object whose class implements PlayGroundState<%>

;; A Clock is an object whose class implements Toy<%>

;; A Football is an object whose class implements Toy<%>

;; A Throbber is an object whose class implements Toy<%>

;; A Square-toy is an object whose class implements Toy<%>

;; Template for ListOfToy<%>
;; INTERPRETATION:
;; A ListOfToy<%> is one of:
;; -- empty
;; -- (cons Toy<%> ListOfToy<%>)

;; TEMPLATE:
;; lot-fn: ListOfToy<%> -> ??
;; (define (lot-fn lot)
;;   (cond
;;    [(empty? lot) ...]
;;    [else (... 
;;            (toy-fn(first lot))
;;            (lot-fn (rest lot)))]))

;;============================================================================;;
;;                              INTERFACES                                    ;;
;;============================================================================;;

;; PlaygroundState Interface
;; --------------------------

;; PlaygroundState implements the PlaygroundState<%> interface

(define PlaygroundState<%>
  (interface (SWidget<%>) ;; this means: include all the methods in
    ;; SWidget<%>. 
    
    ;; -> Int
    ;; -> Int
    ;; RETURNS: the x or y coordinates of the center of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; RETURNS: true iff the target is selected by the button down
    ;; mouse event
    target-selected?
    
    ;; -> ListOfToy<%>
    ;; RETURNS: a list of toys contained in the playground
    get-toys
    ))

;; Toy Interface
;; --------------

;; a toy implements the Toy<%> interface
;; a toy can be a throbber, a football, a clock or a square-toy.

(define Toy<%> 
  (interface (SWidget<%>);; this means: include all the methods in SWidget<%>
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS: some data related to the toy. The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a square, it is the velocity of the square (rightward is
    ;; positive)
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a football, it is the current size of the football (in
    ;; arbitrary units; bigger is more)
    toy-data
    ))
;;============================================================================;;
;;                             END OF FILE                                    ;;
;;============================================================================;;