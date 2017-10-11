;; football.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(require "toy-interfaces.rkt")

(provide Football%)
(provide make-football)

;;============================================================================;;
;;                                 CLASS                                      ;;
;;============================================================================;;

;; FOOTBALL Class
;; -------------------

;; Football appears on f KeyEvent and keeps on shrinking in size until size 0.
;; This is selectable and draggable.
;; Football does not shrink when selected.
;; A Football is a (new Football% [x PosInt] [y PosInt] [scaling-ratio PosReal]
;;                                [selected? Boolean] [saved-mx PosInt]
;;                                [saved-my PosInt])
               
               
(define Football%
  (class* object%(Toy<%>)
    
    ;; x and y represents the coordinates of the center of the football toy
    (init-field x y) ;; PosInt
    
    ;; scaling-ratio represents the scaling factor by which the football
    ;; image is to be shrinked
    (init-field [scaling-ratio 1.0]) ;; PosReal
    
    ;; discount-factor repesents the constant value by which the scaling-factor
    ;; will be multiplied to at every tick
    (define discount-factor 0.95) ;; PosReal
    
    ;; Football image and the boundary of the football
    (define FOOTBALL (scale scaling-ratio (bitmap "football.png")))
    (define IMAGE-WIDTH (image-width FOOTBALL))
    (define IMAGE-HEIGHT (image-height FOOTBALL))
    
    ;; is the football selected? Default is false.
    (init-field [selected? false]) ;; Boolean
    
    ;; saved-mx and saved-my represents the position of
    ;; the last button-down event inside the football relative to the square's
    ;; center.(if the football is selected).  Else any value.
    (init-field [saved-mx SEL-INIT-X] [saved-my SEL-INIT-Y]) ;; PosInt
    
    (super-new)
    
    ;; toy-x -> PosInt
    ;; toy-y -> PosInt
    ;; RETURNS: the x or y position of the center of the football toy
    ;; EXAMPLE:
    ;; (send f1 toy-x) ->50
    ;; (send f1 toy-y) ->50
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    ;; toy-data -> PosInt
    ;; RETURNS: the size of the football which is the product of the width
    ;; and the height of the image
    ;; EXAMPLE:(send f1 toy-data) -> (* 64 64)
    (define/public (toy-data) size)
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    ;; add a new field,the football's size initialized
    ;; where size is the product of the width and height of the football
    (field [size (* IMAGE-WIDTH IMAGE-HEIGHT)])
    
    ;; after-tick : -> Void
    ;; GIVEN: no argument
    ;; EFFECT: updates the current state of the football to the state that
    ;; should be after a tick
    ;; updates the value of the scaling-ratio field of the football
    ;; EXAMPLE: Call after-tick method by the football object and verify the
    ;; comparable fields
    ;; (send BALL1 after-tick)->(check-equal? (send BALL1 toy-data) (* 64 64))
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (set! scaling-ratio (* discount-factor scaling-ratio))))
    
    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the current state of the football to the state that
    ;; should follow the given key event. Doesnt change any fields
    ;; EXAMPLE: Call the after-key-event method using the football object
    ;; and verify the comparable fields of the football
    ;; (send BALL1 after-key-event "w")
    ;; ->(check-equal? (send BALL1 toy-data) (* 64 64))
    (define/public (after-key-event kev) 50)
    
    ;; after-button-down : Int Int -> Void
    ;; GIVEN: the location of the mouse on button-down event
    ;; EFFECT: updates the current state of the football to the state that
    ;; should be after button down event.
    ;; updates the mouse position of the football and marks the football
    ;; selected
    ;; EXAMPLE: Call after-button-down method by the fottball object and then
    ;; verify the comparable fields of the ball
    ;; (send f1 after-button-down 85 55)->(check-equal? (send f1 toy-x) 50)
    ;; STRATEGY: Cases on whether the new mouse position is inside the football
    (define/public (after-button-down mx my)
      (if (in-football? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          50))
    
    ;; after-button-up : Int Int -> Void
    ;; GIVEN: the location of a mouse during the button-up event
    ;; EFFECT: updates the current state of the football to the state that
    ;; should be after button up event
    ;; EXAMPLE: Call after-button-up method by the football object and then
    ;; verify the comparable fields
    ;; (send BALL1 after-button-up 55 55)
    ;; ->(check-equal? (send BALL1 for-test:selected?) false)
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    ;; after-drag : Int Int -> Void
    ;; GIVEN : x and y coordinates of mouse of the drag event
    ;; EFFECT: updates the current state of the football to the state that
    ;; should be after mouse drag event
    ;; EXAMPLE: Call after-drag method by the football object and then
    ;; verify the comparable fields
    ;; (send f2 after-drag 55 55)->(check-equal? (send f2 toy-x) 50)
    ;; STRATEGY: Cases on whether the football is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
       (if selected?
           (begin
             (set! x (- mx saved-mx))
             (set! y (- my saved-my))
             (set! selected? true))
           50))
    
    ;; in-football? : Int Int -> Boolean
    ;; RETURNS: true iff the given new coordinates is inside the boundary of
    ;; the given football.
    (define (in-football? other-x other-y)
      (local [(define HALF-WIDTH (/ IMAGE-WIDTH 2))
              (define HALF-HEIGHT (/ IMAGE-HEIGHT 2))]
        (and
         (<= (abs (- other-x x)) HALF-WIDTH)
         (<= (abs (- other-y y)) HALF-HEIGHT))))
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with football
    ;; painted on it.
    ;; EXAMPLE:
    ;; (send f2 add-to-scene EMPTY-CANVAS)->
    ;; (place-image (scale 1.0 (bitmap "football.png")) 50 50 EMPTY-CANVAS)
    (define/public (add-to-scene canvas)
      (place-image
       (scale scaling-ratio FOOTBALL)
       x y
       canvas))
    
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)))

;;============================================================================;;
;;                               make-football                                ;;
;;============================================================================;;
;; make-football : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a football at the given position.
;; EXAMPLE:(make-football 50 50)
(define (make-football x y)
  (new Football% [x x][y y]))

;;============================================================================;;
;;                                   TESTS                                    ;;
;;============================================================================;;

;; Examples for testing
;; ---------------------
(define BALL1 (new Football% [x 50] [y 50] [selected? true]))


;; Test cases for football
;; ------------------------
(begin-for-test 
  (local
    ((define f1 (make-football 50 50))
     (define f2 (make-football 50 50)))
    (check-equal? (send f1 toy-x) 50)
    (check-equal? (send f1 toy-y) 50)
    (check-equal? (send f1 toy-data) (* 64 64))
    (send f1 after-tick)
    (check-equal? (send f1 toy-x) 50)
    (send BALL1 after-tick)
    (check-equal? (send BALL1 toy-data) (* 64 64))
    (send BALL1 after-key-event "w")
    (check-equal? (send BALL1 toy-data) (* 64 64))
    (send f1 after-button-down 55 55)
    (check-equal? (send f1 for-test:selected?) true)
    (send f1 after-button-down 85 55)
    (check-equal? (send f1 toy-x) 50)
    (send BALL1 after-button-up 55 55)
    (check-equal? (send BALL1 for-test:selected?) false)
    (send f2 after-drag 55 55)
    (check-equal? (send f2 toy-x) 50)
    (send f1 after-drag 55 55)
    (check-equal? (send f1 toy-x) 50)
    (check-equal? (send f2 add-to-scene EMPTY-CANVAS)
                  (place-image (scale 1.0 (bitmap "football.png"))
                               50 50 EMPTY-CANVAS))))