;; square-toy.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(require "toy-interfaces.rkt")

(provide Square-toy%)
(provide make-square-toy)

;; SQUARE-TOY CLASS
;; -----------------

;; A Square-toy is a 40x40 square which moves rightward at a constant rate and 
;; executes a perfect bounce when it reaches the edge of the canvas.
;; Square is selectable and draggable. It does not move while being selected.
;; A Square-toy is a (new Square-toy% [x PosInt] [y PosInt] [vel Int]
;;                                    [selected? Boolean] [saved-mx Int]
;;                                    [saved-my Int])
               
(define Square-toy%
  (class* object%(Toy<%>)
    
    ;; x and y represents the coordinates of the center of the square toy
    ;; vel represents the speed at which the square moves horizontally
    (init-field x y) ;;  PosInt
    (init-field vel) ;;  Int
    
    ;; is the square selected? Default is false.
    (init-field [selected? false]) ;; Boolean
    
    ;; saved-mx and saved-my represents the position of
    ;; the last button-down event inside the square relative to the square's
    ;; center.(if the square is selected).  Else any value.
    (init-field [saved-mx SEL-INIT-X] [saved-my SEL-INIT-Y]) ;; Int
    
    (super-new)
    
    ;; toy-x -> PosInt
    ;; toy-y -> PosInt
    ;; RETURNS: the x or y position of the center of the square toy
    ;; EXAMPLE:
    ;; (send s1 toy-x)->50
    ;; (send s1 toy-y)->50
    (define/public (toy-x) x) 
    (define/public (toy-y) y)
    
    ;; toy-data -> Int
    ;; RETURNS: the speed at which the square moves(rightward is positive)
    ;; EXAMPLE: (send s1 toy-data)->1
    (define/public (toy-data) vel) 
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    ;; add a new field,the square's side length initialized to 40
    (field [side SIDE]) ;; PosInt
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates the current state of the to the state that should be
    ;; after a tick
    ;; updates the x coordinate of the center of the square toy and the speed
    ;; at which it moves
    ;; WHERE: a selected square doesn't move.
    ;; EXAMPLE:Call the after-tick method by the square-toy object s1 and verify
    ;; the comaprable fields of the square
    ;; (send s1 after-tick)->(check-equal? (send s1 toy-x) 51)
    ;; STRATEGY: Cases on whether the square is selected
    (define/public (after-tick)
      (if selected?
          this
          (begin
            (set! x (set-x))
            (set! vel (set-vel)))))
    
    ;; set-x: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates the x-coordinate of the center of the square after the
    ;; next tick
    ;; RETURNS: the x coordinate of the center of the square toy
    (define (set-x)
      (cond
        [(>= (+ x vel) RIGHT-BOUNDARY) RIGHT-BOUNDARY]
        [(<= (+ x vel) LEFT-BOUNDARY) LEFT-BOUNDARY]
        [else (+ x vel)]))
    
    ;; set-vel: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates the speed with which the square moves in x-direction
    ;; after the next tick
    ;; RETURNS: the speed value with which the square toy moves on the canvas
    (define (set-vel)
      (if (or (>= (+ x vel) RIGHT-BOUNDARY)
              (<= (+ x vel) LEFT-BOUNDARY))
          (- 0 vel)
          vel))
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the current state of the square-toy to the state that
    ;; shoud be after the given key event.Does not update any field
    ;; EXAMPLE:
    ;; (send s1 after-key-event "w")
    ;; ->(check-equal? (send s1 toy-data) 1)
    (define/public (after-key-event kev) this)
    
    ;; after-button-down : Int Int -> Void
    ;; GIVEN: the location of mouse in button-down event
    ;; EFFECT: updates the current state of the square-toy to the state that
    ;; should be after button down event
    ;; updates the mouse position of the square toy and marks it selected
    ;; EXAMPLE: Call after-key-event method on square object and verify the
    ;; comparable data
    ;; (send s1 after-key-event "w")->(check-equal? (send s1 toy-data) 1)
    ;; STRATEGY: Cases on whether the new mouse position is inside the square
    ;; toy
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          50))
    
    ;; after-button-up : Int Int -> Void
    ;; GIVEN: the location of a mouse for the button up event
    ;; EFFECT: updates the current state of the square-toy to the state that
    ;; should be after button up event
    ;; EXAMPLE:Call the after-button-up method from the square toy object and
    ;; verify the comparable data
    ;; (send SQUARE1 after-button-up 55 55)
    ;; ->(check-equal? (send SQUARE1 for-test:selected?) false)
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    ;; after-drag : Int Int -> Void
    ;; GIVEN : x and y coordinates of mouse of the drag event
    ;; EFFECT: updates the current state of the square-toy to the state that
    ;; should be after button drag event
    ;; EXAMPLE:Call the after-drag method on SQUARE1 object and verify the
    ;; comparable data.
    ;; (send SQUARE1 after-drag 55 55)
    ;; (check-equal? (send SQUARE1 toy-x) 55)
    ;; STRATEGY: Cases on whether the square toy is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
       (if selected?
           (begin
             (set! x (- mx saved-mx))
             (set! y (- my saved-my))
             (set! selected? true))
           50))
    
    ;; in-square? : Int Int -> Boolean
    ;; GIVEN: the x and y coordinates of the mouse position
    ;; RETURNS: true iff the given new coordinates is inside the bounding box of
    ;; the given canvas.
    (define (in-square? other-x other-y)
      (and (<= (- x LEFT-BOUNDARY) other-x (+ x LEFT-BOUNDARY))
           (<= (- y TOP-BOUNDARY) other-y (+ y TOP-BOUNDARY))))
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with the square toy
    ;; painted on it.
    ;; EXAMPLE:(send s1 add-to-scene EMPTY-CANVAS)->
    ;;  (place-image SQR1 50 50 EMPTY-CANVAS)
    (define/public (add-to-scene canvas)
      (local ((define SQUARE_IMAGE (square side "outline" "black")))
      (place-image SQUARE_IMAGE x y canvas)))
    
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)
    ))

;;============================================================================;;
;;                               make-square-toy                              ;;
;;============================================================================;;
;; make-square-toy : PosInt PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;; travelling right at the given speed.
;; EXAMPLE:(make-square-toy 50 50 1)
(define (make-square-toy x y vel)
  (new Square-toy% [x x][y y] [vel vel]))

;;============================================================================;;
;;                                   TESTS                                    ;;
;;============================================================================;;

;; Examples for testing
;; ---------------------
(define SQUARE1 (new Square-toy% [x 50] [y 50] [vel 1] [selected? true]
                     [saved-mx SEL-INIT-X] [saved-my SEL-INIT-Y]))
(define SQUARE3 (new Square-toy% [x 55] [y 55] [vel 1] [selected? true]
                     [saved-mx SEL-INIT-X] [saved-my SEL-INIT-Y]))

(define SQR1 (square 40 "outline" "black"))

;; Test cases for Square-toy
;; --------------------------
(begin-for-test 
  (local
    ((define s1 (make-square-toy 50 50 1))
     (define s2 (make-square-toy 499 50 1))
     (define s3 (make-square-toy 0 50 0)))
    (check-equal? (send s1 toy-x) 50)
    (check-equal? (send s1 toy-y) 50)
    (check-equal? (send s1 toy-data) 1)
    (send s1 after-tick)
    (check-equal? (send s1 toy-x) 51)
    (send SQUARE3 after-tick)
    (check-equal? (send SQUARE3 toy-data) 1)
    (send s2 after-tick)
    (check-equal? (send s2 toy-x) 480)
    (send s3 after-tick)
    (check-equal? (send s3 toy-x) 20)
    (send s1 after-key-event "w")
    (check-equal? (send s1 toy-data) 1)
    (send s1 after-button-down 55 55)
    (check-equal? (send s1 for-test:selected?) true)
    (send s2 after-button-down 85 55)
    (check-equal? (send s2 for-test:selected?) false)
    (send SQUARE1 after-drag 55 55)
    (check-equal? (send SQUARE1 toy-x) 55)
    (send SQUARE1 after-button-up 55 55)
    (check-equal? (send SQUARE1 for-test:selected?) false)
    (send s3 after-drag 55 55)
    (check-equal?  (send s3 toy-x) 20)
    (check-equal?
     (send s1 add-to-scene EMPTY-CANVAS)
     (place-image SQR1 51 50 EMPTY-CANVAS))))