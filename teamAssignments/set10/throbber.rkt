;; throbber.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(require "toy-interfaces.rkt")

(provide Throbber%)
(provide make-throbber)

;; Throbber Class
;; ----------------------------

;; A Throbber is a circle of initial radius 5 having green color.
;; Throbber expands gradually till radius 20 and shrinks gradually
;; till radius 20.
;; A throbber is selectable and draggable.
;; A throbber is a (new Throbber% [x PosInt] [y PosInt] [radius PosInt]
;;                                [is-increasing? Boolean] [saved-mx Int]
;;                                [saved-my Int] [selected? Boolean])
              
(define Throbber%
  (class* object%(Toy<%>)
    ;; x and y represents the coordinates of the center of the throbber
    (init-field x y) ;; PosInt
    
    ;; radius of throbber, defaults to 5
    (init-field [radius 5]) ;; PosInt
    
    ;; is-increasing? returns flag, true iff the throbber's radius is increasing
    (init-field [is-increasing? true]) ;; Boolean
    
    ;; saved-mx and saved-my represents the position of
    ;; the last button-down event inside the throbber relative to the
    ;; throbber's center.(if the throbber is selected).  Else any value.
    (init-field [saved-mx SEL-INIT-X]) ;; Int
    (init-field [saved-my SEL-INIT-Y]) ;; Int
    
    ;; true iff the throbber is selected.By default throbber is unselected
    (init-field [selected? false]) ;; Boolean
    
    (super-new)
    
    ;; Constants for the radius
    (define MAX_RADIUS 20)
    (define MIN_RADIUS 5)
    (define INC_RADIUS 1)
    
    ;; toy-x -> PosInt
    ;; toy-y -> PosInt
    ;; RETURNS: the x and y coordinates of the center of the throbber toy
    ;; EXAMPLES:(send t1 toy-x)-> 50
    ;; (send t1 toy-y)->50
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    ;; toy-data -> PosInt
    ;; RETURNS: The radius of the throbber
    ;; EXAMPLE: (send t1 toy-data) ->5
    (define/public (toy-data) radius)
    
    ;; after-tick:  -> Void
    ;; GIVEN: No arguments
    ;; EFFECT: Updates the current state of the throbber to the state that
    ;; should follow after a tick.
    ;; EXAMPLE: Check the comparable data after invoking a method by the
    ;; throbber object(as given below)
    ;; It changes the is-increasing? and radiues fields of the throbber
    ;; (send THROBBER1 after-tick)
    ;; -> (check-equal? (send THROBBER1 toy-x) 50)
    ;; (send THROBBER2 after-tick)
    ;; -> (check-equal? (send THROBBER2 toy-data) 19)
    ;; (send THROBBER3 after-tick)
    ;; ->(check-equal? (send THROBBER3 toy-data) 5)
    ;; STRATEGY: Cases on is-increasing? flag
    (define/public (after-tick)
      (if is-increasing?
          (increase-radius)
          (decrease-radius)))
    
    ;; increase-radius:  -> Void
    ;; GIVEN: no argument
    ;; EFFECT: Updates the current state of the throbber with radius increased
    ;; by 1 or reversing the is-increasing? flag depending upon current radius.
    ;; STRATEGY: Cases on the current value of the radius of the throbber
    (define (increase-radius)
      (if (< radius MAX_RADIUS)
          (begin
            (set! radius (+ radius INC_RADIUS))
            (set! is-increasing? true))
          (begin
            (set! radius (- radius INC_RADIUS))
            (set! is-increasing? false))))
    
    ;; decrease-radius: -> Void
    ;; GIVEN: No argument
    ;; EFFECT: Updates the current state of the throbber with radius decreased
    ;; by 1 or reversing the is-increasing? flag depending upon current radius.
    ;; STRATEGY: Cases on the current value of the radius of the throbber
    (define (decrease-radius)
      (if (> radius MIN_RADIUS)
          (begin
            (set! radius (- radius INC_RADIUS))
            (set! is-increasing? false))
          (begin
            (set! radius (+ radius INC_RADIUS))
            (set! is-increasing? true))))
          
    
    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: a key event
    ;; RETURNS: updates the current state of the throbber to the state that
    ;; should be after the given key event.
    ;; EXAMPLE: Check the comparable data after invoking after-key-event method
    ;; by the throbber object(as given below)
    ;; It does not change any fields of the throbber
    ;; (send t1 after-key-event "w")->(check-equal? (send t1 toy-x) 50)
    (define/public (after-key-event kev) 50)
    
    ;; after-button-down: Int Int -> Void
    ;; GIVEN: the location of mouse in button-down event
    ;; EFFECT: updates the current state of the throbber to the state that
    ;; should be after button down event
    ;; updates the mouse positions fields of the throbber and marks it as
    ;; selected
    ;; EXAMPLE:Call the method after-button-down by a throbber object t1
    ;; and check the comparable value
    ;; (send t1 after-button-down 55 55)->(check-equal? (send t1 toy-x) 50)
    ;; STRATEGY:Cases on whether the new mouse position is in the throbber
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (begin
            (set! saved-mx mx)
            (set! saved-my my)
            (set! selected? true))
          50))
    
    ;; after-button-up: Int Int -> Void
    ;; GIVEN : x and y coordinates of mouse button up event
    ;; EFFECT: updates the current state of the throbber to the state that
    ;; should be after button up mouse event
    ;; updates the mouse position of the throbber to the default values
    ;; and marks the throbber unselected
    ;; EXAMPLE: Invoke after-button-up method by the t1 throbber object and
    ;; verify the comparable fields
    ;; (send t1 after-button-up 55 55)
    ;; ->(check-equal? (send t1 for-test:selected?) false)
    (define/public (after-button-up mx my)
      (begin
        (set! saved-mx SEL-INIT-X)
        (set! saved-my SEL-INIT-Y)
        (set! selected? false)))
    
    ;; after-drag: Int Int -> Void
    ;; GIVEN : x and y coordinates of mouse of the drag event
    ;; EFFECT: updates the current state of the throbber to the state that
    ;; should be after drag event
    ;; updates the center coordinates of the throbber to the new values
    ;; also updates the mouse position and the selected field
    ;; EXAMPLE: call after-drag method on the throbber object t1 and verify the
    ;; comparable fields
    ;; (send t1 after-drag 100 50)
    ;; ->(check-equal? (send t1 for-test:selected?) false)
    ;; STRATEGY: Cases on whether the throbber is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (+ x (- mx saved-mx)))
            (set! y (+ y (- my saved-my)))
            (set! saved-mx mx)
            (set! saved-my my)
            (set! selected? true))
          50))
    
    ;; in-throbber?: Int Int -> Boolean
    ;; GIVEN: x and y coordinates of mouse button down event
    ;; RETURNS: true iff the given new coordinates is inside the bounding box of
    ;; the given circle.
    (define (in-throbber? mx my)
      (<= (+ (sqr (- x mx)) (sqr (- y my)))
          (sqr radius)))
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with the throbber
    ;; painted on it.
    ;; EXAMPLE:(send t1 add-to-scene EMPTY-CANVAS)
    ;; ->  (place-image (circle 5 "solid" "green") 50 50 EMPTY-CANVAS))
    (define/public (add-to-scene canvas)
      (local ((define THROBBER (circle radius "solid" "green")))
      (place-image THROBBER x y canvas)))
    
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)
    ))

;;============================================================================;;
;;                             make-throbber                                  ;;
;;============================================================================;;
;; make-throbber: PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
;; EXAMPLE:(make-throbber 50 50)
(define (make-throbber x y)
  (new Throbber% [x x] [y y]))

;;============================================================================;;
;;                                   TESTS                                    ;;
;;============================================================================;;

;; Examples for testing
;; ----------------------
(define THROBBER1 (new Throbber%
                       [x 50]
                       [y 50]
                       [radius 20]
                       [is-increasing? true]
                       [saved-mx SEL-INIT-X]
                       [saved-my SEL-INIT-Y]))
(define THROBBER2 (new Throbber%
                       [x 50]
                       [y 50]
                       [radius 20]
                       [is-increasing? false]
                       [saved-mx SEL-INIT-X]
                       [saved-my SEL-INIT-Y]
                       [selected? false]))
(define THROBBER3 (new Throbber%
                       [x 50]
                       [y 50]
                       [radius 4]
                       [is-increasing? false]
                       [saved-mx SEL-INIT-X]
                       [saved-my SEL-INIT-Y]
                       [selected? false]))
(define THROBBER9 (new Throbber% [x 50] [y 50] [radius 5]
                       [is-increasing? true] [saved-mx 0]
                       [saved-my 0] [selected? true]))

 
;; Test cases for throbber toy
(begin-for-test
  (local
    ((define t1 (make-throbber 50 50))
     (define t2 (make-throbber 50 50)))
    (check-equal? (send t1 toy-x) 50)
    (check-equal? (send t1 toy-y) 50)
    (check-equal? (send t1 toy-data) 5)
    (send t1 after-tick)
    (check-equal? (send t1 toy-data) 6)
    (send THROBBER1 after-tick)
    (check-equal? (send THROBBER1 toy-x) 50)
    (send THROBBER2 after-tick)
    (check-equal? (send THROBBER2 toy-data) 19)
    (send THROBBER3 after-tick)
    (check-equal? (send THROBBER3 toy-data) 5)
    (send t1 after-button-down 51 51)
    (check-equal? (send t1 for-test:selected?) true)
    (send t1 after-button-down 55 55)
    (check-equal? (send t1 toy-x) 50)
    (send THROBBER9 after-drag 60 90)
    (check-equal? (send THROBBER9 toy-x) 110)
    (send t1 after-button-up 55 55)
    (check-equal? (send t1 for-test:selected?) false)
    (send t1 after-drag 100 50)
    (check-equal? (send t1 for-test:selected?) false)
    (send t1 after-key-event "w")
    (check-equal? (send t1 toy-x) 50)
    (check-equal? (send t2 add-to-scene EMPTY-CANVAS)
                  (place-image (circle 5 "solid" "green")
                               50 50 EMPTY-CANVAS))))
