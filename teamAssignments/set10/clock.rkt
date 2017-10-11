;; clock.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(require "toy-interfaces.rkt")

(provide Clock%)
(provide make-clock)

;;============================================================================;;
;;                                 CLASS                                      ;;
;;============================================================================;;

;; Clock Class
;; -------------

;; A Clock displays the number of ticks since it was created.
;; Otherwise the appearance of the clock is unspecified.
;; A Clock is a (new Clock% [x PosInt] [y PosInt] [val PosInt]
;;                          [selected? Boolean] [saved-my Int] [saved-my Int])

(define Clock%
  (class* object%(Toy<%>)
    ;; x and y represents the coordinates of the center of the clock toy
    ;; val represents the tick value at every tick, initialzed to 0
    (init-field x y [val 0]) ;; PosInt
    
    ;; is the clock selected? Default is false.
    (init-field [selected? false]) ;; Boolean
    
    ;; if the clock is selected, the position of
    ;; the last button-down event inside the clock, relative to the
    ;; clock's center.  Else any value.
    (init-field [saved-mx SEL-INIT-X] [saved-my SEL-INIT-Y]) ;; Int
    
    (super-new)
    
    ;; toy-x -> PosInt
    ;; toy-y -> PosInt
    ;; RETURNS: the x or y position of the center of the clock toy
    ;; EXAMPLE:
    ;; (send c1 toy-x)->40
    ;; (send c1 toy-y)->30
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    ;; toy-data -> PosInt
    ;; RETURNS:the value of a tick
    ;; EXAMPLE:(send c1 toy-data)->0
    (define/public (toy-data) val)
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    ;; add a new field,the clock's side length initialized to 50
    (field [side SIDE])
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates the current state of the clock to the state that
    ;; should be after a tick
    ;; updates the val field of the clock
    ;; WHERE: a selected clock doesn't move.
    ;; EXAMPLE: Call after-tick method by the clock object and then
    ;; verify the comparable fields
    ;; (send c1 after-tick)->(check-equal? (send c1 toy-data) 1)
    (define/public (after-tick)
      (set! val (+ val 1)))
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECt: updates the current state of the clock to the state that
    ;; should be after the given key event. Doesnt change any field
    ;; EXAMPLE: Call after-key-event method by the clock object and then
    ;; verify the comparable fields
    ;; (send c1 after-key-event "w")->(check-equal? (send c1 toy-data) 1)
    (define/public (after-key-event kev) 55)
    
    ;; after-button-down : Int Int -> Void
    ;; GIVEN: the location of the mouse in button-down event
    ;; EFFECT: updates the current state of the clock to the state that
    ;; should be after button down event
    ;; EXAMPLE: Call after-button-down method by the clock object and then
    ;; verify the comparable fields
    ;; (send c1 after-button-down 80 70)
    ;; -> (check-equal? (send c1 for-test:selected?) false)
    ;; STRATEGY: Cases on whether the coordinates of the mouse is in the clock 
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          55))
    
    ;; after-button-up : Int Int -> Void
    ;; GIVEN: the location of a mouse during button-up event
    ;; EFFECT: updates the current state of the clock to the state that
    ;; should be after button up event
    ;; Marks the clock as unselected
    ;; EXAMPLE: Call after-button-up method by the clock object and then
    ;; verify the comparable fields.
    ;; (send CLOCK2 after-button-up 45 35)
    ;; -> (check-equal? (send CLOCK2 for-test:selected?) false)
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    ;; after-drag : Int Int -> Void
    ;; GIVEN: the location of mouse during drag event
    ;; EFFECT: updates the current state of the clock to the state that
    ;; should be after drag event
    ;; EXAMPLE: Call after-drag method by the clock object and then
    ;; verify the comparable fields
    ;; (send CLOCK2 after-drag 45 35)
    ;; -> (check-equal? (send CLOCK2 toy-y) 30)-> true
    ;; STRATEGY: Cases on whether the clock is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))
            (set! selected? true))
          55))
    
    ;; in-clock? : Int Int -> Boolean
    ;; GIVEN: the x and y coordinates of the mouse posiiton in the button down
    ;; event
    ;; RETURNS: true iff the given new coordinates is inside the bounding box of
    ;; the given canvas.
    (define (in-clock? other-x other-y)
      (and (<= (- x LEFT-BOUNDARY) other-x (+ x LEFT-BOUNDARY))
           (<= (- y TOP-BOUNDARY) other-y (+ y TOP-BOUNDARY))))
    
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with clock
    ;; painted on it.
    ;; EXAMPLE:(send CLOCK1 add-to-scene EMPTY-CANVAS)
    ;;-> (place-image TEXT 40 30
    ;;   (place-image SQR 40 30 EMPTY-CANVAS))
    (define/public (add-to-scene canvas)
      (local ((define COLOR "blue")
              (define CLOCK (square side "outline" "blue")))
      (place-image (place-text COLOR) x y
                   (place-image CLOCK x y canvas))))
    
    ;; place-text: String -> TextString
    ;; GIVEN: the color
    ;; RETURNS: the number of ticks that will be displayed inside the clock 
    (define (place-text str)
      (local ((define FONT 13))
      (text (number->string val) FONT str)))
    
    ;; -> Boolean
    (define/public (for-test:selected?) false)

    ;; -> Int
    (define/public (for-test:saved-mx) saved-mx)
    ))
;;============================================================================;;
;;                                   make-clock                               ;;
;;============================================================================;;
;; make-clock : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.
;; EXAMPLE:(make-clock 40 30)
(define (make-clock x y)
  (new Clock% [x x] [y y]))

;;============================================================================;;
;;                                   TESTS                                    ;;
;;============================================================================;;

;; Examples for testing
;; ---------------------
(define CLOCK1 (new Clock%
                    [x 40]
                    [y 30]
                    [val 1]
                    [selected? false]
                    [saved-mx SEL-INIT-X]
                    [saved-my SEL-INIT-Y]))
(define CLOCK2 (new Clock%
                    [x 40]
                    [y 30]
                    [val 1]
                    [selected? true]
                    [saved-mx SEL-INIT-X]
                    [saved-my SEL-INIT-Y]))

(define TEXT (text (number->string 1) 13 "blue"))
(define SQR (square 40 "outline" "blue"))

;; Test cases for Clock toy
;; -------------------------
(begin-for-test
  (local
    ((define c1 (make-clock 40 30)))
    (check-equal? (send c1 toy-x) 40)
    (check-equal? (send c1 toy-y) 30)
    (check-equal? (send c1 toy-data) 0)
    (send c1 after-tick)
    (check-equal? (send c1 toy-data) 1)
    (send c1 after-key-event "w")
    (check-equal? (send c1 toy-data) 1)
    (send CLOCK2 after-button-down 45 35)
    (check-equal? (send CLOCK2 toy-x) 40)
    (send c1 after-button-down 80 70)
    (check-equal? (send c1 for-test:selected?) false)
    (send CLOCK2 after-drag 45 35)
    (check-equal? (send CLOCK2 toy-y) 30)
    (send CLOCK1 after-drag 85 35)
    (check-equal? (send CLOCK1 for-test:saved-mx) 0)
    (send CLOCK2 after-button-up 45 35)
    (check-equal? (send CLOCK2 for-test:selected?) false)
    (check-equal? (send CLOCK1 add-to-scene EMPTY-CANVAS)
                  (place-image TEXT 40 30
                               (place-image SQR 40 30 EMPTY-CANVAS)))))
