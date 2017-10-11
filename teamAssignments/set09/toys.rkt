;; toys.rkt

;; GOAL
;; -----

;; To simulate a marvelous toy which has following features:
;; 1) The toy consists of a canvas that is 600 pixels high and 500 pixels wide.
;; 2) On the canvas, the system displays a circle of radius 10 in outline mode.
;;    The circle initially appears in the center of the canvas which is called
;;    the "target."
;; 3) The child interacts with the toy by dragging the target (using smooth
;;    drag, as usual) and by typing character into the system. Each of the
;;    characters listed below causes a new toy to be created with its center
;;    located at the center of the target. Toys are also moveable using smooth
;;    drag.
;;    a) When the child types "s", a new square-shaped toy pops up. It is
;;       represented as a 40x40 pixel outline square. When a square-shaped
;;       toy appears, it begins travelling rightward at a constant rate. When
;;       its edge reaches the edge of the canvas, it executes a Perfect Bounce.
;;    b) When the child types "t", a new throbber appears. A throbber starts
;;       as a solid green circle of radius 5. At every tick, it expands
;;       gradually until it reaches a radius of 20. Once it reaches a radius
;;       of 20, it contracts gradually until it reaches a radius of 5, and
;;       then resumes its cycle.
;;    c) When the child types "w", a clock appears. This clock displays the
;;       number of ticks since it was created. Otherwise the appearance of
;;       the clock is unspecified.
;;    d) When the child types "f", a Official Tom Brady Deflatable Football(TM) 
;;       appears. The TBDF initially appears as an image of a football, but it
;;       gets smaller with every tick until it reaches size 0.
;;  4) We are not responsible for anything that happens after the mouse leaves
;;     the canvas.
;=============================================================================

#lang racket

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide
 Widget<%>
 WorldState<%>
 PlaygroundState<%>
 Toy<%>
 PlaygroundState%
 Square-toy%
 Football%
 Clock%
 Throbber%
 make-world
 run
 make-square-toy
 make-throbber
 make-clock
 make-football)

;==============================================================================
;;                               CONSTANTS                                    ;;
;==============================================================================
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

;==============================================================================
;;                            Data Definitions                                ;;
;=============================================================================

;; A Time is a NonNegative Int

;; A Widget is an object whose class implements Widget<%>

;; A PlaygroundState is a
;; (make-world-state ListOfToys PosInt PosInt
;;                   PosInt Int Int Boolean)

;; INTERP:
;; (make-world-state toys speed targ-x targ-y targ-sel-x targ-sel-y targ-sel?)
;; represents a world containing the
;; toys which represents the list of toys - ListOfToys<%>
;; speed is the square speed at which the square toy moves towards right
;; targ-x and targ-y are the x and y coordinates of the center of the target
;; targ-sel-x and targ-sel-y are the x and y coordinates of the mouse event
;; targ-sel? is the predicate which represents whether the target is selected
;; or not


;; A PlayGround is an object whose class implements PlayGroundState<%>

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
;==============================================================================
;;                                  Interfaces                                ;;
;==============================================================================

;; Widget Interface
;; -----------------

;; Every object that lives in the world must implement the Widget<%>
;; interface.

(define Widget<%>
  (interface ()
    
    ;; -> Widget
    ;; GIVEN: no arguments
    ;; RETURNS: the state of this object that should follow at time(t+1).
    after-tick          
    
    ;; Int Int -> Widget
    ;; GIVEN: a location of the mouse
    ;; RETURNS: the state of this object that should follow the
    ;; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    
    ;; KeyEvent -> Widget
    ;; GIVEN: a key event
    ;; RETURNS: the state of this object that should follow the
    ;; given key event
    after-key-event
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    add-to-scene
    ))

;; WorldState Interface
;; ---------------------

;; The World implements the WorldState<%> interface

(define WorldState<%>
  (interface ()
    
    ;; -> World
    ;; GIVEN: no arguments
    ;; RETURNS: the state of the world at the next tick
    after-tick          
    
    ;; Int Int MouseEvent-> World
    ;; GIVEN: a location of mouse and a mouse event
    ;; RETURNS: the state of the world that should follow the
    ;; given mouse event at the given location.
    after-mouse-event
    
    ;; KeyEvent -> Widget
    ;; GIVEN: a key event
    ;; RETURNS: the state of the world that should follow the
    ;; given key event
    after-key-event     
    
    ;; -> Scene
    ;; RETURNS: a scene that depicts this World
    to-scene
    ))


;; PlaygroundState Interface
;; --------------------------

;; Playground implements the PlaygroundState<%> interface

(define PlaygroundState<%>
  (interface (WorldState<%>) ;; this means: include all the methods in
    ;; WorldState<%>. 
    
    ;; -> PosInt
    ;; -> PosInt
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
  (interface (Widget<%>);; this means: include all the methods in Widget<%>
    
    ;; -> PosInt
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> PosInt
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

;==============================================================================
;;                                  CLASSES                                   ;;
;==============================================================================

;; SQUARE-TOY CLASS
;; -----------------

;; A Square-toy is a 40x40 square which moves rightward at a constant rate and 
;; executes a perfect bounce when it reaches the edge of the canvas.
;; Square is selectable and draggable. It does not move while being selected.
;; A Square-toy is a (new Square-toy% [x PosInt] [y PosInt] [vel PosInt]
;;                                    [selected? Boolean] [saved-mx Int]
;;                                    [saved-my Int])
               
(define Square-toy%
  (class* object%(Toy<%>)
    
    ;; x and y represents the coordinates of the center of the square toy
    ;; vel represents the speed at which the square moves horizontally
    (init-field x y) ;;  PosInt
    (init-field vel) ;;  PosInt
    
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
    ;; EXAMPLE:(send s1 toy-x)->50
    ;; (send s1 toy-y)->50
    (define/public (toy-x) x) 
    (define/public (toy-y) y)
    
    ;; toy-data -> PosInt
    ;; RETURNS:the velocity of the square (rightward is positive)
    ;; EXAMPLE: (send s1 toy-data)->1
    (define/public (toy-data) vel) 
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    ;; add a new field,the square's side length initialized to 40
    (field [side SIDE]) ;; PosInt
    
    ;; after-tick : -> Toy<%>
    ;; GIVEN: no arguments
    ;; RETURNS: a square toy like this one, but as it should be after a tick
    ;; WHERE: a selected square doesn't move.
    ;; EXAMPLE:(send (send s1 after-tick) for-test:square)
    ;; ->(send SQUARE2 for-test:square))
    ;; STRATEGY: Cases on whether the square is selected
    (define/public (after-tick)
      (if selected?
          this
          (new Square-toy%
               [x (set-x)]
               [y y]
               [vel (set-vel)]
               [selected? selected?]
               [saved-mx saved-mx]
               [saved-my saved-my])))
    
    ;; set-x: -> PosInt
    ;; GIVEN: no arguments
    ;; RETURNS: The x-coordinate of the center of the square after the next tick
    ;; STRATEGY: Combine simpler functions
    (define (set-x)
      (cond
        [(>= (+ x vel) RIGHT-BOUNDARY) RIGHT-BOUNDARY]
        [(<= (+ x vel) LEFT-BOUNDARY) LEFT-BOUNDARY]
        [else (+ x vel)]))
    
    ;; set-vel: -> PosInt
    ;; GIVEN: no arguments
    ;; RETURNS: the speed with which the square moves in x-direction after
    ;; the next tick
    ;; STRATEGY:Combine simpler functions
    (define (set-vel)
      (if (or (>= (+ x vel) RIGHT-BOUNDARY)
              (<= (+ x vel) LEFT-BOUNDARY))
          (- 0 vel)
          vel))
    
    ;; after-key-event : KeyEvent -> Toy<%>
    ;; GIVEN: a key event
    ;; RETURNS: a square-toy like this one, but as it should be after the
    ;; given key event.
    ;; EXAMPLE:(send (send s1 after-key-event "w") for-test:square)
    ;; ->(send s1 for-test:square))
    (define/public (after-key-event kev) this)
    
    ;; after-button-down : Int Int -> Toy<%>
    ;; GIVEN: the location of mouse in button-down event
    ;; RETURNS: a square-toy like this one, but as it should be after button
    ;; down event
    ;; EXAMPLE:(send (send s1 after-button-down 85 55) for-test:square)
    ;; ->(send s1 for-test:square)
    ;; STRATEGY: Cases on whether the new mouse position is inside the square
    ;; toy
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
          (new Square-toy%
               [x x] [y y] [vel vel]
               [selected? true]
               [saved-mx (- mx x)] [saved-my (- my y)])
          this))
    
    ;; after-button-up : Int Int -> Toy<%>
    ;; GIVEN: the location of a mouse
    ;; RETURNS: a square-toy like this one, but as it should be after button
    ;; up event
    ;; EXAMPLE:(send (send SQUARE1 after-button-up 55 55) for-test:square)
    ;; ->(send s1 for-test:square))
    (define/public (after-button-up mx my)
      (new Square-toy%
           [x x] [y y] [vel vel]
           [selected? false]
           [saved-mx saved-mx] [saved-my saved-my]))
    
    ;; after-drag : Int Int -> Toy<%>
    ;; GIVEN : x and y coordinates of mouse of the drag event
    ;; RETURNS: a square-toy like this one, but as it should be after button
    ;; drag event
    ;; EXAMPLE:(send (send SQUARE1 after-drag 55 55) for-test:square)
    ;; ->(send SQUARE6 for-test:square))
    ;; STRATEGY: Cases on whether the square toy is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Square-toy%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [vel vel]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    
    ;; in-square? : Int Int -> Boolean
    ;; GIVEN: the x and y coordinates of the mouse position
    ;; RETURNS true iff the given new coordinates is inside the bounding box of
    ;; the given canvas.
    ;; STRATEGY: Combine simpler functions
    (define (in-square? other-x other-y)
      (and (<= (- x LEFT-BOUNDARY) other-x (+ x LEFT-BOUNDARY))
           (<= (- y TOP-BOUNDARY) other-y (+ y TOP-BOUNDARY))))
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with the square toy
    ;; painted on it.
    ;; EXAMPLE:(send s1 add-to-scene EMPTY-CANVAS)->
    ;;  (place-image SQR1 50 50 EMPTY-CANVAS)
    (define/public (add-to-scene canvas)
      (place-image (square side "outline" "black") x y canvas))
    
    ;; -> (list PosInt PosInt PosInt Boolean Int Int)
    (define/public (for-test:square)
      (list x y vel selected? saved-mx saved-my))
    ))

;; make-square-toy : PosInt PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;; travelling right at the given speed.
;; EXAMPLE:(make-square-toy 50 50 1)
(define (make-square-toy x y vel)
  (new Square-toy% [x x][y y] [vel vel]))


;; Clock Class
;; ------------------

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
    ;; EXAMPLE:(send c1 toy-x)->40
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
    
    ;; after-tick : -> Toy<%>
    ;; GIVEN: no arguments
    ;; RETURNS: A clock like this one, but as it should be after a tick
    ;; a selected clock doesn't move.
    ;; EXAMPLE:(send (send c1 after-tick) for-test:clock)
    ;; ->(send CLOCK1 for-test:clock))
    (define/public (after-tick)
      (new Clock%
           [x x]
           [y y]
           [val (+ val 1)]
           [selected? selected?]
           [saved-mx saved-mx]
           [saved-my saved-my]))
    
    ;; after-key-event : KeyEvent -> Toy<%>
    ;; GIVEN: a key event
    ;; RETURNS: A clock like this one, but as it should be after the
    ;; given key event.
    ;; EXAMPLE:(send (send c1 after-key-event "w") for-test:clock)
    ;; ->(send c1 for-test:clock)
    (define/public (after-key-event kev) this)
    
    ;; after-button-down : Int Int -> Toy<%>
    ;; GIVEN: the location of the mouse in button-down event
    ;; RETURNS: a clock like this one, but as it should be after button
    ;; down event
    ;; EXAMPLE:(send (send c1 after-button-down 80 70) for-test:clock)
    ;; ->(send c1 for-test:clock)
    ;; STRATEGY: Cases on whether the coordinates of the mouse is in the clock 
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (new Clock%
               [x x] [y y] [val val]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    
    ;; after-button-up : Int Int -> Toy<%>
    ;; GIVEN: the location of a mouse during button-up event
    ;; RETURNS: a clock like this one, but as it should be after button
    ;; up event
    ;; EXAMPLE:(send (send CLOCK2 after-button-up 45 35) for-test:clock)
    ;;->(send CLOCK1 for-test:clock)
    (define/public (after-button-up mx my)
      (new Clock%
           [x x] [y y] [val val]
           [selected? false]
           [saved-mx saved-mx]
           [saved-my saved-my]))
    
    ;; after-drag : Int Int -> Toy<%>
    ;; GIVEN: the location of mouse during drag event
    ;; RETURNS: a clock like this one, but as it should be after drag event
    ;; EXAMPLE:(send (send CLOCK1 after-drag 85 35) for-test:clock)
    ;;-> (send CLOCK1 for-test:clock)
    ;; STRATEGY: Cases on whether the clock is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Clock%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [val val]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    
    ;; in-clock? : Int Int -> Boolean
    ;; GIVEN: the x and y coordinates of the mouse posiiton in the button down
    ;; event
    ;; RETURNS true iff the given new coordinates is inside the bounding box of
    ;; the given canvas.
    ;; STRATEGY: Combine simpler functions
    (define (in-clock? other-x other-y)
      (and (<= (- x LEFT-BOUNDARY) other-x (+ x LEFT-BOUNDARY))
           (<= (- y TOP-BOUNDARY) other-y (+ y TOP-BOUNDARY))))
    
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with clock
    ;; painted on it.
    ;; EXAMPLE:(send CLOCK1 add-to-scene EMPTY-CANVAS)
    ;;-> (place-image TEXT 40 30
    ;;                         (place-image SQR 40 30 EMPTY-CANVAS))
    ;; STRATEGY: Combine simpler functions
    (define/public (add-to-scene canvas)
      (place-image (place-text "blue") x y
                   (place-image (square side "outline" "blue") x y canvas)))
    
    ;; place-text: String -> TextString
    ;; GIVEN:the color
    ;; RETURNS: the number of ticks that will be displayed inside the clock 
    ;; STRATEGY: Combine simpler functions
    (define (place-text str)
      (text (number->string val) 13 str))
    
    ;; -> (list PosInt PosInt PosInt Boolean Int Int)
    (define/public (for-test:clock)
      (list x y val selected? saved-mx saved-my))
    ))

;; make-clock : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.
;; EXAMPLE:(make-clock 40 30)
(define (make-clock x y)
  (new Clock% [x x] [y y]))


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
    
    ;; toy-x -> PosInt
    ;; toy-y -> PosInt
    ;; RETURNS: the x or y position of the center of the throbber toy
    ;; EXAMPLE:(send t1 toy-x)-> 50
    ;; (send t1 toy-y)->50
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    ;; toy-data -> PosInt
    ;; RETURNS:the radius of the throbber
    ;; EXAMPLE:(send t1 toy-data) ->5
    (define/public (toy-data) radius)
    
    ;; after-tick:  -> Toy<%>
    ;; GIVEN:  no arguments
    ;; RETURNS: the state of the throbber that should follow at time t+1.
    ;; EXAMPLE: (send (send t1 after-tick) for-test:throbber)
    ;; ->(send THROBBER4 for-test:throbber)
    ;; (send (send THROBBER1 after-tick) for-test:throbber)
    ;; ->(send THROBBER5 for-test:throbber)
    ;; STRATEGY: Cases based on is-increasing? flag
    (define/public (after-tick)
      (if is-increasing?
          (increase-radius)
          (decrease-radius)))
    
    ;; increase-radius -> Toy<%>
    ;; GIVEN: no argument
    ;; RETURNS: a new throbber with radius increased by 1 or reversing
    ;; the is-increasing? flag depending upon current radius.
    ;; STRATEGY: Cases on the current value of the radius of the throbber
    (define (increase-radius)
      (if (< radius 20)
          (new Throbber%
               [x x]
               [y y]
               [radius (+ radius 1)]
               [is-increasing? true]
               [saved-mx saved-mx]
               [saved-my saved-my]
               [selected? selected?])
          (new Throbber%
               [x x]
               [y y]
               [radius (- radius 1)]
               [is-increasing? false]
               [saved-mx saved-mx]
               [saved-my saved-my]
               [selected? selected?])))
    
    ;; decrease-radius -> Toy<%>
    ;; GIVEN: no argument
    ;; RETURNS: a new throbber with radius decreased by 1 or reversing
    ;; the is-increasing? flag depending upon current radius.
    ;; STRATEGY: Cases on the current value of the radius of the throbber
    (define (decrease-radius)
      (if (> radius 5)
          (new Throbber%
               [x x]
               [y y]
               [radius (- radius 1)]
               [is-increasing? false]
               [saved-mx saved-mx]
               [saved-my saved-my]
               [selected? selected?])
          (new Throbber%
               [x x]
               [y y]
               [radius (+ radius 1)]
               [is-increasing? true]
               [saved-mx saved-mx]
               [saved-my saved-my]
               [selected? selected?])))
    
    ;; after-key-event: KeyEvent -> Toy<%>
    ;; GIVEN: a key event
    ;; RETURNS: a throbber like this one, but as it should be after the
    ;; given key event.
    ;; EXAMPLE:(send (send t1 after-key-event "w") for-test:throbber)
    ;; -> (send t1 for-test:throbber)
    (define/public (after-key-event kev) this)
    
    ;; after-button-down:Int Int -> Toy<%>
    ;; GIVEN: the location of mouse in button-down event
    ;; RETURNS: the state of the throbber like this one,but as it should be
    ;; after button down event
    ;; EXAMPLE:(send (send t1 after-button-down 55 55) for-test:throbber)
    ;; -> (send t1 for-test:throbber)
    ;; STRATEGY:Cases on whether the new mouse position is in the throbber
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (new Throbber%
               [x x]
               [y y]
               [radius radius]
               [is-increasing? is-increasing?]
               [saved-mx mx]
               [saved-my my]
               [selected? true])
          this
          ))
    
    ;; after-button-up: Int Int -> Toy<%>
    ;; GIVEN : x and y coordinates of mouse button up event
    ;; RETURNS: a new unselected throbber after button up mouse event
    ;; EXAMPLE:(send (send t1 after-button-up 55 55) for-test:throbber)
    ;; ->(send t1 for-test:throbber)
    (define/public (after-button-up mx my)
      (new Throbber%
           [x x] [y y]
           [radius radius]
           [is-increasing? is-increasing?]
           [saved-mx 0] [saved-my 0]
           [selected? false]))
    
    ;; after-drag: Int Int -> Toy<%>
    ;; GIVEN : x and y coordinates of mouse of the drag event
    ;; RETURNS: a throbber like this one, but as it should be after 
    ;; drag event
    ;; EXAMPLE: (send (send THROBBER9 after-drag 60 90) for-test:throbber)
    ;; ->(send THROBBER10 for-test:throbber)
    ;; STRATEGY: Cases on whether the throbber is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Throbber%
               [x (+ x (- mx saved-mx))]
               [y (+ y (- my saved-my))]
               [radius radius]
               [is-increasing? is-increasing?]
               [saved-mx mx] [saved-my my]
               [selected? true])
          this))
    
    
    ;; in-throbber?: Int Int -> Boolean
    ;; GIVEN: x and y coordinates of mouse button down event
    ;; RETURNS true iff the given new coordinates is inside the bounding box of
    ;; the given circle.
    ;; STRATEGY: Combine simpler functions
    (define (in-throbber? mx my)
      (<= (+ (sqr (- x mx)) (sqr (- y my)))
          (sqr radius)))
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with the throbber
    ;; painted on it.
    ;; EXAMPLE:(send t1 add-to-scene EMPTY-CANVAS)
    ;; ->  (place-image (circle 5 "solid" "green") 50 50 EMPTY-CANVAS))
    ;; STARTEGY:Combine simpler functions
    (define/public (add-to-scene canvas)
      (place-image (circle radius "solid" "green") x y canvas))
    
    ;; -> (list PosInt PosInt PosInt Boolean Int Int Boolean)
    (define/public (for-test:throbber)
      (list x y radius is-increasing? saved-mx saved-my selected?))))

;; make-throbber: PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
;; EXAMPLE:(make-throbber 50 50)
(define (make-throbber x y)
  (new Throbber% [x x] [y y]))


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
    ;; EXAMPLE:(send f1 toy-x) ->50
    ;; (send f1 toy-y) ->50
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    ;; toy-data -> PosInt
    ;; RETURNS:the size of the football which is the product of the width
    ;; height
    ;; EXAMPLE:(send f1 toy-data) -> (* 64 64)
    (define/public (toy-data) size)
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    ;; add a new field,the football's size initialized
    ;; where size is the product of the width and height of the football
    (field [size (* IMAGE-WIDTH IMAGE-HEIGHT)])
    
    ;; after-tick : Time -> Toy<%>
    ;; GIVEN: the framerate at which the football should move at every tick
    ;; RETURNS: A football like this one, but as it should be after a tick
    ;; EXAMPLE: (send (send f1 after-tick) for-test:football)
    ;; -> (send BALL1 for-test:football)
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (new Football%
               [x x]
               [y y]
               [scaling-ratio (* discount-factor scaling-ratio)]
               [selected? selected?]
               [saved-mx saved-mx]
               [saved-my saved-my])))
    
    ;; after-key-event: KeyEvent -> Toy<%>
    ;; GIVEN: a key event
    ;; RETURNS: the state of the football that should follow the
    ;; given key event
    ;; EXAMPLE:f1 for-test:football))
    ;; ->after-key-event "w") for-test:football)
    (define/public (after-key-event kev) this)
    
    ;; after-button-down : Int Int -> Toy<%>
    ;; GIVEN: the location of the mouse on button-down event
    ;; RETURNS: a football like this one, but as it should be after button
    ;; down event
    ;; EXAMPLE:(send (send f1 after-button-down 85 55) for-test:football)
    ;; -> (send BALL2 for-test:football)
    ;; STRATEGY: Cases on whether the new mouse position is inside the football
    (define/public (after-button-down mx my)
      (if (in-football? mx my)
          (new Football%
               [x x][y y]
               [scaling-ratio scaling-ratio]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    
    ;; after-button-up : Int Int -> Toy<%>
    ;; GIVEN: the location of a mouse during the button-up event
    ;; RETURNS: a football like this one, but as it should be after button
    ;; up event
    ;; EXAMPLE:(send (send BALL3 after-button-up 55 55) for-test:football)
    ;; ->(send f1 for-test:football)
    (define/public (after-button-up mx my)
      (new Football%
           [x x] [y y]
           [scaling-ratio scaling-ratio]
           [selected? false]
           [saved-mx saved-mx]
           [saved-my saved-my])) 
    
    ;; after-drag : Int Int -> Toy<%>
    ;; GIVEN : x and y coordinates of mouse of the drag event
    ;; RETURNS: a football like this one, but as it should be after mouse
    ;; drag event
    ;; EXAMPLE:(send (send f1 after-drag 55 55) for-test:football)
    ;; ->
    ;; STRATEGY: Cases on whether the football is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Football%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [scaling-ratio scaling-ratio]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    
    ;; in-football? : Int Int -> Boolean
    ;; RETURNS true iff the given new coordinates is inside the boundary of
    ;; the given football.
    ;; STRATEGY: Combine simpler functions
    (define (in-football? other-x other-y)
      (local [(define HALF-WIDTH (/ IMAGE-WIDTH 2))
              (define HALF-HEIGHT (/ IMAGE-HEIGHT 2))]
        (and
         (<= (abs (- other-x x)) HALF-WIDTH)
         (<= (abs (- other-y y)) HALF-HEIGHT))))
    
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with football
    ;; painted on it.
    ;; EXAMPLE:(send f1 add-to-scene EMPTY-CANVAS)
    ;; ->(place-image (scale 1.0 (bitmap "football.png"))
    ;;                          50 50 EMPTY-CANVAS))
    ;; STRATEGY: Combine simpler functions
    (define/public (add-to-scene canvas)
      (place-image
       (scale scaling-ratio FOOTBALL)
       x y
       canvas))
    
    ;; -> (list PosInt PosInt PosReal Boolean Int Int)
    (define/public (for-test:football)
      (list x y scaling-ratio selected? saved-mx saved-my))
    ))

;; make-football : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a football at the given position.
;; EXAMPLE:(make-football 50 50)
(define (make-football x y)
  (new Football% [x x][y y]))

;===============================================================================

;; PlaygroundState Class
;; ----------------------

;; A PlaygroundState is a (make-world-state ListOfToys PosInt
;;                                      PosInt PosInt Int Int Boolean)

;; make-world-state:
;; ListOfToy% PosInt PosInt PosInt Int Int Boolean->
;; PlaygroundState%
;; GIVEN:
;; toys is list of toys in the playground ListOfToys<%>
;; speed is the square speed at which the square toy moves towards right
;; targ-x and targ-y are the x and y coordinates of the center of the target
;; targ-sel-x and targ-sel-y are the x and y coordinates of the mouse event
;; targ-sel? is the predicate which represents whether the target is selected
;; or not
;; RETURNS: a new playground with the toys
(define (make-world-state toys speed targ-x targ-y
                          targ-sel-x targ-sel-y targ-sel?)
  (new PlaygroundState%
       [toys toys]
       [speed speed]
       [targ-x targ-x]
       [targ-y targ-y]
       [targ-sel-x targ-sel-x]
       [targ-sel-y targ-sel-y]
       [targ-sel? targ-sel?]))

;===============================================================================
(define PlaygroundState%
  (class* object%(PlaygroundState<%>)
    
    ;; toys is a list of toys in the playground
    (init-field toys)  ;; ListOfToys<%>
    
    ;; speed is the square speed in (in pixels/tick), at which the square moves
    ;; towards right direction until it bounces bak and then it moves towards
    ;; left.
    (init-field speed) ;; PosInt
    
    ;; targ-x and targ-y are the coordinates of the center of the target
    (init-field [targ-x TARGET-INIT-X]) ;; PosInt
    (init-field [targ-y TARGET-INIT-Y]) ;; PosInt
    
    ;; targ-sel-x and targ-sel-y are the coordinates of the mouse event
    ;; of the target
    (init-field [targ-sel-x SEL-INIT-X]) ;; Int
    (init-field [targ-sel-y SEL-INIT-Y]) ;; Int
    
    ;; is the target selected? Default is false
    (init-field [targ-sel? false]) ;; Boolean
    
    (super-new)
    
    ;; after-tick : -> PlaygroundState<%>
    ;; GIVEN: no argument 
    ;; RETURNS: a new playground after tick
    ;; EXAMPLE: see tests below
    ;; STRATEGY: Use HOF map on toys 
    (define/public (after-tick)
      (make-world-state
       (map
        ;; Toy<%> -> Toy<%>
        ;; GIVEN: a toy in the playground
        ;; RETURNS: the toy as the given one, as it should be after a tick
        (lambda (toy) (send toy after-tick))
        toys)
       speed
       targ-x
       targ-y
       targ-sel-x
       targ-sel-y
       targ-sel?))          
    
    ;; after-mouse-event: Int Int MouseEvent-> PlaygroundState<%>
    ;; GIVEN: x and y coordinates of mouse and a mouse event
    ;; RETURNS: the state of the playground that should follow the
    ;; specified mouse event at the given location.
    ;; EXAMPLE: see tests below
    ;; STRATEGY: Cases on mouse event
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")(world-after-button-down mx my)]
        [(mouse=? mev "drag")(world-after-drag mx my)]
        [(mouse=? mev "button-up")(world-after-button-up mx my)]
        [else this]))
    
    ;; world-after-button-down : Int Int -> PlaygroundState<%>
    ;; GIVEN: x and y coordinates of mouse position  on button down event
    ;; RETURNS: the new playground state after button down mouse event
    ;; EXAMPLE: see tests below
    ;; STRATEGY: Use HOF map on toys 
    (define (world-after-button-down mx my)
      (if (in-target? mx my)
          (make-world-state
           (map
            ;; Toy<%> -> Toy<%>
            ;; GIVEN: a toy in the play ground
            ;; RETURNS: a toy like the given one ,as it should be after
            ;; button down event
            (lambda (toy) (send toy after-button-down mx my))
            toys)
           speed
           targ-x
           targ-y
           mx
           my
           true)
          (make-world-state
           (map
            ;; Toy<%> -> Toy<%>
            ;; GIVEN: a toy in the play ground
            ;; RETURNS: a toy like the given one ,as it should be after
            ;; button down event when the mouse is not inside target
            (lambda (toy) (send toy after-button-down mx my))
            toys)
           speed
           targ-x
           targ-y
           targ-sel-x
           targ-sel-y
           false)
          ))
    
    ;; world-after-drag : Int Int -> PlaygroundState<%>
    ;; GIVEN: x and y coordinates of mouse position
    ;; RETURNS: the new playground state after button down mouse event
    ;; EXAMPLE: see tests below
    ;; STRATEGY: Use HOF map on toys
    (define (world-after-drag mx my)
      (if targ-sel?
          (make-world-state
           (map
            ;; Toy<%> -> Toy<%>
            ;; GIVEN: a toy in the play ground
            ;; RETURNS: a toy like the given one ,as it should be after
            ;; drag event
            (lambda (toy) (send toy after-drag mx my))
            toys)
           speed
           (+ targ-x (- mx targ-sel-x))
           (+ targ-y (- my targ-sel-y))
           mx
           my
           true)
          (make-world-state
           (map
            ;; Toy<%> -> Toy<%>
            ;; GIVEN: a toy in the play ground
            ;; RETURNS: a toy like the given one ,as it should be after
            ;; drag event when the target is not selected
            (lambda (toy) (send toy after-drag mx my))
            toys)
           speed
           targ-x
           targ-y
           targ-sel-x
           targ-sel-y
           targ-sel?)))
    
    ;; in-target? : Int Int -> Boolean
    ;; GIVEN: x and y coordinates of the mouse position
    ;; RETURNS: true iff the mouse event is inside the target
    ;; otherwise returns false
    ;; EXAMPLE: see tests below
    ;; STRATEGY: Combine simpler fucntions
    (define (in-target? x y)
      (<= (+ (sqr (- x targ-x))
             (sqr (- y targ-y)))
          (sqr TARGET-RADIUS)))
    
    ;; world-after-button-up : Int Int -> PlaygroundState<%>
    ;; GIVEN: x and y coordinates of the mouse position for the button up event
    ;; RETURNS: the playground state after button up mouse event
    ;; EXAMPLE: see tests below
    ;; STRATEGY: Use HOF map on toys
    (define (world-after-button-up mx my)
      (make-world-state
       (map
        ;; Toy<%> -> Toy<%>
        ;; GIVEN: a toy in the play ground
        ;; RETURNS: a toy like the given one ,as it should be after
        ;; button up event
        (lambda (toy) (send toy after-button-up mx my))
        toys)
       speed
       targ-x
       targ-y
       SEL-INIT-X
       SEL-INIT-Y
       false))
    
    ;; after-key-event : KeyEvent -> PlaygroundState<%>
    ;; GIVEN: a key event
    ;; RETURNS: the state of the play ground that should follow the
    ;; specified key event
    ;; EXAMPLE: see tests below
    ;; STRATEGY: Cases on key event kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "t") (add-throbber-to-playground)]
        [(key=? kev "s") (add-square-toy-to-playground)]
        [(key=? kev "w") (add-clock-to-playground)]
        [(key=? kev "f") (add-football-to-playground)]
        [else this]))
    
    ;; add-throbber-to-playground :  -> PlaygroundState<%>
    ;; GIVEN:no arguments
    ;; RETURNS: a new palyground after adding a throbber to
    ;; list of toys to the given play ground
    ;; EXAMPLE: see tests below
    (define (add-throbber-to-playground)
      (new PlaygroundState%
           [toys (cons (new Throbber%
                            [x targ-x]
                            [y targ-y]
                            [radius 5]
                            [is-increasing? true]) toys)]
           [speed speed]
           [targ-x targ-x]
           [targ-y targ-y]
           [targ-sel-x targ-sel-x]
           [targ-sel-y targ-sel-y]
           [targ-sel? targ-sel?]))
    
    ;; add-square-toy-to-playground :  -> PlaygroundState<%>
    ;; GIVEN: no arguments
    ;; RETURNS: a new playground after adding a square toy to
    ;; list of toys in the given playground
    ;; EXAMPLE: see tests below
    (define (add-square-toy-to-playground)
      (new PlaygroundState%
           [toys (cons (new Square-toy%
                            [x targ-x]
                            [y targ-y]
                            [vel speed]
                            [selected? false]
                            [saved-mx 0]
                            [saved-my 0]) toys)]
           [speed speed]
           [targ-x targ-x]
           [targ-y targ-y]
           [targ-sel-x targ-sel-x]
           [targ-sel-y targ-sel-y]
           [targ-sel? targ-sel?]))
    
    ;; add-clock-to-playground :  -> PlaygroundState<%>
    ;; GIVEN: no arguments
    ;; RETURNS: a new playground after adding a clock to
    ;; list of toys in the  given playground
    ;; EXAMPLE: see tests below
    (define (add-clock-to-playground)
      (local ((define CLOCK-INIT-TIME 0))
        (new PlaygroundState%
             [toys (cons (new Clock%
                              [x targ-x]
                              [y targ-y]
                              [val CLOCK-INIT-TIME]) toys)]
             [speed speed]
             [targ-x targ-x]
             [targ-y targ-y]
             [targ-sel-x targ-sel-x]
             [targ-sel-y targ-sel-y]
             [targ-sel? targ-sel?])))
    
    ;; add-football-to-playground :  -> PlaygroundState<%>
    ;; GIVEN: no arguments
    ;; RETURNS: a new playground after adding a football to
    ;; list of toys in the given playground
    ;; EXAMPLE: see tests below
    (define (add-football-to-playground)
      (new PlaygroundState%
           [toys (cons (new Football%
                            [x targ-x]
                            [y targ-y]) toys)]
           [speed speed]
           [targ-x targ-x]
           [targ-y targ-y]
           [targ-sel-x targ-sel-x]
           [targ-sel-y targ-sel-y]
           [targ-sel? targ-sel?]))
    
    ;; to-scene:  -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene that depicts this PlaygroundState<%>
    ;; STRATEGY: Use HOF foldr on toys
    ;; EXAMPLE: see tests below
    (define/public (to-scene)
      (foldr
       ;; Toy<%> Scene -> Scene
       ;; GIVEN: a toy in the play ground
       ;; RETURNS: a toy painted in the canvas
       (lambda (toy scene)
         (send toy add-to-scene scene))
       (place-image TARGET-CIRCLE targ-x targ-y EMPTY-CANVAS)
       toys))
    
    ;; RETURNS: the x or y coordinates of the center of the target
    (define/public (target-x) targ-x) ;; Int
    (define/public (target-y) targ-y) ;; Int
    
    
    ;; Is the target selected?.Default value is false
    (define/public (target-selected?) targ-sel?)  ;;Boolean
    
    ;; RETURNS:the list of the toys in the PlaygroundState<%>
    (define/public (get-toys) toys) ;; ListOfToy<%>
    
    ;; -> (list ListOfToy<%> PosInt PosInt PosInt Int Int Boolean )
    ; (define/public (for-test:playground)
    ;  (list toys speed targ-x targ-y targ-sel-x targ-sel-y targ-sel?))

    ))
;; make-playground: PosInt PosInt->PlaygroundState<%>
;; GIVEN: x and y coordinates of the target
;; RETURNS: a playground
;; EXAMPLE: (make-playground '() 1)
(define (make-playground toys speed)
  (new PlaygroundState% [toys '()] [speed 1]))

;===============================================================================
;; make-world: PosInt -> PlaygroundState<%>
;; GIVEN: a square-speed in pixels/tick
;; RETURNS: a world with a target, but no toys, and in which any
;; square toys created in the future will travel at the given speed (in
;; pixels/tick).
;; EXAMPLE: (make-world 1)
(define (make-world speed)
  (new PlaygroundState%
       [toys empty]
       [speed speed]
       [targ-x TARGET-INIT-X]
       [targ-y TARGET-INIT-Y]
       [targ-sel-x SEL-INIT-X]
       [targ-sel-y SEL-INIT-Y]
       [targ-sel? false]))


;===============================================================================
;; run : PosNum PosInt -> PlaygroundState<%> 
;; GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;;       creates and runs a world in which square toys travel at the given
;;       speed.
;; RETURNS: the final state of the world.
;; EXAMPLE(run 0.1 1)
;; STRATEGY: Deliver events to the event handler functions
(define (run frameRate squareSpeed)
  (big-bang (make-world squareSpeed)
            (on-tick
             ;; PlaygroundState<%> -> PlaygroundState<%>
             ;; GIVEN: a play ground state
             ;; RETURNS: the state of the given play ground after a tick
             (lambda (w) (send w after-tick))
             frameRate)
            (on-draw
             ;; PlaygroundState<%> -> Scene
             ;; GVIEN: a play ground state
             ;; RETURNS: the given play ground painted on a scene
             (lambda (w) (send w to-scene)))
            (on-key
             ;; PlaygroundState<%> KeyEvent -> PlaygroundState<%>
             ;; GIVEN: a play ground state and a key event
             ;; RETURNS: the play ground state that should after the
             ;; given key event
             (lambda (w kev)
               (send w after-key-event kev)))
            (on-mouse
             ;; PlaygroundState<%> Integer Integer MouseEvent ->
             ;; PlaygroundState<%>
             ;; GIVEN: a play ground state and x,y coordinates of mouse
             ;; RETURNS: the given play ground state followed after the given
             ;;          mouse event
             (lambda (w mx my mev)
               (send w after-mouse-event mx my mev)))))

;===============================================================================
;; Examples for testing

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

(define CLOCK3 (new Clock% [x 40] [y 30] [val 1] [selected? true]
                    [saved-mx 5] [saved-my 5]))

(define CLOCK4 (new Clock% [x 45] [y 35] [val 1] [selected? true]
                    [saved-mx 0] [saved-my 0]))
(define TEXT (text (number->string 1) 13 "blue"))
(define SQR (square 40 "outline" "blue"))
(define SQR1 (square 40 "outline" "black"))
(define SQUARE1 (new Square-toy% [x 50] [y 50] [vel 1] [selected? true]
                     [saved-mx SEL-INIT-X] [saved-my SEL-INIT-Y]))
(define SQUARE2 (new Square-toy% [x 51] [y 50] [vel 1] [selected? false]
                     [saved-mx SEL-INIT-X] [saved-my SEL-INIT-Y]))
(define SQUARE3 (new Square-toy% [x 480] [y 50] [vel -1]
                     [selected? false] [saved-mx SEL-INIT-X]
                     [saved-my SEL-INIT-Y]))
(define SQUARE4 (new Square-toy% [x 20] [y 50] [vel 0] [selected? false]
                     [saved-mx SEL-INIT-X] [saved-my SEL-INIT-Y]))
(define SQUARE5 (new Square-toy% [x 50] [y 50] [vel 1] [selected? true]
                     [saved-mx 5] [saved-my 5]))
(define SQUARE6 (new Square-toy% [x 55] [y 55] [vel 1] [selected? true]
                     [saved-mx SEL-INIT-X] [saved-my SEL-INIT-Y]))
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
(define THROBBER4 (new Throbber% [x 50] [y 50] [radius 6]
                       [is-increasing? true] [saved-mx 0]
                       [saved-my 0] [selected? false]))
(define THROBBER5 (new Throbber% [x 50] [y 50] [radius 19]
                       [is-increasing? false] [saved-mx 0]
                       [saved-my 0] [selected? false]))
(define THROBBER6 (new Throbber% [x 50] [y 50] [radius 19]
                       [is-increasing? false] [saved-mx 0]
                       [saved-my 0] [selected? false]))
(define THROBBER7 (new Throbber% [x 50] [y 50] [radius 5]
                       [is-increasing? true] [saved-mx 0]
                       [saved-my 0] [selected? false]))
(define THROBBER8 (new Throbber% [x 50] [y 50] [radius 5]
                       [is-increasing? true] [saved-mx 51]
                       [saved-my 51] [selected? true]))
(define THROBBER9 (new Throbber% [x 50] [y 50] [radius 5]
                       [is-increasing? true] [saved-mx 0]
                       [saved-my 0] [selected? true]))
(define THROBBER10 (new Throbber% [x 110] [y 140] [radius 5]
                        [is-increasing? true] [saved-mx 60]
                        [saved-my 90] [selected? true]))
(define BALL1 (new Football% [x 50] [y 50] [scaling-ratio (* 1.0 0.95)]))
(define BALL2 (new Football% [x 50] [y 50] [scaling-ratio 1.0]
                   [selected? true] [saved-mx 5] [saved-my 5]))
(define BALL3 (new Football% [x 50] [y 50] [selected? true]))
(define BALL4 (new Football% [x 55] [y 55] [selected? true]))

(define toys_list (list SQUARE1 (make-clock 40 30) THROBBER2))
(define PLAY1 (make-world-state toys_list 1 TARGET-INIT-X TARGET-INIT-Y
                                SEL-INIT-X SEL-INIT-Y false))

;; TESTS:
(begin-for-test
  ;; Test cases for Clock toy
  (local
    ((define c1 (make-clock 40 30)))
    (check-equal? (send c1 toy-x) 40)
    (check-equal? (send c1 toy-y) 30)
    (check-equal? (send c1 toy-data) 0)
    (check-equal? (send (send c1 after-tick) for-test:clock)
                  (send CLOCK1 for-test:clock))
    (check-equal? (send (send c1 after-key-event "w") for-test:clock)
                  (send c1 for-test:clock))
    (check-equal? (send (send CLOCK2 after-button-down 45 35) for-test:clock)
                  (send CLOCK3 for-test:clock))
    (check-equal? (send (send c1 after-button-down 80 70) for-test:clock)
                  (send c1 for-test:clock))
    (check-equal? (send (send CLOCK2 after-drag 45 35) for-test:clock)
                  (send CLOCK4 for-test:clock))
    (check-equal? (send (send CLOCK1 after-drag 85 35) for-test:clock)
                  (send CLOCK1 for-test:clock))
    (check-equal? (send (send CLOCK2 after-button-up 45 35) for-test:clock)
                  (send CLOCK1 for-test:clock))
    (check-equal? (send (send c1 after-button-up 90 35) for-test:clock)
                  (send c1 for-test:clock))
    (check-equal? (send CLOCK1 add-to-scene EMPTY-CANVAS)
                  (place-image TEXT 40 30
                               (place-image SQR 40 30 EMPTY-CANVAS))))
  
  ;; Test cases for Square-toy
  (local ((define s1 (make-square-toy 50 50 1)))
    (check-equal? (send s1 toy-x) 50)
    (check-equal? (send s1 toy-y) 50)
    (check-equal? (send s1 toy-data) 1)
    (check-equal? (send (send s1 after-tick) for-test:square)
                  (send SQUARE2 for-test:square))
    (check-equal? (send (send SQUARE6 after-tick) for-test:square)
                  (send SQUARE6 for-test:square))
    (check-equal? (send (send (make-square-toy 499 50 1) after-tick)
                        for-test:square)
                  (send SQUARE3 for-test:square))
    (check-equal? (send (send (make-square-toy 0 50 0) after-tick)
                        for-test:square)
                  (send SQUARE4 for-test:square))
    (check-equal? (send (send s1 after-key-event "w") for-test:square)
                  (send s1 for-test:square))
    (check-equal? (send (send s1 after-button-down 55 55) for-test:square)
                  (send SQUARE5 for-test:square))
    (check-equal? (send (send s1 after-button-down 85 55) for-test:square)
                  (send s1 for-test:square))
    (check-equal? (send (send SQUARE1 after-button-up 55 55) for-test:square)
                  (send s1 for-test:square))
    (check-equal? (send (send SQUARE1 after-drag 55 55) for-test:square)
                  (send SQUARE6 for-test:square))
    (check-equal? (send (send s1 after-drag 55 55) for-test:square)
                  (send s1 for-test:square))
    (check-equal? (send s1 add-to-scene EMPTY-CANVAS)
                  (place-image SQR1 50 50 EMPTY-CANVAS)))
  
  ;; Test cases for throbber toy
  
  (local ((define t1 (make-throbber 50 50)))
    (check-equal? (send t1 toy-x) 50)
    (check-equal? (send t1 toy-y) 50)
    (check-equal? (send t1 toy-data) 5)
    (check-equal? (send (send t1 after-tick) for-test:throbber)
                  (send THROBBER4 for-test:throbber))
    (check-equal? (send (send THROBBER1 after-tick) for-test:throbber)
                  (send THROBBER5 for-test:throbber))
    (check-equal? (send (send THROBBER2 after-tick) for-test:throbber)
                  (send THROBBER6 for-test:throbber))
    (check-equal? (send (send THROBBER3 after-tick) for-test:throbber)
                  (send THROBBER7 for-test:throbber))
    (check-equal? (send (send t1 after-button-down 51 51) for-test:throbber)
                  (send THROBBER8 for-test:throbber))
    (check-equal? (send (send t1 after-button-down 55 55) for-test:throbber)
                  (send t1 for-test:throbber))
    (check-equal? (send (send t1 after-button-up 55 55) for-test:throbber)
                  (send t1 for-test:throbber))
    (check-equal? (send (send THROBBER9 after-drag 60 90) for-test:throbber)
                  (send THROBBER10 for-test:throbber))
    (check-equal? (send (send t1 after-drag 100 50) for-test:throbber)
                  (send t1 for-test:throbber))
    (check-equal? (send (send t1 after-key-event "w") for-test:throbber)
                  (send t1 for-test:throbber))
    (check-equal? (send t1 add-to-scene EMPTY-CANVAS)
                  (place-image (circle 5 "solid" "green") 50 50 EMPTY-CANVAS)))

  ;; Test cases for football
  
  (local ((define f1 (make-football 50 50)))
    (check-equal? (send f1 toy-x) 50)
    (check-equal? (send f1 toy-y) 50)
    (check-equal? (send f1 toy-data) (* 64 64))
    (check-equal? (send (send f1 after-tick) for-test:football)
                  (send BALL1 for-test:football))
    (check-equal? (send (send BALL3 after-tick) for-test:football)
                  (send BALL3 for-test:football))
    (check-equal? (send (send f1 after-key-event "w") for-test:football)
                  (send f1 for-test:football))
    (check-equal? (send (send f1 after-button-down 55 55) for-test:football)
                  (send BALL2 for-test:football))
    (check-equal? (send (send f1 after-button-down 85 55) for-test:football)
                  (send f1 for-test:football))
    (check-equal? (send (send BALL3 after-button-up 55 55) for-test:football)
                  (send f1 for-test:football))
    (check-equal? (send (send f1 after-drag 55 55) for-test:football)
                  (send f1 for-test:football))
    (check-equal? (send (send BALL3 after-drag 55 55) for-test:football)
                  (send BALL4 for-test:football))
    (check-equal? (send f1 add-to-scene EMPTY-CANVAS)
                  (place-image (scale 1.0 (bitmap "football.png"))
                               50 50 EMPTY-CANVAS))))
;===============================================================================
;; TESTS for PlayGroundState<%>

(define tb (make-throbber 50 50))
(define toy-list (list THROBBER1))
(define throbber-after-tick (send THROBBER1 after-tick))
(define toy-list2 (list throbber-after-tick))
(define initial-playground-state
  (make-world-state (list tb) 3 TARGET-INIT-X TARGET-INIT-Y SEL-INIT-X
                    SEL-INIT-Y false))
(define initial-playground-state-after-tick
  (make-world-state (list (send tb after-tick)) 3 TARGET-INIT-X
                    TARGET-INIT-Y SEL-INIT-X SEL-INIT-Y false))
(define new-playground
  (new PlaygroundState%
       [toys (list THROBBER1)]
       [speed 3]
       [targ-x TARGET-INIT-X]
       [targ-y TARGET-INIT-Y]
       [targ-sel-x SEL-INIT-X]
       [targ-sel-y SEL-INIT-Y]
       [targ-sel? true]))

(begin-for-test
  (check-equal? (send (send tb after-tick) for-test:throbber)
                (send THROBBER4 for-test:throbber))
  (check-equal? (equal? (send initial-playground-state after-tick)
                        initial-playground-state-after-tick)
                false)
  (check-equal? (equal? (send initial-playground-state after-mouse-event
                              50 50 "button-down")
                        (new PlaygroundState%
                             [toys (list (send tb after-button-down 50 50))]
                             [speed 3]
                             [targ-x TARGET-INIT-X]
                             [targ-y TARGET-INIT-Y]
                             [targ-sel-x SEL-INIT-X]
                             [targ-sel-y SEL-INIT-Y]
                             [targ-sel? false]))
                false)
  (check-equal?
   (equal? (send initial-playground-state after-mouse-event TARGET-INIT-X
                 TARGET-INIT-Y "button-down")
           (new PlaygroundState%
                [toys (list tb)]
                [speed 3]
                [targ-x TARGET-INIT-X]
                [targ-y TARGET-INIT-Y]
                [targ-sel-x TARGET-INIT-X]
                [targ-sel-y TARGET-INIT-Y]
                [targ-sel? true]))
   false)
  (check-equal?
   (equal? (send initial-playground-state after-mouse-event TARGET-INIT-X
                 TARGET-INIT-Y "button-up")
           (new PlaygroundState%
                [toys (list tb)]
                [speed 3]
                [targ-x TARGET-INIT-X]
                [targ-y TARGET-INIT-Y]
                [targ-sel-x TARGET-INIT-X]
                [targ-sel-y TARGET-INIT-Y]
                [targ-sel? false]))
   false)
  (check-equal?
   (equal? (send new-playground after-mouse-event 30
                 30 "drag")
           (new PlaygroundState%
                [toys (list (send THROBBER1 after-drag 30 30))]
                [speed 3]
                [targ-x 30]
                [targ-y 30]
                [targ-sel-x 30]
                [targ-sel-y 30]
                [targ-sel? true]))
   false)
  (check-equal?
   (equal? (send initial-playground-state after-mouse-event 30
                 30 "drag")
           (new PlaygroundState%
                [toys (list (send THROBBER1 after-drag 30 30))]
                [speed 3]
                [targ-x TARGET-INIT-X]
                [targ-y TARGET-INIT-Y]
                [targ-sel-x TARGET-INIT-X]
                [targ-sel-y TARGET-INIT-Y]
                [targ-sel? false]))
   false)
  (check-equal?
   (equal? (send initial-playground-state after-mouse-event TARGET-INIT-X
                 TARGET-INIT-Y "enter")
           (new PlaygroundState%
                [toys (list tb)]
                [speed 3]
                [targ-x TARGET-INIT-X]
                [targ-y TARGET-INIT-Y]
                [targ-sel-x TARGET-INIT-X]
                [targ-sel-y TARGET-INIT-Y]
                [targ-sel? false]))
   false)
  (check-equal?
   (equal? (make-world 3)
           (make-world-state empty 3 TARGET-INIT-X TARGET-INIT-Y
                             SEL-INIT-X SEL-INIT-Y false))
   false)
  (check-equal?
   (send initial-playground-state target-x)
   TARGET-INIT-X)
  (check-equal?
   (send initial-playground-state target-y)
   TARGET-INIT-Y)
  (check-equal?
   (send initial-playground-state target-selected?)
   false)
  (check-equal?
   (send initial-playground-state get-toys)
   (list tb))
  (check-equal?
   (equal? (send initial-playground-state after-key-event "t")
           (new PlaygroundState%
                [toys (cons (new Throbber%
                                 [x TARGET-INIT-X]
                                 [y TARGET-INIT-Y]
                                 [radius 5]
                                 [is-increasing? true]) (list tb))]
                [speed 3]
                [targ-x TARGET-INIT-X]
                [targ-y TARGET-INIT-Y]
                [targ-sel-x TARGET-INIT-X]
                [targ-sel-y TARGET-INIT-Y]
                [targ-sel? false]))
   false)
  (check-equal?
   (equal? (send initial-playground-state after-key-event "s")
           (new PlaygroundState%
                [toys (cons (new Square-toy%
                                 [x TARGET-INIT-X]
                                 [y TARGET-INIT-Y]
                                 [vel 3]
                                 [selected? false]
                                 [saved-mx 0]
                                 [saved-my 0]) (list tb))]
                [speed 3]
                [targ-x TARGET-INIT-X]
                [targ-y TARGET-INIT-Y]
                [targ-sel-x TARGET-INIT-X]
                [targ-sel-y TARGET-INIT-Y]
                [targ-sel? false]))
   false)
  (check-equal?
   (equal? (send initial-playground-state after-key-event "w")
           (new PlaygroundState%
                [toys (cons (new Clock%
                                 [x TARGET-INIT-X]
                                 [y TARGET-INIT-Y]
                                 [val 0]
                                 [selected? false]
                                 [saved-mx 0]
                                 [saved-my 0]) (list tb))]
                [speed 3]
                [targ-x TARGET-INIT-X]
                [targ-y TARGET-INIT-Y]
                [targ-sel-x TARGET-INIT-X]
                [targ-sel-y TARGET-INIT-Y]
                [targ-sel? false]))
   false)
  (check-equal?
   (equal? (send initial-playground-state after-key-event "f")
           (new PlaygroundState%
                [toys (cons (new Football%
                                 [x TARGET-INIT-X]
                                 [y TARGET-INIT-Y]
                                 [scaling-ratio 1]
                                 [selected? false]
                                 [saved-mx SEL-INIT-X]
                                 [saved-my SEL-INIT-Y]) (list tb))]
                [speed 3]
                [targ-x TARGET-INIT-X]
                [targ-y TARGET-INIT-Y]
                [targ-sel-x TARGET-INIT-X]
                [targ-sel-y TARGET-INIT-Y]
                [targ-sel? false]))
   false)
   (check-equal?
   (equal? (send initial-playground-state after-key-event "u")
           (new PlaygroundState%
                [toys (list tb)]
                [speed 3]
                [targ-x TARGET-INIT-X]
                [targ-y TARGET-INIT-Y]
                [targ-sel-x TARGET-INIT-X]
                [targ-sel-y TARGET-INIT-Y]
                [targ-sel? false]))
   false)
  (local ((define p1 (make-playground '() 1)))
  (check-equal? (send p1 to-scene)
                (place-image TARGET-CIRCLE TARGET-INIT-X TARGET-INIT-Y
                             EMPTY-CANVAS))
   (check-equal? (send initial-playground-state to-scene)
                  (place-image (circle 5 "solid" "green") 50 50
                               (place-image TARGET-CIRCLE TARGET-INIT-X
                                            TARGET-INIT-Y EMPTY-CANVAS)))))
  


