;; toys.rkt

;;============================================================================;;
;;                                  GOAL                                      ;;
;;============================================================================;;

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
;===============================================================================

#lang racket

(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

;;============================================================================;;
;;                              INTERFACES                                    ;;
;;============================================================================;;

(require "toy-interfaces.rkt")

;;============================================================================;;
;;                               CLASSES                                      ;;
;;============================================================================;;

(require "square-toy.rkt")
(require "clock.rkt")
(require "throbber.rkt")
(require "football.rkt")

;;============================================================================;;
;;                             PROVIDE STATEMENTS                             ;;
;;============================================================================;;

(provide
 PlaygroundState%
 run
 make-playground
 Toy<%>
 PlaygroundState<%>
 Throbber%
 make-throbber
 Square-toy%
 make-square-toy
 Football%
 make-football
 Clock%
 make-clock)

;;============================================================================;;
;;                                 CLASS                                      ;;
;;============================================================================;;

;; PlaygroundState Class
;; ----------------------

;; A PlaygorundState is a (new PlaygroundState% [world WorldState%] [speed Int]
;;                                              [toys ListofToy<%>] [targ-x Int]
;;                                              [targ-y Int] [targ-sel-x Int]
;;                                              [targ-sel-y Int]
;;                                              [targ-sel? Boolean])

(define PlaygroundState%
  (class* object%(PlaygroundState<%>)

    (init-field world) ;; the world to which the toys will be added
    
    ;; speed is the square speed in (in pixels/tick), at which the square moves
    ;; towards right direction until it bounces back and then it moves towards
    ;; left.
    (init-field speed) ;; Int

    ;; toys is a list of toys in the playground
    (init-field [toys empty])  ;; ListOfToy<%>
    
    ;; targ-x and targ-y are the coordinates of the center of the target
    (init-field [targ-x TARGET-INIT-X]) ;; Int
    (init-field [targ-y TARGET-INIT-Y]) ;; Int
    
    ;; targ-sel-x and targ-sel-y are the coordinates of the mouse event
    ;; of the target
    (init-field [targ-sel-x SEL-INIT-X]) ;; Int
    (init-field [targ-sel-y SEL-INIT-Y]) ;; Int
    
    ;; is the target selected? Default is false
    (init-field [targ-sel? false]) ;; Boolean
    
    (super-new)

    ;; after-tick : -> Void
    ;; GIVEN: no argument 
    ;; EFFECT: updates the current playground state to the state
    ;; after a tick
    ;; EXAMPLE: (send new-playground after-tick) new-playground)
    ;; When the object new-playground calls the after-tick method, there will
    ;; be no changes in any of the fields of the playgroundstate
    (define/public (after-tick) this)
    
    ;; after-button-down : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position  on button down event
    ;; EFFECT: updates the current playground state to the state after button
    ;; down event. If the target is selected, updates the mouse coordinates and
    ;; marks the target as selected.
    ;; EXAMPLE:
    ;; (send new-playground after-button-down TARGET-INIT-X TARGET-INIT-Y)
    ;; When the object new-playground calls the after-button-down method,
    ;; the targ-sel-x and targ-sel-y values are updated with the new mouse
    ;; coordinates.targ-sel? is set to true
    ;; STRATEGY: Cases on whether the mouse position is inside the target
    (define/public (after-button-down mx my)
      (if (in-target? mx my)
          (begin
            (set! targ-sel-x mx)
            (set! targ-sel-y my)
            (set! targ-sel? true))
          this))
        
    ;; after-button-up : Int Int -> Void
    ;; GIVEN: x and y coordinates of the mouse position for the button up event
    ;; EFFECT: updates the state of the playground with the target unselected
    ;; EXAMPLE:
    ;; (send new-playground after-button-up TARGET-INIT-X TARGET-INIT-Y)
    ;; When the object new-playground calls he after-button-up method, the
    ;; target is unselected without any other changes in the playgroundstate
    (define/public (after-button-up mx my)
      (set! targ-sel? false))
    
    ;; after-drag : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position
    ;; EFFECT: updates the current playground state to the state after mouse
    ;; drag event
    ;; EXAMPLE:(send new-playground after-drag 30 30)
    ;; When the object new-playground calls the after-drag, the position
    ;; parameters of the target are updated,the mouse coordinates are updated
    ;; with the current mouse position
    ;; STRATEGY: Cases on whether the target is selected
    (define/public (after-drag mx my)
       (if targ-sel?
           (begin
             (set! targ-x (+ targ-x (- mx targ-sel-x)))
             (set! targ-y (+ targ-y (- my targ-sel-y)))
             (set! targ-sel-x mx)
             (set! targ-sel-y my)
             (set! targ-sel? true))
           this))
    
    ;; in-target? : Int Int -> Boolean
    ;; GIVEN: x and y coordinates of the mouse position
    ;; RETURNS: true iff the mouse event is inside the target
    ;; otherwise returns false
    (define (in-target? x y)
      (<= (+ (sqr (- x targ-x))
             (sqr (- y targ-y)))
          (sqr TARGET-RADIUS)))

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; RETURNS: updates the current playground state to the state that should
    ;; follow the specified key event
    ;; EXAMPLE:
    ;; (send initial-playground-state after-key-event "t")-> adds a throbber toy
    ;; to the list of stateful-widgets in the world and also to the list of toy
    ;; (send initial-playground-state after-key-event "f")-> adds a footbal
    ;; to the list of stateful-widgets in the world and also to the list of toy
    ;; (send initial-playground-state after-key-event "w")-> adds a clock
    ;; to the list of stateful-widgets in the world and also to the list of toy
    ;; (send initial-playground-state after-key-event "s")-> adds a square toy
    ;; to the list of stateful-widgets in the world and also to the list of toy
    ;; STRATEGY: Cases on key event kev
    (define/public (after-key-event kev)
      ;; Constants for key events
      (local ((define KEY_THROBBER "t")
              (define KEY_SQUARE "s")
              (define KEY_CLOCK "w")
              (define KEY_BALL "f"))
      (cond
        [(key=? kev KEY_THROBBER) (add-throbber-to-playground)]
        [(key=? kev KEY_SQUARE) (add-square-toy-to-playground)]
        [(key=? kev KEY_CLOCK) (add-clock-to-playground)]
        [(key=? kev KEY_BALL) (add-football-to-playground)]
        [else this])))
    
    ;; add-throbber-to-playground :  -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: adds a throbber to list of stateful objects
    ;; in the given world and also to the list of toys
    (define (add-throbber-to-playground)
      (begin
        (send world add-stateful-widget (new Throbber%
                                             [x targ-x]
                                             [y targ-y]
                                             [radius 5]
                                             [is-increasing? true]))
        (set! toys (cons (new Throbber%
                   [x targ-x]
                   [y targ-y]
                   [radius 5]
                   [is-increasing? true]) toys))))
    
    ;; add-square-toy-to-playground :  -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: adds a square-toy to list of stateful objects
    ;; in the given world and also to the list of toys
    (define (add-square-toy-to-playground)
       (begin
         (send world add-stateful-widget (new Square-toy%
                                              [x targ-x]
                                              [y targ-y]
                                              [vel speed]))
         (set! toys (cons (new Square-toy%
                    [x targ-x]
                    [y targ-y]
                    [vel speed]) toys))))
    
    ;; add-clock-to-playground :  -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: adds a clock to list of stateful objects
    ;; in the given world and also to the list of toys
    (define (add-clock-to-playground)
       (local ((define CLOCK-INIT-TIME 0))
         (begin
         (send world add-stateful-widget (new Clock%
                                              [x targ-x]
                                              [y targ-y]
                                              [val CLOCK-INIT-TIME]))
         (set! toys (cons (new Clock%
                               [x targ-x]
                               [y targ-y]
                               [val CLOCK-INIT-TIME]) toys)))))
    
    ;; add-football-to-playground :  -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: adds a football to the list of stateful objects
    ;; in the given world and also to the list of toys
    (define (add-football-to-playground)
      (begin
        (send world add-stateful-widget (new Football%
                                             [x targ-x]
                                             [y targ-y]))
        (set! toys (cons (new Football%
                              [x targ-x]
                              [y targ-y]) toys))))
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene that depicts this PlaygroundState<%>
    ;; EXAMPLE:
    ;; (send initial-playground-state add-to-scene EMPTY-CANVAS)
    ;; ->(place-image TARGET-CIRCLE 250 300 EMPTY-CANVAS)
    (define/public (add-to-scene canvas)
      (place-image TARGET-CIRCLE targ-x targ-y EMPTY-CANVAS))
          
    ;; RETURNS: the x or y coordinates of the center of the target
    (define/public (target-x) targ-x) ;; Int
    (define/public (target-y) targ-y) ;; Int
    
     ;; Is the target selected?.Default value is false
    (define/public (target-selected?) targ-sel?)  ;;Boolean

    ;; -> ListOfToy<%>
    (define/public (get-toys) toys) ;; ListOfToy<%>
    ))

;;============================================================================;;
;;                             make-playground                                ;;
;;============================================================================;;
;; -> WorldState<%>
;; the-world is a constant which calls make-world from WidgetWorks.rkt
;; with the Canvas width of 500 and canvas hieght of 600
(define the-world (make-world CANVAS-WIDTH CANVAS-HEIGHT))

;; make-playground: PosInt -> PlaygroundState<%>
;; GIVEN: a square-speed in pixels/tick
;; RETURNS: a playground state with a target, but no toys, and in which any
;; square toys created in the future will travel at the given speed (in
;; pixels/tick).
;; EXAMPLE: (make-playground 1)
(define (make-playground speed)
       (new PlaygroundState%
            [world the-world]
            [toys empty]
            [speed speed]
            [targ-x TARGET-INIT-X]
            [targ-y TARGET-INIT-Y]
            [targ-sel-x SEL-INIT-X]
            [targ-sel-y SEL-INIT-Y]
            [targ-sel? false]))

;;============================================================================;;
;;                                    run                                     ;;
;;============================================================================;;
;; run : PosNum PosInt -> Void
;; GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;;       creates and runs a world in which square toys travel at the given
;;       speed.
;; EFFECT: the final state of the world.
;; EXAMPLE: (run 0.1 1)
;; STRATEGY: Deliver events to the event handler functions by calling the run
;; from StatefulWorld<%> in WidgetWorks.rkt
(define (run frameRate squareSpeed)
  (begin
    (send the-world add-stateful-widget (make-playground squareSpeed))
    (send the-world run frameRate)))

;;============================================================================;;
;;                                   TESTS                                    ;;
;;============================================================================;;

;; TESTS for PlayGroundState<%>
;; -----------------------------
(define THROBBER1 (new Throbber%
                       [x 50]
                       [y 50]
                       [radius 20]
                       [is-increasing? true]
                       [saved-mx SEL-INIT-X]
                       [saved-my SEL-INIT-Y]))
(define tb (make-throbber 50 50))
(define toy-list (list THROBBER1))
(define throbber-after-tick (send THROBBER1 after-tick))
(define toy-list2 (list throbber-after-tick))


(begin-for-test
  (local ((define new-playground
  (new PlaygroundState%
       [world (make-world 500 600)]
       [toys empty]
       [speed 3]
       [targ-x TARGET-INIT-X]
       [targ-y TARGET-INIT-Y]
       [targ-sel-x SEL-INIT-X]
       [targ-sel-y SEL-INIT-Y]
       [targ-sel? true]))
    (define initial-playground-state
       (new PlaygroundState%
       [world (make-world 500 600)]
       [speed 3]))
    (define pg (make-playground 1)))
          
    (check-equal?
     (send new-playground after-tick) new-playground)
    (send new-playground after-button-down TARGET-INIT-X TARGET-INIT-Y)
    (check-equal? (send new-playground target-selected?) true)
    (send new-playground after-button-down 300 400)
    (check-equal? (send new-playground target-x) TARGET-INIT-X)
    (send new-playground after-drag 30 30)
    (check-equal? (send new-playground get-toys) empty)
    (send new-playground after-button-up TARGET-INIT-X TARGET-INIT-Y)
    (check-equal? (send new-playground target-selected?) false)
    (send new-playground after-drag 30 30)
    (check-equal? (send new-playground target-selected?) false)
    (send initial-playground-state after-key-event "t")
    (check-equal? (send initial-playground-state target-x) 250)
    (send initial-playground-state after-key-event "f")
    (check-equal? (send initial-playground-state target-x) 250)
    (send initial-playground-state after-key-event "w")
    (check-equal? (send initial-playground-state target-x) 250)
    (send initial-playground-state after-key-event "s")
    (check-equal? (send initial-playground-state target-x) 250)
    (send initial-playground-state after-key-event "u")
    (check-equal? (send initial-playground-state target-y) 300)
    (check-equal? (send initial-playground-state add-to-scene EMPTY-CANVAS)
                  (place-image TARGET-CIRCLE 250 300 EMPTY-CANVAS))
    (check-equal? (send pg target-selected?) false)))

