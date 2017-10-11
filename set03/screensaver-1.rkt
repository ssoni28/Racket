;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Screensaver-1
;; A screensaver that displays two rectangles that move around a canvas 
;; provided that the rectangles bounce smoothly off the edge of the
;; canvas and bounce perfectly if the rectangle would go past a corner.

;; The user can pause/unpause the screensaver with the space bar.
;; The simulation is paused initially.

;; start with (screensaver speed-of-simulation)
;; examples:
;; (screensaver 0.5)
;; (screensaver 1)
;; (screensaver 0.02)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-rect1
         world-rect2
         world-paused?
         new-rectangle
         rect-x
         rect-y
         rect-vx
         rect-vy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct world (rect1 rect2 paused?))
;; A world is a (make-world Rectangle Rectangle Boolean)
;; rect1 and rect2 are the two rectnagles
;; paused? describes whether or not the world is paused

;; template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-rect1 w)
;;        (world-rect2 w)
;;        (world-paused? w)))

;

(define-struct rect (x y vx vy))
;; A Rect is a (make-rect NonNegInt NonNegInt Int Int)
;; x is the x coordinate of the center of the rectangle
;; y is the y coordiante of the center of the rectangle
;; vx is the velocity of rectangle in x direction
;; vy is the velocity of rectangle in y direction

;; template:
;; rect-fn : Rect -> ??
;; (define (rect-fn r)
;;   (... (rect-x r)
;;        (rect-y r)
;;        (rect-vx r)
;;        (rect-vy r)))
;;
;

;; END OF DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS
(define RECT-IMAGE (rectangle 60 50 "outline" "blue"))
;; initial coordinates of rectangles
(define RECT-1-INITIAL (make-rect 200 100 -12 20))
(define RECT-2-INITIAL (make-rect 200 200 23 -14))
;; dimensions of rectangle
(define RECT-HALF-WIDTH (/ 60 2))
(define RECT-HALF-HEIGHT (/ 50 2))
;; dimensions of the canvas
(define CANVAS-WIDTH 400)
; CANVAS-RIGHT-EDGE is same as CANVAS-WIDTH
(define CANVAS-HEIGHT 300)
; CANVAS-BOTTOM-EDGE is same as CANVAS-HEIGHT
(define CANVAS-TOP-EDGE 0)
(define CANVAS-LEFT-EDGE 0)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Examples for testing
;;Rectangles 
(define RECT-1-BOUNCE (make-rect 116 240 -12 20))
(define RECT-2-BOUNCE (make-rect 361 102 23 -14))
(define RECT-1-LEFT-BOUNCE (make-rect 32 140 -12 -20))
(define RECT-2-TOP-BOUNCE (make-rect 246 32 -23 -14))
(define RECT-1-BOTTOM-BOUNCE (make-rect 212 240 12 20))
(define RECT-2-BOTTOM-BOUNCE (make-rect 223 270 23 14))
(define RECT-1-PERFECT-BOUNCE (make-rect 388 45 12 -20))
(define RECT-2-PERFECT-BOUNCE (make-rect 39 270 -23 14))
(define RECT-1-PERFECT-TOP-BOUNCE (make-rect 388 45 12 -20))
(define RECT-2-PERFECT-BOTTOM-BOUNCE (make-rect 377 270 23 14))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples of worlds, for testing
(define initial-paused-world (make-world RECT-1-INITIAL RECT-2-INITIAL true))
(define initial-unpaused-world (make-world RECT-1-INITIAL RECT-2-INITIAL false))
;; examples KeyEvents for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GIVEN: any value(ignore)
;; RETURNS: the initial paused world having two rectangles centered at
;; positions (200,100) and (200,200) and have velocities of (-12,20) and
;; (23,-14) respectively.
;; EXAMPLES:
;; (initial-world 1) ->
;;     (make-world RECT-1-INITIAL RECT-2-INITIAL true)
;; (initial-world 9000) ->
;;     (make-world RECT-1-INITIAL RECT-2-INITIAL true)
;; (initial-world "hello") ->
;;     (make-world RECT-1-INITIAL RECT-2-INITIAL true)
;; STRATEGY: combine simpler functions
(define (initial-world value)
  (make-world RECT-1-INITIAL RECT-2-INITIAL true))
;; Test:
(begin-for-test
  (check-equal?
   (initial-world 0.5)
   initial-paused-world
   "The initial world is paused having both rectangles at initial coordinates")
  (check-equal?
   (initial-world 2)
   initial-paused-world
   "The initial world is paused having both rectangles at initial coordinates"))

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; STRATEGY : combine simpler functions
(define (new-rectangle x y vx vy)
  (make-rect x y vx vy))
;; Test:
(begin-for-test
  (check-equal?
   (new-rectangle 200 100 -12 20)
   RECT-1-INITIAL
   "make-rectangle with x=100 y=100 xv=-12 yv=20 should create RECT-1-INITIAL"))

;; world-after-tick : WorldState -> WorldState
;; GIVEN : a world state w
;; RETURNS: the world state that should follow the given world
;;           state after a tick
;; STRATEGY: Use template for World on w
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (rectangle-after-tick (world-rect1 w))
       (rectangle-after-tick (world-rect2 w))
       (world-paused? w))))
;; TEST: see tests below.

;; rectangle-after-tick : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of a given rectangle after a tick if it were in
;;          an unpaused world. 
;;
;; STRATEGY : combine simpler functions
(define (rectangle-after-tick r)
  (if (outside-edge-of-canvas? r)
      (bounce-rectangle r)
      (move-rectangle-normal r)))
;; TEST: see tests below.

;; outside-edge-of-canvas? : Rectangle -> Boolean
;; GIVEN: the state of a rectangle
;; RETURNS: true if any one of the edge of the rectangle would go out of
;;          the canvas on next tick. 
;; STRATEGY : combine simpler functions
(define (outside-edge-of-canvas? rect)
  (or
   (or  (is-left-edge-out-next-tick? rect) (is-right-edge-out-next-tick? rect))
   (or  (is-top-edge-out-next-tick? rect) (is-bottom-edge-out-next-tick? rect))))
;; TEST: see tests below.

;; move-rectangle-normal : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of the rectangle on the next tick if the rectangle
;;          would not hit or go past the canvas on the next tick.
;; STRATEGY : use template for Rect on r
(define (move-rectangle-normal rect)
  (make-rect (+ (rect-x rect) (rect-vx rect))
             (+ (rect-y rect) (rect-vy rect))
             (rect-vx rect)
             (rect-vy rect)))
;; TEST: see tests below.

;; bounce-rectangle : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of the rectangle on the next tick if the rectangle
;;          would hit or go past the canvas on the next tick.
;; Examples:
;; 
;; STRATEGY : cases for bounce on the next tick
(define (bounce-rectangle rect)
  (cond
    [(is-left-edge-out-next-tick? rect)(bounce-rect-from-left rect)]
    [(is-right-edge-out-next-tick? rect)(bounce-rect-from-right rect)]
    [(is-top-edge-out-next-tick? rect)(bounce-rect-from-top rect)]
    [(is-bottom-edge-out-next-tick? rect)(bounce-rect-from-bottom rect)]))
;; TEST: see tests below.

;; is-left-edge-out-next-tick? : Rectangle -> Boolean
;; is-right-edge-out-next-tick? : Rectangle -> Boolean
;; is-top-edge-out-next-tick? : Rectangle -> Boolean
;; is-bottom-edge-out-next-tick? : Rectangle -> Boolean
;; GIVEN: the state of a rectangle
;; RETURNS: true if any edge of the rectangle (left,right,top or bottom)
;;          would hit or go past the edge of the canvas on the next tick.
;; STRATEGY : use template for Rect on r
(define (is-left-edge-out-next-tick? rect)
  (<= ( + (- (rect-x rect) RECT-HALF-WIDTH) (rect-vx rect)) CANVAS-LEFT-EDGE))

(define (is-right-edge-out-next-tick? rect)
  (>= ( + (+ (rect-x rect) RECT-HALF-WIDTH) (rect-vx rect)) CANVAS-WIDTH))

(define (is-top-edge-out-next-tick? rect)
  (<= ( + (- (rect-y rect) RECT-HALF-HEIGHT) (rect-vy rect)) CANVAS-TOP-EDGE))

(define (is-bottom-edge-out-next-tick? rect)
  (>= ( + (+ (rect-y rect) RECT-HALF-HEIGHT) (rect-vy rect)) CANVAS-HEIGHT))

;; TEST: see tests below.

;; is-perfect-bounce-left-right? : Rectangle -> Boolean
;; is-perfect-bounce-top-bottom? : Rectangle -> Boolean
;; GIVEN: the state of a rectangle
;; RETURNS: true iff conditions for a perfect bounce are satisfied
;; Examples: 
;; STRATEGY : combine simpler functions
(define (is-perfect-bounce-left-right? rect)
  (or 
   (is-top-edge-out-next-tick? rect)
   (is-bottom-edge-out-next-tick? rect)))

(define (is-perfect-bounce-top-bottom? rect)
  (or 
   (is-left-edge-out-next-tick? rect)
   (is-right-edge-out-next-tick? rect)))
;; TEST: see tests below.

;; bounce-rect-from-left : Rectangle -> Rectangle
;; bounce-rect-from-right : Rectangle -> Rectangle
;; bounce-rect-from-top : Rectangle -> Rectangle
;; bounce-rect-from-bottom : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of the rectangle after a smooth bounce or
;;          a perfect bounce. 
;; STRATEGY : combine simpler functions
(define (bounce-rect-from-left rect) 
  (if (is-perfect-bounce-left-right? rect)
      (move-rectangle-normal (rect-after-perfect-bounce rect))
      (move-rectangle-normal (rect-after-normal-bounce-left-right rect))))

(define (bounce-rect-from-right rect) 
  (if (is-perfect-bounce-left-right? rect)
      (move-rectangle-normal (rect-after-perfect-bounce rect))
      (move-rectangle-normal (rect-after-normal-bounce-left-right rect))))

(define (bounce-rect-from-top rect)
  (if (is-perfect-bounce-top-bottom? rect)
      (move-rectangle-normal (rect-after-perfect-bounce rect))
      (move-rectangle-normal (rect-after-normal-bounce-top-bottom rect))))

(define (bounce-rect-from-bottom rect)
  (if (is-perfect-bounce-top-bottom? rect)
      (move-rectangle-normal (rect-after-perfect-bounce rect))
      (move-rectangle-normal (rect-after-normal-bounce-top-bottom rect))))
;; TEST: see tests below.

;; rect-after-perfect-bounce : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of the rectangle after a perfect bounce
;;          from the corner of the canvas with reversed velocitites.
;; STRATEGY : use template for Rect on rect
(define (rect-after-perfect-bounce rect)
  (make-rect (rect-x rect)
             (rect-y rect)
             (* -1 (rect-vx rect))
             (* -1 (rect-vy rect))))
;; TEST: see tests below.

;; rect-after-normal-bounce-top-bottom : Rectangle -> Rectangle
;; rect-after-normal-bounce-left-right : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of the rectangle after a smooth bounce
;;          from the edge of the canvas.
;; STRATEGY : Use template for Rect on rect
(define (rect-after-normal-bounce-top-bottom rect)
  (make-rect (rect-x rect)
             (rect-y rect)
             (rect-vx rect)
             (* -1 (rect-vy rect))))

(define (rect-after-normal-bounce-left-right rect)
  (make-rect (rect-x rect)
             (rect-y rect)
             (* -1 (rect-vx rect))
             (rect-vy rect)))
;; TEST:
(begin-for-test
  (check-equal?
   (bounce-rect-from-top (new-rectangle 377 50 12 -20))
   (new-rectangle 365 70 -12 20))
  (check-equal?
   (bounce-rect-from-bottom (new-rectangle 376 280 23 14))
   (new-rectangle 353 266 -23 -14)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples for tests
(define initial-unpaused-world-after-tick
  (make-world (rectangle-after-tick RECT-1-INITIAL)
              (rectangle-after-tick RECT-2-INITIAL)
              false))
(define unpaused-world-after-tick
  (make-world (rectangle-after-tick RECT-1-BOUNCE)
              (rectangle-after-tick RECT-2-BOUNCE)
              false))
(define top-bounce-unpaused-world-after-tick
  (make-world (rectangle-after-tick RECT-1-LEFT-BOUNCE)
              (rectangle-after-tick RECT-2-TOP-BOUNCE)
              false))
(define bottom-bounce-unpaused-world-after-tick
  (make-world (rectangle-after-tick RECT-1-BOTTOM-BOUNCE)
              (rectangle-after-tick RECT-2-BOTTOM-BOUNCE)
              false))
(define perfect-bounce-unpaused-world-after-tick
  (make-world (rectangle-after-tick RECT-1-PERFECT-BOUNCE)
              (rectangle-after-tick RECT-2-PERFECT-BOUNCE)
              false))
(define perfect-bounce-bottom-unpaused-world-after-tick
  (make-world (rectangle-after-tick RECT-1-PERFECT-TOP-BOUNCE)
              (rectangle-after-tick RECT-2-PERFECT-BOTTOM-BOUNCE)
              false))
(define unpaused-world
  (make-world
   RECT-1-BOUNCE
   RECT-2-BOUNCE
   false))
(define top-bounce-unpaused-world
  (make-world
   RECT-1-LEFT-BOUNCE
   RECT-2-TOP-BOUNCE
   false))
(define bottom-bounce-unpaused-world
  (make-world
   RECT-1-BOTTOM-BOUNCE
   RECT-2-BOTTOM-BOUNCE
   false))
(define perfect-bounce-unpaused-world
  (make-world
   RECT-1-PERFECT-BOUNCE
   RECT-2-PERFECT-BOUNCE
   false))
(define perfect-bounce-bottom-unpaused-world
  (make-world RECT-1-PERFECT-TOP-BOUNCE
              RECT-2-PERFECT-BOTTOM-BOUNCE
              false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests:
(begin-for-test
  (check-equal? 
    (world-after-tick initial-unpaused-world) 
    initial-unpaused-world-after-tick
    "in unpaused world, the rectangles should move with their velocities
     at next tick and world should still be unpaused")
  
  (check-equal? 
    (world-after-tick initial-paused-world)
    initial-paused-world
    "in paused world, rectangles should be unmoved")

  (check-equal? 
    (world-after-tick unpaused-world)
    unpaused-world-after-tick
    "in unpaused world, if rectangles are going to hit or go past the edge
     of canvas than the rectangles should bounce")
  
  (check-equal? 
    (world-after-tick top-bounce-unpaused-world)
    top-bounce-unpaused-world-after-tick
    "in unpaused world, if rectangles are going to hit or go past the edge
     of canvas than the rectangles should bounce")
  
  (check-equal? 
    (world-after-tick bottom-bounce-unpaused-world)
    bottom-bounce-unpaused-world-after-tick
    "in unpaused world, if rectangles are going to hit or go past the edge
     of canvas than the rectangles should bounce")
  
  (check-equal? 
    (world-after-tick perfect-bounce-unpaused-world)
    perfect-bounce-unpaused-world-after-tick
    "in unpaused world, if rectangles are going to hit or go past the edge
     of canvas than the rectangles should bounce")
  
  (check-equal? 
    (world-after-tick perfect-bounce-bottom-unpaused-world)
    perfect-bounce-bottom-unpaused-world-after-tick
    "in unpaused world, if rectangles are going to hit or go past the edge
     of canvas than the rectangles should bounce"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAPHICS
;;
;; rect-velocity-string : Rectangle ->String
;; GIVEN: the state of a rectangle 
;; RETURNS: a string representing the velocities (vx,vy) of the given
;;          rectangle.
;; STRATEGY: combine simpler functions
(define (rect-velocity-string rect)
  (string-append  
   "("
   (number->string (rect-vx rect))
   ","
   (number->string (rect-vy rect))
   ")"))

;; TEST:
(begin-for-test
  (check-equal?
   (rect-velocity-string RECT-1-INITIAL)
   "(-12,20)"
   "The velocities (vx,vy) of initial rectangle should be (-12,20)")
  (check-equal?
   (rect-velocity-string RECT-2-INITIAL)
   "(23,-14)"
   "The velocities (vx,vy) of initial rectangle should be (23,-14)"))

;; place-rect : Rectangle Scene -> Scene
;; GIVEN: the state of a rectangle and a scene
;; RETURNS: a scene like the given one, but with the given rectangle painted
;; on it.
(define (place-rect rect canvas)
  (place-image RECT-IMAGE
               (rect-x rect)
               (rect-y rect)
               (place-image (text (rect-velocity-string rect) 12 "blue")
                            (rect-x rect)
                            (rect-y rect)
                            canvas)))
;; TEST:
(define IMAGE-INITIAL
  (place-image RECT-IMAGE
               200
               100
               (place-image
                (text (rect-velocity-string RECT-1-INITIAL) 12 "blue")
                200
                100
                EMPTY-CANVAS)))
                            
(begin-for-test
  (check-equal?
   (place-rect RECT-1-INITIAL EMPTY-CANVAS)
   IMAGE-INITIAL
   "(place-rect RECT-1-INITIAL EMPTY-CANVAS) returnes unexpected image"))

;; world-to-scene : World -> Scene
;; GIVEN: a world state
;; RETURNS: a Scene that portrays the given world state.
;; EXAMPLE: (world-to-scene initial-paused-world) should return a canvas with
;; two rectangles, one having center at (200,100) and one at (200,200) with
;; velocities (-12,20) and (23,-14) repectively, initially paused.
;;          
;; STRATEGY: Use template for World on w
(define (world-to-scene w)
  (place-rect (world-rect1 w) (place-rect (world-rect2 w) EMPTY-CANVAS)))

(define image-with-initial-rects 
  (place-rect RECT-1-INITIAL (place-rect RECT-2-INITIAL EMPTY-CANVAS)))
(begin-for-test 
  (check-equal?
   image-with-initial-rects
   (world-to-scene (make-world RECT-1-INITIAL RECT-2-INITIAL false))
   "World to scene with init rectangles should create an image with the init rectangles"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY-EVENTS
;;
;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN: a world state w
;; RETURNS: the WorldState that should follow the given WorldState
;;          after the given key event.
;;          on space, toggle paused?-- ignore all others
;; EXAMPLES:
;; (world-after-key-event initial-paused-world " ")
;;                                     -> initial-unpaused-world 
;; STRATEGY: cases on KeyEvent ke
(define (world-after-key-event w ke)
  (cond
    [(is-pause-key-event? ke)(world-toggle-paused w)]                                         
    [else w]))
;; Tests: See Tests Below

;; helper function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents a pause instruction
;; STRATEGY: combine simpler functions
(define (is-pause-key-event? ke)
  (key=? ke " "))

;; world-toggle-paused : WorldState -> WorldState
;; GIVEN: a world state
;; RETURNS: a world state just like the given one, but with paused? toggled
;; EXAMPLES:
;;   (world-toggle-paused initial-paused-world) -> initial-unpaused-world
;; STRATEGY: use template for World on w
(define (world-toggle-paused w)
  (make-world (world-rect1 w)
              (world-rect2 w)
              (not (world-paused? w))))

;; TESTS:
(begin-for-test
  (check-equal?
    (world-after-key-event initial-paused-world pause-key-event)
    initial-unpaused-world
    "after pause key, a paused world should become unpaused")

  (check-equal?
    (world-after-key-event initial-unpaused-world pause-key-event)
    initial-paused-world
    "after pause key, an unpaused world should become paused")

  (check-equal?
    (world-after-key-event initial-paused-world non-pause-key-event)
    initial-paused-world
    "after a non-pause key, a paused world should be unchanged")

  (check-equal?
    (world-after-key-event initial-unpaused-world non-pause-key-event)
    initial-unpaused-world
    "after a non-pause key, an unpaused world should be unchanged"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCREENSAVER FUNCTION.
;;
;; screensaver : PosReal -> WorldState
;; GIVEN : the speed of the simulation, in seconds/tick
;; EFFECT : runs the simulation, starting with the initial state where
;;          the simulation is paused.
;; RETURNS : the final state of the world
(define (screensaver speed-of-simulation)
  (big-bang (initial-world speed-of-simulation)
            (on-tick world-after-tick speed-of-simulation)
            (on-key  world-after-key-event)
            (on-draw world-to-scene)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;