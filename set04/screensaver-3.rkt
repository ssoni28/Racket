;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screensaver-3.rkt

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         rect-after-key-event
         world-rects
         world-paused?
         new-rectangle
         rect-x
         rect-y
         rect-vx
         rect-vy
         world-after-mouse-event
         rect-after-mouse-event
         rect-selected?)

;; start with (screensaver speed-of-simulation)
;; examples:
;; (screensaver 0.5)
;; (screensaver 1)
;; (screensaver 0.02)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct rect (x y vx vy selected? selected-x selected-y))

;; Interpretation:
;; A Rect is a
;; (make-rect NonNegInt NonNegInt Integer Integer Boolean Integer Integer)
;; x is the x coordinate of the center of the rectangle
;; y is the y coordiante of the center of the rectangle
;; vx is the velocity of rectangle in x direction
;; vy is the velocity of rectangle in y direction
;; selected? sets to true iff current rectangle is selected
;; selected-x is the x coordiante of mouse pointer on button-down mouse event
;; selected-y is the y-coordiante of mouse pointer on button-down mouse event.

;; Template:
;; rect-fn : Rect -> ??
;; (define (rect-fn r)
;;   (... (rect-x r)
;;        (rect-y r)
;;        (rect-vx r)
;;        (rect-vy r)
;;        (rect-selected? r)
;;        (rect-selected-x r)
;;        (rect-selected-y r)))


;; A ListOfRectangle (LOR) is either
;; -- empty
;; -- (cons Rect LOR)

;; Template:
;; lor-fn : LOR -> ??
;; (define (lor-fn lor)
;;   (cond
;;     [(empty? lor) ...]
;;     [else(...
;;            (rect-fn (first lor))
;;            (lor-fn (rest lor)))]))


(define-struct world (rects paused?))

;; Interpretation:
;; A World is a (make-world ListOfRectangle Boolean)
;; rects is a ListOfRectangle
;; paused? describes whether or not the world is paused.

;; Template:
;; world-fn : World -> ??
;;(define (world-fn w)
;;  (... (world-rects w)
;;       (world-paused? w)))

;; KeyEvent is defined in the 2htdp/universe module. A KeyEvent
;; represents key board events. Every KeyEvent is a string, but
;; not all strings are key events. The predicate for comparing
;; two KeyEvent is key=? .

;; MouseEvent is defined in the 2htdp/universe module. A MouseEvent
;; represents mouse events. Every MouseEvent is a string, but
;; not all strings are mouse events. The predicate for comparing
;; two MouseEvent is mouse=? .

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF DATA DEFINITIONS                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mouse pointer
(define INITIAL-MOUSE-POINTER-X 0)
(define INITIAL-MOUSE-POINTER-Y 0)

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
;; CANVAS-RIGHT-EDGE is same as CANVAS-WIDTH
(define CANVAS-HEIGHT 300)
;; CANVAS-BOTTOM-EDGE is same as CANVAS-HEIGHT
(define CANVAS-CENTER-X 200)
(define CANVAS-CENTER-Y 150)
(define CANVAS-TOP-EDGE 0)
(define CANVAS-LEFT-EDGE 0)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define TOP-TANGENT 25)
(define BOTTOM-TANGENT 275)
(define LEFT-TANGENT 30)
(define RIGHT-TANGENT 370)
(define REVERSE -1)

;; images of rectangle
(define RECT-IMAGE-BLUE (rectangle 60 50 "outline" "blue"))
(define RECT-IMAGE-RED (rectangle 60 50 "outline" "red"))

;; image of selection circle
(define CIRCLE-IMAGE (circle 5 "outline" "red"))

;; initial velocity
(define INITIAL-VELOCITY-X 0)
(define INITIAL-VELOCITY-Y 0)

;; dimension of the rectangle
(define RECT-HALF-WIDTH (/ 60 2))
(define RECT-HALF-HEIGHT (/ 50 2))

;; text font size
(define FONT-SIZE 12)

;; color of text and rectangle
(define COLOR-RED "red")
(define COLOR-BLUE "blue")

;; increase in speed on arrow keyvent in pixels per sec
(define INCREASE-SPEED 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF CONSTANTS                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES FOR TEST                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rect-1
  (make-rect CANVAS-CENTER-X
             CANVAS-CENTER-Y
             INITIAL-VELOCITY-X
             INITIAL-VELOCITY-Y
             true
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define rect-2
  (make-rect 200
             100
             -12
             20
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define initial-world-state
  (make-world empty
              true))

(define sample-list
  (list rect-1 rect-2))

(define sample-world
  (make-world sample-list
              true))

(define sample-world-unpaused
  (make-world sample-list
              false))


(define rect-1-after-mouse-event
  (make-rect CANVAS-CENTER-X
             CANVAS-CENTER-Y
             INITIAL-VELOCITY-X
             INITIAL-VELOCITY-Y
             true
             CANVAS-CENTER-X
             CANVAS-CENTER-Y))
(define rect-1-after-button-up
  (make-rect CANVAS-CENTER-X
             CANVAS-CENTER-Y
             INITIAL-VELOCITY-X
             INITIAL-VELOCITY-Y
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define rect-2-after-tick
  (make-rect 188
             120
             -12
             20
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))
(define rect-1-after-up-key
  (make-rect CANVAS-CENTER-X
             CANVAS-CENTER-Y
             INITIAL-VELOCITY-X
             (- INITIAL-VELOCITY-Y INCREASE-SPEED)
             true
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define rect-1-after-down-key
  (make-rect CANVAS-CENTER-X
             CANVAS-CENTER-Y
             INITIAL-VELOCITY-X
             (+ INITIAL-VELOCITY-Y INCREASE-SPEED)
             true
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define rect-1-after-left-key
  (make-rect CANVAS-CENTER-X
             CANVAS-CENTER-Y
             (- INITIAL-VELOCITY-X INCREASE-SPEED)
             INITIAL-VELOCITY-Y 
             true
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define rect-1-after-right-key
  (make-rect CANVAS-CENTER-X
             CANVAS-CENTER-Y
             (+ INITIAL-VELOCITY-X INCREASE-SPEED)
             INITIAL-VELOCITY-Y 
             true
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))


(define sample-list-after-tick
  (list rect-1 rect-2-after-tick))
(define sample-new-world
  (make-world (cons rect-1-after-button-up
                    sample-list)
              true))


(define sample-unpaused-world-after-tick
  (make-world sample-list-after-tick
              false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF EXAMPLES                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCREENSAVER FUNCTION                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial
;;         state as specified in the problem set.
;; RETURNS: the final state of the world
;; STRATEGY: combine simpler functions
(define (screensaver speed-of-simulation)
  (big-bang (initial-world speed-of-simulation)
            (on-tick world-after-tick speed-of-simulation)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial-world                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial paused world having no rectangles
;; EXAMPLES: (initial-world 1) -> initial-world-state
;; STRATEGY: combine simpler functions
(define (initial-world value)
  (make-world empty true))

;; TEST:
(begin-for-test
  (check-equal?
   (initial-world 1)
   initial-world-state
   "The initial paused world should be returned having
    no rectangles")
  (check-equal?
   (initial-world "anb")
   initial-world-state
   "The initial paused world should be returned having
    no rectangles"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : WorldState -> Scene
;; GIVEN: a world state
;; RETURNS: a Scene that portrays the given world state.
;; EXAMPLE: (world-to-scene initial-world-state) -> initial-scene
;; STRATEGY: Use template for World on w
(define (world-to-scene w)
  (place-selection-circle (world-rects w)
                          (place-rects
                           (world-rects w)
                           EMPTY-CANVAS)))
;; TEST : see tests below

;; place-selection-circle : LOR Scene -> Scene
;; GIVEN: a list of rectangles and a scene
;; RETURNS: a Scene that portrays the given
;;          scene with a selection circle on
;;          button-down mouse event on given
;;          rectangles.      
;; STRATEGY: Use template for ListOfRectangle on lor
(define (place-selection-circle lor canvas)
  (cond
    [(empty? lor) canvas]
    [else (if (place-selection-circle-condition? (first lor))
              (add-circle-to-scene (first lor) canvas)
              (place-selection-circle (rest lor) canvas))]))

;; TEST: see test below

;; place-selection-circle-condition? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: true iff the given rectangle is selected and
;;          the mouse button-down is inside the rectangle
;; STRATEGY: Use template for Rect on rect
(define (place-selection-circle-condition? rect)
  (and (rect-selected? rect)
       (in-rectangle? rect
                      (rect-selected-x rect)
                      (rect-selected-y rect))))

;; TEST: see test below

;; in-rectangle? : Rectangle Integer Integer -> Boolean
;; GIVEN: a rectangle and mouse button-down coordinates
;; RETURNS: true iff the button-down is inside the rectangle
;; STRATEGY: Use template for Rect on r
(define (in-rectangle? r mx my)
  (and
   (and ( >= (+ (rect-x r) RECT-HALF-WIDTH) mx)
        ( <= (- (rect-x r) RECT-HALF-WIDTH) mx))
   (and ( >= (+ (rect-y r) RECT-HALF-HEIGHT) my)
        ( <= (- (rect-y r) RECT-HALF-HEIGHT) my))))

;; add-circle-to-scene : Rectangle Scene -> Scene
;; GIVEN: a scene and a rectangle
;; RETURNS: a Scene that portrays the given scene
;;          with a circle.       
;; STRATEGY: Use template for Rect on rect
(define (add-circle-to-scene rect canvas)
  (place-image CIRCLE-IMAGE
               (rect-selected-x rect)
               (rect-selected-y rect)
               canvas))

;; place-rects : ListOfRectangle Scene -> Scene
;; GIVEN: a list of recatngles and a scene
;; RETURNS: a scene like the given one, but with
;;          the given list of rectangles painted
;;          on it.
;; STRATEGY: Use template for LOR on lor
(define (place-rects lor canvas) 
  (cond
    [(empty? lor) canvas]
    [else (place-rect (first lor)
                      (place-rects (rest lor)
                                   canvas))]))

;; place-rect : Rectangle Scene -> Scene
;; GIVEN: the state of a rectangle and a scene
;; RETURNS: a scene like the given one, but with
;;          the given rectangle painted on it.
;; STRATEGY: Use template for Rect on rect
(define (place-rect rect canvas)
  (place-image (get-rect-image rect)
               (rect-x rect)
               (rect-y rect)
               (place-image (get-rect-text rect)
                            (rect-x rect)
                            (rect-y rect)
                            canvas)))

;; get-rect-image : Rectangle  -> Image
;; GIVEN: the state of a rectangle 
;; RETURNS: an image of the rectangle
;; WHERE: image is Red if rectangle is
;;        selected otherwise it is blue
;; STRATEGY: Use template for Rect on rect
(define (get-rect-image rect)
  (if (rect-selected? rect)
      RECT-IMAGE-RED
      RECT-IMAGE-BLUE))

;; get-rect-text : Rectangle  -> Image
;; GIVEN: the state of a rectangle 
;; RETURNS: the text displaying the velocity
;;          string (vx,vy) of given rectangle
;; WHERE: text is Red if rectangle is selected
;;        otherwise it is blue
;; STRATEGY: Use template for Rect on rect
(define (get-rect-text rect)
  (if (rect-selected? rect)
      (text (rect-velocity-string rect) FONT-SIZE COLOR-RED)
      (text (rect-velocity-string rect) FONT-SIZE COLOR-BLUE)))

;; rect-velocity-string : Rectangle -> String
;; GIVEN: the state of a rectangle 
;; RETURNS: a string representing the velocities
;;          (vx,vy) of the given rectangle.
;; STRATEGY: Use template for Rect on rect
(define (rect-velocity-string rect)
  (string-append 
   "("
   (number->string (rect-vx rect))
   ","
   (number->string (rect-vy rect))
   ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES FOR TEST                                                          ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define initial-scene
  (place-selection-circle (world-rects initial-world-state)
                          (place-rects (world-rects initial-world-state)
                                       EMPTY-CANVAS)))
(define sample-scene
  (place-selection-circle (world-rects sample-world)
                          (place-rects (world-rects sample-world)
                                       EMPTY-CANVAS)))

(define selected-rect
  (make-rect CANVAS-CENTER-X
             CANVAS-CENTER-Y
             INITIAL-VELOCITY-X
             INITIAL-VELOCITY-X
             true
             CANVAS-CENTER-X
             CANVAS-CENTER-Y))

(define unselected-rect
  (make-rect CANVAS-CENTER-X
             CANVAS-CENTER-Y
             -2
             -2
             false
             CANVAS-CENTER-X
             CANVAS-CENTER-Y))

(define unselected-rect-button-up
  (make-rect CANVAS-CENTER-X
             CANVAS-CENTER-Y
             INITIAL-VELOCITY-X
             INITIAL-VELOCITY-Y
             false
             0
             0))


(define unselected-rect-after-tick
  (make-rect (+ CANVAS-CENTER-X -2)
             (+ CANVAS-CENTER-Y -2)
             -2
             -2
             false
             CANVAS-CENTER-X
             CANVAS-CENTER-Y))


(define selected-list
  (list rect-1 rect-2 selected-rect))

(define selected-list-after-tick
  (list rect-1
        rect-2-after-tick
        selected-rect))

(define unselected-list
  (list rect-1 rect-2 unselected-rect))


(define unselected-list-after-tick
  (list rect-1
        rect-2-after-tick
        unselected-rect-after-tick))

(define selected-world-state
  (make-world selected-list true))

(define selected-scene
  (place-selection-circle (world-rects selected-world-state)
                          (place-rects (world-rects selected-world-state)
                                       EMPTY-CANVAS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST:

(begin-for-test
  (check-equal?
   (world-to-scene initial-world-state)
   initial-scene
   "Image of initial-world-state should be created on the given canvas")
  (check-equal?
   (world-to-scene sample-world)
   sample-scene
   "Image of sample-world should be created on the given canvas")
  (check-equal?
   (world-to-scene selected-world-state)
   selected-scene
   "Image of selected-world-state should be created on the given canvas"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-tick                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : WorldState -> WorldState
;; GIVEN: a world state
;; RETURNS: the world state that should follow
;;          the given world state after a tick.
;; STRATEGY: Use template for World on w
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (world-after-tick-helper w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES FOR TESTING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sample-unpaused-world
  (make-world sample-list false))

(define selected-unpaused-world-state
  (make-world selected-list false))

(define selected-unpaused-world-state-after-tick
  (make-world selected-list-after-tick false))

(define unselected-unpaused-world-state
  (make-world unselected-list false))

(define unselected-unpaused-world-state-after-tick
  (make-world unselected-list-after-tick false))

;; TEST:
(begin-for-test
  (check-equal?
   (world-after-tick sample-world)
   sample-world
   "The given paused world should be paused after tick")
  (check-equal?
   (world-after-tick sample-unpaused-world)
   sample-unpaused-world-after-tick
   "The given unpaused world should be unpaused after tick
    with given rectangles moved by given velocities")
  (check-equal?
   (world-after-tick selected-unpaused-world-state)
   selected-unpaused-world-state-after-tick
   "The given selected unpaused world should be unpaused
    and selected after tick having selected rectangles
    not moved and other rectangles moved by their velocity
    after tick")
  (check-equal?
   (world-after-tick unselected-unpaused-world-state)
   unselected-unpaused-world-state-after-tick
   "The given unselected unpaused world should be have all
    rectangles moved by their given velocities after tick"))

;; world-after-tick-helper : WorldState -> WorldState
;; GIVEN : a world state w
;; RETURNS: the state of the world that should follow 
;;          the given world state after a tick without
;;          being paused 
;; STRATEGY: Use template for World on w
(define (world-after-tick-helper w)
  (make-world
   (rectangle-after-tick (world-rects w))
   (world-paused? w)))

;; rectangle-after-tick : ListOfRectangle -> ListOfRectangle
;; GIVEN: a list of rectangles
;; RETURNS: the state of a given list of rectangles after a 
;;          tick if they were in an unpaused world. 
;; STRATEGY : Use template for ListOfRectangle on lor
(define (rectangle-after-tick lor)
  (cond
    [(empty? lor) empty]
    [else (if (rect-selected? (first lor))
              (cons (first lor) (rectangle-after-tick (rest lor)))
              (cons (rectangle-after-tick-helper (first lor))
                    (rectangle-after-tick (rest lor))))]))

;; rectangle-after-tick-helper : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of a given rectangle after a 
;;          tick if it were in an unpaused world and
;;          is unselected.
;; STRATEGY: combine simpler functions
(define (rectangle-after-tick-helper r)
  (if (outside-edge-of-canvas? r)
      (bounce-rectangle r)
      (move-rectangle-normal r)))

;; outside-edge-of-canvas? : Rectangle -> Boolean
;; GIVEN: the state of a rectangle
;; RETURNS: true if any one of the edge of the
;;          rectangle would go out of the canvas
;;          on next tick.
;; STRATEGY : combine simpler functions
(define (outside-edge-of-canvas? rect)
  (or
   (or  (is-left-edge-out-next-tick? rect)
        (is-right-edge-out-next-tick? rect))
   (or  (is-top-edge-out-next-tick? rect)
        (is-bottom-edge-out-next-tick? rect))))

;; move-rectangle-normal : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of the rectangle on the next
;;          tick if the rectangle would not hit or
;;          go past the canvas on the next tick.
;; STRATEGY : Use template for Rect on r
(define (move-rectangle-normal rect)
  (make-rect (+ (rect-x rect) (rect-vx rect))
             (+ (rect-y rect) (rect-vy rect))
             (rect-vx rect)
             (rect-vy rect)
             (rect-selected? rect)
             (rect-selected-x rect)
             (rect-selected-y rect)))

;; bounce-rectangle : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of the rectangle on the next
;;          tick if the rectangle would hit or go
;;          past the canvas on the next tick.
;; STRATEGY : cases for bounce on the next tick
(define (bounce-rectangle rect)
  (cond
    [(is-left-edge-out-next-tick? rect)(bounce-rect-from-left rect)]
    [(is-right-edge-out-next-tick? rect)(bounce-rect-from-right rect)]
    [(is-top-edge-out-next-tick? rect)(bounce-rect-from-top rect)]
    [(is-bottom-edge-out-next-tick? rect)(bounce-rect-from-bottom rect)]))

;; is-left-edge-out-next-tick? : Rectangle -> Boolean
;; is-right-edge-out-next-tick? : Rectangle -> Boolean
;; is-top-edge-out-next-tick? : Rectangle -> Boolean
;; is-bottom-edge-out-next-tick? : Rectangle -> Boolean
;; GIVEN: the state of a rectangle
;; RETURNS: true if any edge of the rectangle (left,right,
;;          top or bottom) would hit or go past the edge
;;          of the canvas on the next tick.
;; STRATEGY : Use template for Rect on r
(define (is-left-edge-out-next-tick? rect)
  (<= ( + (- (rect-x rect) RECT-HALF-WIDTH)
          (rect-vx rect)) CANVAS-LEFT-EDGE))

(define (is-right-edge-out-next-tick? rect)
  (>= ( + (+ (rect-x rect) RECT-HALF-WIDTH)
          (rect-vx rect)) CANVAS-WIDTH))

(define (is-top-edge-out-next-tick? rect)
  (<= ( + (- (rect-y rect) RECT-HALF-HEIGHT)
          (rect-vy rect)) CANVAS-TOP-EDGE))

(define (is-bottom-edge-out-next-tick? rect)
  (>= ( + (+ (rect-y rect) RECT-HALF-HEIGHT)
          (rect-vy rect)) CANVAS-HEIGHT))


;; bounce-rect-from-left : Rectangle -> Rectangle
;; bounce-rect-from-right : Rectangle -> Rectangle
;; bounce-rect-from-top : Rectangle -> Rectangle
;; bounce-rect-from-bottom : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of the rectangle after a smooth
;;           bounce or a perfect bounce.
;; STRATEGY : cases for bounce on smooth or perfect bounce
(define (bounce-rect-from-left rect) 
  (cond
    [(is-top-edge-out-next-tick? rect)
     (rect-after-perfect-bounce-from-left-top rect)]
    [(is-bottom-edge-out-next-tick? rect)
     (rect-after-perfect-bounce-from-left-bottom rect)]
    [else (rect-after-normal-bounce-left rect)]))

(define (bounce-rect-from-right rect)
  (cond
    [(is-top-edge-out-next-tick? rect)
     (rect-after-perfect-bounce-from-right-top rect)]
    [(is-bottom-edge-out-next-tick? rect)
     (rect-after-perfect-bounce-from-right-bottom rect)]
    [else (rect-after-normal-bounce-right rect)]))

(define (bounce-rect-from-top rect)
  (cond
    [(is-left-edge-out-next-tick? rect)
     (rect-after-perfect-bounce-from-left-top rect)]
    [(is-right-edge-out-next-tick? rect)
     (rect-after-perfect-bounce-from-right-top rect)]
    [else (rect-after-normal-bounce-top rect)]))

(define (bounce-rect-from-bottom rect)
  (cond
    [(is-left-edge-out-next-tick? rect)
     (rect-after-perfect-bounce-from-left-bottom rect)]
    [(is-right-edge-out-next-tick? rect)
     (rect-after-perfect-bounce-from-right-bottom rect)]
    [else (rect-after-normal-bounce-bottom rect)]))

;; TEST:
(begin-for-test
  (check-equal?
   (bounce-rect-from-top (make-rect -1 -1 -1 -1 false 0 0))
   (rect-after-perfect-bounce-from-left-top
    (make-rect -1 -1 -1 -1 false 0 0))
   "Rectangle after perfect bounce from left top should be returned")
  (check-equal?
   (bounce-rect-from-top (make-rect 401 -1 1 -1 false 0 0))
   (rect-after-perfect-bounce-from-right-top
    (make-rect 401 -1 1 -1 false 0 0))
   "Rectangle after perfect bounce from right top should be returned")
  (check-equal?
   (bounce-rect-from-bottom (make-rect -1 301 -1 1 false 0 0))
   (rect-after-perfect-bounce-from-left-bottom
    (make-rect -1 301 -1 1 false 0 0))
   "Rectangle after perfect bounce from left bottom")
  (check-equal?
   (bounce-rect-from-bottom (make-rect 401 301 1 1 false 0 0))
   (rect-after-perfect-bounce-from-right-bottom
    (make-rect 401 301 1 1 false 0 0))
   "Rectangle after perfect bounce from right bottom"))

;; rect-after-perfect-bounce-from-left-top : Rectangle -> Rectangle
;; rect-after-perfect-bounce-from-left-bottom : Rectangle -> Rectangle
;; rect-after-perfect-bounce-from-right-top : Rectangle -> Rectangle
;; rect-after-perfect-bounce-from-right-bottom : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of the rectangle after a perfect bounce
;;          from the corners of the canvas with reversed velocitites.
;; STRATEGY : Use template for Rect on rect
(define (rect-after-perfect-bounce-from-left-top rect)
  (make-rect LEFT-TANGENT
             TOP-TANGENT
             (* REVERSE (rect-vx rect))
             (* REVERSE (rect-vy rect))
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define (rect-after-perfect-bounce-from-left-bottom rect)
  (make-rect LEFT-TANGENT
             BOTTOM-TANGENT
             (* REVERSE (rect-vx rect))
             (* REVERSE (rect-vy rect))
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define (rect-after-perfect-bounce-from-right-top rect)
  (make-rect RIGHT-TANGENT
             TOP-TANGENT
             (* REVERSE (rect-vx rect))
             (* REVERSE (rect-vy rect))
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define (rect-after-perfect-bounce-from-right-bottom rect)
  (make-rect RIGHT-TANGENT
             BOTTOM-TANGENT
             (* REVERSE (rect-vx rect))
             (* REVERSE (rect-vy rect))
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

;; rect-after-normal-bounce-top : Rectangle -> Rectangle
;; rect-after-normal-bounce-bottom : Rectangle -> Rectangle
;; rect-after-normal-bounce-left : Rectangle -> Rectangle
;; rect-after-normal-bounce-right : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of the rectangle after a smooth bounce
;;          from the edges of the canvas.
;; STRATEGY : Use template for Rect on rect
(define (rect-after-normal-bounce-top rect)
  (make-rect (+ (rect-x rect) (rect-vx rect))
             TOP-TANGENT
             (rect-vx rect)
             (* REVERSE (rect-vy rect))
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define (rect-after-normal-bounce-bottom rect)
  (make-rect (+ (rect-x rect) (rect-vx rect))
             BOTTOM-TANGENT
             (rect-vx rect)
             (* REVERSE (rect-vy rect))
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))


(define (rect-after-normal-bounce-left rect)
  (make-rect LEFT-TANGENT
             (+ (rect-y rect) (rect-vy rect))
             (* REVERSE (rect-vx rect))
             (rect-vy rect)
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

(define (rect-after-normal-bounce-right rect)
  (make-rect RIGHT-TANGENT
             (+ (rect-y rect) (rect-vy rect))
             (* REVERSE (rect-vx rect))
             (rect-vy rect)
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

;; EXAMPLES FOR TEST:
(define rect-bounce-left (make-rect 32 150 -4 4 false 0 0))
(define rect-bounce-right (make-rect 369 150 4 4 false 0 0))
(define rect-bounce-top (make-rect 32 26 -4 -4 false 0 0))
(define rect-bounce-bottom (make-rect 32 269 -4 4 false 0 0))
(define rect-bounce-top-1 (make-rect 135 28 -4 -4 false 0 0))
(define rect-bounce-bottom-1 (make-rect 202 273 4 4 false 0 0))
(define rect-bounce-right-bottom (make-rect 367 273 4 4 false 0 0))
(define rect-bounce-right-top (make-rect 367 10 20 -14 false 0 0))
(define rect-bounce-left-bottom (make-rect 10 273 4 4 false 0 0))

(define bounce-list (list rect-bounce-left rect-bounce-right
                          rect-bounce-top rect-bounce-bottom
                          rect-bounce-top-1 rect-bounce-bottom-1
                          rect-bounce-right-bottom rect-bounce-right-top
                          rect-bounce-left-bottom))

(define bounce-world (make-world bounce-list false))
(define bounce-world-after-tick (make-world
                                 (rectangle-after-tick bounce-list)
                                 false))

;; TEST:
(begin-for-test
  (check-equal?
   (world-after-tick bounce-world)
   bounce-world-after-tick
   "The world after smooth and perfect bounces"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-key-event                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; help functions for key event

;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents
;;           a pause instruction
;; STRATEGY: combine simpler functions
(define (is-pause-key-event? ke)
  (key=? ke " "))

;; is-n-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents
;;           a n key event
;; STRATEGY: combine simpler functions
(define (is-n-key-event? ke)
  (key=? ke "n"))

;; is-up-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents
;;           an up arrow key event
;; STRATEGY: combine simpler functions
(define (is-up-key-event? ke)
  (key=? ke "up"))

;; is-down-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents
;;           a down arrow key event
;; STRATEGY: combine simpler functions
(define (is-down-key-event? ke)
  (key=? ke "down"))

;; is-left-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents
;;           a left arrow key event
;; STRATEGY: combine simpler functions
(define (is-left-key-event? ke)
  (key=? ke "left"))

;; is-right-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents
;;           a right arrow key event
;; STRATEGY: combine simpler functions
(define (is-right-key-event? ke)
  (key=? ke "right"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN: a world state and a KeyEvent
;; RETURNS: the WorldState that should follow the given 
;;          worldstate after the given KeyEvent
;; STRATEGY: cases on KeyEvent ke
(define (world-after-key-event w ke)
  (cond
    [(is-pause-key-event? ke) (world-toggle-paused w)]
    [(is-n-key-event? ke) (new-world w)]
    [(is-up-key-event? ke) (world-after-arrow-key-event w ke)]
    [(is-down-key-event? ke) (world-after-arrow-key-event w ke)]
    [(is-left-key-event? ke) (world-after-arrow-key-event w ke)]
    [(is-right-key-event? ke) (world-after-arrow-key-event w ke)]
    [else w]))

;; TEST:
(begin-for-test
  (check-equal?
   (world-after-key-event sample-world " ")
   sample-unpaused-world
   "The world after pause key event should be unpaused")
  (check-equal?
   (world-after-key-event sample-world "q")
   sample-world
   "The world after q key event should be unchanged")
  (check-equal?
   (world-after-key-event sample-world "n")
   sample-new-world
   "The world after n key event should have a new rectangle
    added at the center")
  (check-equal?
   (world-after-key-event sample-world "up")
   (make-world (list rect-1-after-up-key rect-2) true)
   "The world after up key event should increase the
    velocity by 2 ticks per sec")
  (check-equal?
   (world-after-key-event sample-world "down")
   (make-world (list rect-1-after-down-key rect-2) true)
   "The world after down key event should increase the
    velocity by 2 ticks per sec")
  (check-equal?
   (world-after-key-event sample-world "left")
   (make-world (list rect-1-after-left-key rect-2) true)
   "The world after left key event should increase the
    velocity by 2 ticks per sec")
  (check-equal?
   (world-after-key-event sample-world "right")
   (make-world (list rect-1-after-right-key rect-2) true)
   "The world after right key event should increase the
    velocity by 2 ticks per sec"))

;; world-toggle-paused : WorldState -> WorldState
;; GIVEN: a world state
;; RETURNS: a world state just like the given one,
;;          but with paused? toggled
;; STRATEGY: Use template for World on w
(define (world-toggle-paused w)
  (make-world (world-rects w) (not (world-paused? w))))

;; new-world : WorldState -> WorldState
;; GIVEN: a world state
;; RETURNS: a world state just like the given one,
;;          but with added rectangle on n KeyEvent
;; STRATEGY: Use template for World on w
(define (new-world w)
  (make-world (add-new-rectangle (world-rects w))
              (world-paused? w)))

;; add-new-rectangle : ListOfRectangle -> ListOfRectangle
;; GIVEN: a list of rectangles
;; RETURNS: the given list after adding a new rectangle, 
;;          having initial state, in it.
;; STRATEGY: combine simple functions
(define (add-new-rectangle lor)
  (cons (new-rectangle CANVAS-CENTER-X
                       CANVAS-CENTER-Y
                       INITIAL-VELOCITY-X
                       INITIAL-VELOCITY-Y)
        lor))

;; world-after-arrow-key-event : World KeyEvent -> World
;; GIVEN: a world state and a KeyEvent
;; RETURNS: the WorldState that should follow the given 
;;          worldstate after the given arrow keyevent
;; STRATEGY: Use template for World on w
(define (world-after-arrow-key-event w ke)
  (make-world (new-list (world-rects w) ke) (world-paused? w)))

;; new-list : ListOfRectangle KeyEvent -> ListOfRectangle
;; GIVEN: a list of rectangles and a KeyEvent
;; RETURNS: the state of given list of rectangles after the
;;          given KeyEvent
;; STRATEGY: Use template for ListOfRectangle on lor
(define (new-list lor ke)
  (cond
    [(empty? lor) empty]
    [else (if (rect-selected? (first lor))
              (cons (rect-after-key-event (first lor) ke)
                    (new-list (rest lor) ke))
              (cons (first lor)
                    (new-list (rest lor) ke)))])) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-after-key-event                                                       ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-after-key-event : Rectangle KeyEvent -> Rectangle
;; GIVEN: the state of a rectangle and a KeyEvent
;; RETURNS: the state of the rectangle that should follow 
;;          the given rectangle after the given key event.
;; STRATEGY: cases on KeyEvent ke
(define (rect-after-key-event rect ke)
  (cond
    [(is-up-key-event? ke) (rect-after-up-key-event rect)]
    [(is-down-key-event? ke) (rect-after-down-key-event rect)]
    [(is-left-key-event? ke) (rect-after-left-key-event rect)]
    [(is-right-key-event? ke) (rect-after-right-key-event rect)]))

;; rect-after-up-key-event : Rectangle -> Rectangle
;; rect-after-down-key-event : Rectangle -> Rectangle
;; rect-after-left-key-event : Rectangle -> Rectangle
;; rect-after-right-key-event : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of given rectangle after arrow
;;          KeyEvent.
;; STRATEGY: Use template for Rect on rect
(define (rect-after-up-key-event rect)
  (make-rect (rect-x rect)
             (rect-y rect)
             (rect-vx rect)
             (- (rect-vy rect) INCREASE-SPEED)
             (rect-selected? rect)
             (rect-selected-x rect)
             (rect-selected-y rect)))

(define (rect-after-down-key-event rect)
  (make-rect (rect-x rect)
             (rect-y rect)
             (rect-vx rect)
             (+ (rect-vy rect) INCREASE-SPEED)
             (rect-selected? rect)
             (rect-selected-x rect)
             (rect-selected-y rect)))

(define (rect-after-left-key-event rect)
  (make-rect (rect-x rect)
             (rect-y rect)
             (- (rect-vx rect) INCREASE-SPEED)
             (rect-vy rect)
             (rect-selected? rect)
             (rect-selected-x rect)
             (rect-selected-y rect)))

(define (rect-after-right-key-event rect)
  (make-rect (rect-x rect)
             (rect-y rect)
             (+ (rect-vx rect) INCREASE-SPEED)
             (rect-vy rect)
             (rect-selected? rect)
             (rect-selected-x rect)
             (rect-selected-y rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-rectangle                                                              ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx
;;        and vy
;; RETURNS: a rectangle centered at (x,y), which will travel 
;;          with velocity (vx, vy).
;; STRATEGY: combine simpler functions
(define (new-rectangle x y vx vy)
  (make-rect x y vx vy false INITIAL-MOUSE-POINTER-X INITIAL-MOUSE-POINTER-X))
;; TEST:
(begin-for-test
  (check-equal?
   (new-rectangle CANVAS-CENTER-X CANVAS-CENTER-Y
                  INITIAL-VELOCITY-X INITIAL-VELOCITY-Y)
   rect-1-after-button-up
   "The new rectangle should have given center coordiantes and velocity")
  (check-equal?
   (new-rectangle 200 100 -12 20)
   rect-2
   "The new rectangle should have given center coordiantes and velocity"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-mouse-event                                                    ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the world that should follow the given world after the given
;;          mouse event.
;; STRATEGY: Use template for World on w
(define (world-after-mouse-event w mx my mev)
  (make-world
   (rects-after-mouse-event (world-rects w) mx my mev)
   (world-paused? w)))
;; TEST: see test below

;; rects-after-mouse-event :
;;  ListOfRectangle Int Int MouseEvent -> ListOfRectangle
;; GIVEN: a LOR, the x and y coordinates of a mouse event,
;;        and the mouse event
;; RETURN: a list of recatngles after given mouse event
;; STRATEGY: Use template for LOR on lor
(define (rects-after-mouse-event lor mx my mev)
  (cond
    [(empty? lor) empty]
    [else (cons
           (rect-after-mouse-event (first lor) mx my mev)
           (rects-after-mouse-event (rest lor) mx my mev))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rect-after-mouse-event                                                     ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-after-mouse-event : Rectangle Int Int MouseEvent -> Rectangle
;; GIVEN: A rectangle, the x- and y-coordinates of a mouse event, and 
;;        the mouse event
;; RETURNS: the rectangle that should follow the given rectangle after
;;          the given mouse event
;; STRATEGY: cases on MouseEvent mev
(define (rect-after-mouse-event rect mx my mev)
  (cond
    [(mouse=? mev "button-down") (get-rectangle-after-button-down rect mx my)]
    [(mouse=? mev "drag") (rectangle-after-drag rect mx my)]
    [(mouse=? mev "button-up") (rectangle-after-button-up rect)]
    [else rect]))
;; TEST: see test below

;; get-rectangle-after-button-down : Rectangle Integer Integer -> Rectangle
;; GIVEN: a rectangle and the x and y coordiantes of a mouse event
;; RETURNS: the rectangle following a button-down at the given location.
;; STRATEGY: combine simpler functions
(define (get-rectangle-after-button-down r mx my)
  (if (in-rectangle? r mx my)
      (rectangle-after-button-down r mx my)
      r))

;; rectangle-after-button-down : Rectangle Integer Integer -> Rectangle
;; GIVEN: a rectangle and the x and y coordiantes of a mouse event
;; RETURNS: the rectangle following a button-down at the given location
;;          if the button-down is inside the rectangle.
;; STRATEGY: Use template for Rect on r
(define (rectangle-after-button-down r mx my)
  (make-rect (rect-x r)
             (rect-y r)
             (rect-vx r)
             (rect-vy r)
             true
             mx
             my))

;; rectangle-after-drag : Rectangle Integer Integer -> Rectangle
;; GIVEN: a rectangle and the x and y coordiantes of a mouse event
;; RETURNS: the state of rectangle following a button-drag at the
;;          given location.
;; STRATEGY: Use template for Rect on r
(define (rectangle-after-drag r mx my)
  (if (rect-selected? r)
      (rect-after-drag r mx my)
      r))

;; rect-after-drag : Rectangle Integer Integer -> Rectangle
;; GIVEN: a rectangle and the x and y coordiantes of a mouse event
;; RETURNS: the state of rectangle following a button-drag at the
;;          given location if the rectangle is selected.
;; STRATEGY: Use template for Rect on r
(define (rect-after-drag r mx my)
  (make-rect (+ (rect-x r) (- mx (rect-selected-x r)))
             (+ (rect-y r) (- my (rect-selected-y r)))
             (rect-vx r)
             (rect-vy r)
             (rect-selected? r)
             (+ (rect-selected-x r) (- mx (rect-selected-x r)))
             (+ (rect-selected-y r) (- my (rect-selected-y r)))))

;; rectangle-after-button-up : Rectangle -> Rectangle
;; GIVEN: the state of a rectangle
;; RETURNS: the state of given rectangle following a
;;          button-up.
;; STRATEGY: Use template for Rect on r
(define (rectangle-after-button-up r)
  (make-rect (rect-x r)
             (rect-y r)
             (rect-vx r)
             (rect-vy r)
             false
             INITIAL-MOUSE-POINTER-X
             INITIAL-MOUSE-POINTER-Y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES FOR TEST                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-world
  (make-world empty false))

(define world-for-mouse-event-down
  (make-world (list rect-1) false))

(define world-after-mouse-event-down
  (make-world (list rect-1-after-mouse-event) false))

(define world-after-drag
  (make-world (list (rectangle-after-drag rect-1-after-mouse-event 40 50))
              false))

(define unselected-world-after-drag
  (make-world (list rect-2) false))

(define selected-unpaused-world-after-button-up
  (make-world (list rect-1-after-button-up rect-2 unselected-rect-button-up)
              false))

;; TEST:
(begin-for-test
  (check-equal?
   (world-after-mouse-event selected-unpaused-world-state 0 0 "button-up")
   selected-unpaused-world-after-button-up
   "The selected rectangle should have initial-mouse-pointers after
    button-up")
  (check-equal?
   (world-after-mouse-event empty-world 100 200 "button-down")
   empty-world
   "The world with no rectangles should be unchanged after button-down")
  (check-equal?
   (world-after-mouse-event
    world-for-mouse-event-down CANVAS-CENTER-X CANVAS-CENTER-Y "button-down")
   world-after-mouse-event-down
   "The selelcted rectangles should have given mouse coordinates stored in
    selected coordinates field after button-down inside rectangles")
  (check-equal?
   (world-after-mouse-event world-for-mouse-event-down 30 25 "button-down")
   world-for-mouse-event-down
   "The rectangles should have unchanged selected coordinate 
    fields after button-down outside the rectangles")
  (check-equal?
   (world-after-mouse-event world-after-mouse-event-down 40 50 "drag")
   world-after-drag
   "The selected rectangles should move to new location after drag")
  (check-equal?
   (world-after-mouse-event unselected-world-after-drag 30 25 "drag")
   unselected-world-after-drag
   "The unselected rectangles should be unchanged after drag")
  (check-equal?
   (world-after-mouse-event unselected-world-after-drag 30 25 "enter")
   unselected-world-after-drag
   "The world should be unchanged after any other mouse event"))