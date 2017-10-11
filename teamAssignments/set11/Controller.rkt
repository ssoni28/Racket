#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "Model.rkt")
(require "extras.rkt")
(require "Interfaces.rkt")
(require "constants.rkt")

(provide Controller%)

(define Controller%
  (class* object% (Controller<%>)

    (init-field model) ; the model
    
     ;; x and y are the coordinates of the center of the controller
    (init-field [x CANVAS-CENTER-X] [y CANVAS-CENTER-Y]) ;; Real

     ;; handler-width is the side of the 10X10 handler which is present at the
    ;; top left corner of the outbounding rectangle
    (field [handler-width HANDLER-WIDTH]) ;; PosInt

    ;; half-width and half-height are computed from the width and height of the
    ;; rectangle
    (field [half-width  ZERO]) ;; PosInt
    (field [half-height ZERO]) ;; PosInt

    ;; radius of particle
    (field [radius PARTICLE-RADIUS]) ;; PosInt

    ;; the position of the particle
    ;; the particle-x and particle-y are the coordinates of the position of
    ;; the particle
    (field [particle-x ZERO]) ;; Real
    (field [particle-y ZERO]) ;; Real

    ;; the vx and vy of the particle give the velocities in the x and y
    ;; directions, respectively
    (field [particle-vx ZERO]) ;; Real
    (field [particle-vy ZERO]) ;; Real

    ;; fields for dragging
    ;; is the controller selected? Default is false
    (field [selected? false]) ;; Boolean

    ;; is the handler-selected? default is false
    (field [handler-selected? false]) ;; Boolean

    ;; the x and y coordinates of the mouse event on the controller
    ;; If there has ever been a button-down in this object, then these
    ;; contain the position of last button-down relative to
    ;; center of viewer.  Else any value
    (field [saved-mx ZERO]) ;; Int
    (field [saved-my ZERO]) ;; Int

    (super-new)
    
    ;; receive-signal: Signal -> Void
    ;; GIVEN: the signal
    ;; EFFECT: decodes signal and updates local data
    ;; STRATEGY: Cases on fields of the particle
    (define/public (receive-signal sig)
      (cond
        [(report-position-x? sig)
         (set! particle-x (report-position-x-pos sig))]
        [(report-position-y? sig)
         (set! particle-y (report-position-y-pos sig))]
        [(report-velocity-x? sig)
         (set! particle-vx (report-velocity-x-vx sig))]
        [(report-velocity-y? sig)
         (set! particle-vy (report-velocity-y-vy sig))]))

    ;; after-tick : -> Void
    ;; GIVEN: no argument 
    ;; EFFECT: updates the current controller state to the state
    ;; after a tick
    ;; EXAMPLE: 
    (define/public (after-tick) this)

    ;; (abstract in-this?)
    ;; (abstract in-handler?)

    ;; after-button-up : Int Int -> Void
    ;; GIVEN: x and y coordinates of the mouse position for the button up event
    ;; EFFECT: updates the state of the controller with the handler and
    ;; controller unselected.
    (define/public (after-button-up mx my)
      (send this button-up mx my))

    (abstract button-up)

    ;; after-button-down : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position on button down event
    ;; EFFECT: updates the current contoller state to the state after button
    ;; down event. If the viewer is selected, updates the mouse coordinates and
    ;; marks the viewer as selected.
    ;; STRATEGY: Cases on whether the mouse position is inside the controller
    (define/public (after-button-down mx my)
      (send this button-down mx my))

    (abstract button-down)

    ;; after-drag : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position
    ;; EFFECT: updates the current controller state to the state after mouse
    ;; drag event
    ;; STRATEGY: Cases on whether the handler is selected.
    ;; If it is selected, move it so that the vector from its position to
    ;; the drag event is equal to saved-mx and saved-my.  
    (define/public (after-drag mx my)
      (send this drag mx my))

    (abstract drag)

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the current position controller state to the state
    ;; that should follow the specified key event
    ;; send right, left, up, down to the model and increment/decrement
    ;; the appropriate quantity by 5 units
    ;; STRATEGY: Cases on key event
    (define/public (after-key-event kev)
      (send this key-event kev))

    (abstract key-event)

    ;; in-handler? : Int Int -> Boolean
    ;; GIVEN: x and y coordinates of the mouse position
    ;; RETURNS: true iff the mouse event is inside the handler
    ;; otherwise returns false
    (define/public (in-handler? other-x other-y)
      (begin
        (and
         (<= (- x half-width) other-x (+ (- x half-width) handler-width))
         (<= (- y half-height) other-y (+ (- y half-height) handler-width)))))

    ;; current-color: -> String
    ;; GIVEN: no argument
    ;; RETURNS: the color string
    ;; STARTEGY: Cases on selected? 
    ;; (define/public (current-color)
    ;; (if selected? "red" "black"))

    ;; handler-color: -> String
    ;; GIVEN: no argument
    ;; RETURNS: the color string
    ;; STARTEGY: Cases on handler-selected? 
    ;; (define/public (handler-color)
    ;; (if handler-selected? "red" "black"))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with the position controller
    ;; painted on it.
    ;; STRATEGY: place the image centered at x y
    (define/public (add-to-scene scene)
      (place-image (send this viewer-image) x y scene))

    (abstract viewer-image)
    ))