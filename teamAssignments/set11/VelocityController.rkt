;; VelocityController.rkt

;;============================================================================;;
;;                                  DESCRIPTION                               ;;
;;============================================================================;;
;; A velocity controller displays x y position of dimensionless particle
;; bouncing inside the 150X100 rectangle on the given canvas(600X500)
;; and the velocities vx,vy of the particle.
;; User can change the position of the particle using the arrow keys to
;; alter the velocity of the particle in the x or y direction.
;; "up",decrements the vy velocity of the particle by 5 units
;; "down", increment the vy velocity of the particle by 5 units
;; "left", decrements the vx velocity of the particle by 5 units
;; "right", increments the vx velocity of the particle by 5 units
;; The controller rectangle is draggable with the help of a 10X10 handler.
;;============================================================================;;

#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "constants.rkt")
(require "Model.rkt")
(require "Controller.rkt")
(require "ControllerHooks.rkt")

;;============================================================================;;
;;                              PROVIDE STATEMENTS                            ;;
;;============================================================================;;
(provide VelocityController%)

;;============================================================================;;
;;                                    CLASS                                   ;;
;;============================================================================;;

;; A VelocityController% is a (new VelocityController% [model Model<%>]
;;                              [x Real][y Real][width PosInt][height PosInt]
;;                              [half-width PosInt] [half-height PosInt]
;;                              [handler-width PosInt][particle-x Real]
;;                              [particle-y Real][particle-vx Real]
;;                              [particle-vy real][selected? Boolean]
;;                              [handler-selelcted? Boolean][saved-mx Int]
;;                              [saved-my Int]))

(define VelocityController%
  (class* Controller%
    (Controller<%> ControllerHooks<%>) ;; Include all methods of interfaces
                                       ;; Controller<%> & ControllerHooks<%>

    (inherit-field model)  ;; the model

    ;; x and y are the coordinates of the center of the velocity controller
    (inherit-field x y) ;; Real 

    ;; width and height are the dimensions of the
    ;; velocity controller
    (init-field [width CONTROLLER-WIDTH][height CONTROLLER-HEIGHT]) ;; PosInt

    ;; half-width and half-height are computed from the width and height of the
    ;; controller
    (inherit-field half-width)   ;; PosInt
    (inherit-field half-height)  ;; PosInt

    ;; handler-width is the side of the 10X10 handler which is present at the
    ;; top left corner of the outbounding rectangle
    (inherit-field handler-width) ;; PosInt
    
    ;; the position of the particle
    ;; the particle-x and particle-y are the coordinates of the position of
    ;; the particle
    (inherit-field particle-x particle-y) ;; Real
    
    ;; the particle-vx and particle-vy of the particle give the velocities in
    ;; the x and y directions, respectively
    (inherit-field particle-vx particle-vy) ;; Real

    ;; fields for dragging
    ;; is the controller selected?, handler-selected? Default is false
    (inherit-field selected? handler-selected?) ;; Boolean

    ;; the x and y coordinates of the mouse event on the controller
    ;; If there has ever been a button-down in this object, then these
    ;; contain the position of last button-down relative to
    ;; center of viewer.  Else any value
    (inherit-field saved-mx saved-my) ;; Int

    (super-new)

    ;; set the values of particle-x and particle-y
    (set! particle-y PART-Y-VC)
    (set! particle-x PART-X-VC)

    ;; set the values for half-width and half-height
    (set! half-width (/ width 2))
    (set! half-height (/ height 2))
    
    ;; register the velocity controller in the model
    (send model register this)
    
    ;; button-down : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position on button down event
    ;; EFFECT: updates the current contoller state to the state after button
    ;; down event. If the viewer is selected, updates the mouse coordinates and
    ;; marks the viewer as selected.
    ;; EXAMPLE: Call button-down method on velocitycontroller object and
    ;; verify the comparable data
    ;; (send VC after-button-down 400 500)
    ;; (check-equal? (send VC for-test:selected?) false)
    ;; (send VC after-button-down 225 225)
    ;; (check-equal? (send VC for-test:handler-selected?) true)
    ;; STRATEGY: Cases on whether the mouse position is inside the controller
    (define/override (button-down mx my)
      (if (in-this? mx my)
          (if (in-handler? mx my)
              (begin
                (set! handler-selected? true)
                (set! saved-mx (- mx x))
                (set! saved-my (- my y)))
              (begin
                (set! selected? true)))
        3742))

    ;; button-up : Int Int -> Void
    ;; GIVEN: x and y coordinates of the mouse position for the button up event
    ;; EFFECT: updates the state of the controller with the handler and
    ;; controller unselected.
    ;; EXAMPLE: Call button-up method on velocitycontroller object and
    ;; verify the comparable data
    ;; (send VC after-button-up 300 300)
    ;; (check-equal? (send VC for-test:selected?) false)
    (define/override (button-up mx my)
      (set! selected? false)
      (set! handler-selected? false))
      

    ;; drag : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position
    ;; EFFECT: updates the current controller state to the state after mouse
    ;; drag event
    ;; EXAMPLE: Call drag method on velocitycontroller object and
    ;; verify the comparable data
    ;; (send VC1 after-drag 200 300)
    ;; (check-equal? (send VC1 for-test:selected?) false)
    ;; STRATEGY: Cases on whether the handler is selected.
    ;; If it is selected, move it so that the vector from its position to
    ;; the drag event is equal to saved-mx and saved-my. 
    (define/override (drag mx my)
      (if handler-selected?
        (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
        2744))

    ;; in-this? : Int Int -> Boolean
    ;; GIVEN: x and y coordinates of the mouse position
    ;; RETURNS: true iff the mouse event is inside the velocity controller,
    ;; otherwise returns false
    (define/public (in-this? other-x other-y)
      (and
        (<= (- x half-width) other-x (+ x half-width))
        (<= (- y half-height) other-y (+ y half-height))))

    ;; in-handler? : Int Int -> Boolean
    ;; GIVEN: x and y coordinates of the mouse position
    ;; RETURNS: true iff the mouse event is inside the handler
    ;; otherwise returns false
    (define/override (in-handler? other-x other-y)
       (super in-handler? other-x other-y))
    
    ;; key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the current velocity controller state to the state
    ;; that should follow the specified key event
    ;; send right, left, up, down to the model and increment/decrement
    ;; the appropriate quantity by 5 units
    ;; EXAMPLE: Call key-event method on velocitycontroller object and
    ;; verify the comparable data
    ;; (send VC2 after-button-down 300 260)
    ;; (send VC2 after-key-event "left")
    ;; (check-equal? (send VC2 for-test:get-particle-vx) -5)
    ;; (send VC2 after-key-event "right")
    ;; (send VC2 after-key-event "right")
    ;; (check-equal? (send VC2 for-test:get-particle-vx) 5)
    ;; (send VC2 after-key-event "up")
    ;; (check-equal? (send VC2 for-test:get-particle-vy) -5)
    ;; (send VC2 after-key-event "down")
    ;; (check-equal? (send VC2 for-test:get-particle-vy) 0)
    ;; STRATEGY: Cases on key event
    (define/override (key-event kev)
      (if selected?
        (cond
        [(key=? RIGHT kev)
         (send model execute-command
           (make-incr-velocity-x POS-INC))]
        [(key=? LEFT kev)
         (send model execute-command
           (make-incr-velocity-x NEG-DEC))]
        [(key=? DOWN kev)
         (send model execute-command
           (make-incr-velocity-y POS-INC))]
        [(key=? UP kev)
         (send model execute-command
           (make-incr-velocity-y NEG-DEC))])
        3456))
    
    ;; current-color: -> String
    ;; GIVEN: no argument
    ;; RETURNS: the color string
    ;; STARTEGY: Cases on selected? 
    (define/public (current-color)
      (if selected? "red" "black"))

    ;; handler-color: -> String
    ;; GIVEN: no argument
    ;; RETURNS: the color string
    ;; STARTEGY: Cases on handler-selected?
    (define/public (handler-color)
      (if handler-selected? "red" "black"))

    ;; viewer-image: -> Scene
    ;; GIVEN: no argument
    ;; RETURNS: a scene with the specified viewer painted on it
    ;; assemble the image of the viewer
    ;; EXAMPLE: Call add-to-scene method on velocitycontroller object and
    ;; (send VC1 add-to-scene EMPTY-CANVAS)
    ;; -> (place-image IMAGE 300 250 EMPTY-CANVAS)
    (define/override (viewer-image)
      (let
          ((the-data-image (data-image))
           (handle-image (square 10 "outline" (handler-color))))
        (overlay/align LEFT TOP handle-image
                       (overlay 
                        the-data-image
                        (rectangle
                         (max width (+ (image-width the-data-image) 10))
                         (max height (+ (image-height the-data-image) 10))
                         "outline" 
                         "black")))))
    ;; data-image: -> Scene
    ;; GIVEN: no argument
    ;; RETURNS: a scene with the specified data painted on the viewer
    ;; assemble the image  of the text on the viewer
     (define (data-image)
      (above
       (text "Arrow keys change velocity" FONT1 (current-color))
       (text (string-append
              "X = "
              (real->decimal-string (exact->inexact particle-x) 1)
              " Y = "
              (real->decimal-string (exact->inexact particle-y) 1))
             FONT2
             (current-color))
       (text (string-append
              "VX = "
              (real->decimal-string (exact->inexact particle-vx) 1)
              " VY = "
              (real->decimal-string (exact->inexact particle-vy) 1))
             FONT2
             (current-color))))

    ;;for-test:handler-selected? :-> Boolean
    (define/public (for-test:handler-selected?) handler-selected?)

    ;; for-test:selected? : -> Boolean
    (define/public (for-test:selected?) selected?)

    ;; for-test:get-x : -> Real
    (define/public (for-test:get-x) x)

    ;; for-test:get-particle-vy : -> Real
    (define/public (for-test:get-particle-vy) particle-vy)

     ;; for-test:get-particle-vx : -> Real
    (define/public (for-test:get-particle-vx) particle-vx)
    
    ))
;;============================================================================;;
;;                                   TESTS                                    ;;
;;============================================================================;;
;; Examples for tests
;; --------------------

(define MODEL (new Model%))
(define VC (new VelocityController% [model MODEL]))
(define VC1 (new VelocityController% [model MODEL]))
(define VC2 (new VelocityController% [model MODEL]))

(define TEXT
  (above
   (text "Arrow keys change velocity" 10 "black")
   (text (string-append
          "X = "
          (number->string 75.0)
          " Y = "
          (number->string 50.0))
         12
         "black")
   (text (string-append
          "VX = "
          (number->string 0.0)
          " VY = "
          (number->string 0.0))
         12
         "black")))
(define IMAGE
  (let
      ((the-data-image TEXT)
       (handle-image (square 10 "outline" "black")))
    (overlay/align "left" "top" handle-image
                   (overlay 
                    the-data-image
                    (rectangle
                     160
                     60
                     "outline"
                     "black")))))

;; TESTS:
;; --------
(begin-for-test
  (send VC after-tick)
  (check-equal? (send VC for-test:handler-selected?) false)
  (send VC after-button-down 400 500)
  (check-equal? (send VC for-test:selected?) false)
  (send VC after-button-down 225 225)
  (check-equal? (send VC for-test:handler-selected?) true)
  (check-equal? (send VC handler-color) "red")
  (send VC after-drag 225 225)
  (check-equal? (send VC for-test:get-x) 300)
  (send VC after-button-down 300 260)
  (check-equal? (send VC for-test:selected?) true)
  (check-equal? (send VC current-color) "red")
  (send VC after-button-up 300 300)
  (check-equal? (send VC for-test:selected?) false)
  (send VC1 after-button-down 400 500)
  (send VC1 after-drag 200 300)
  (check-equal? (send VC1 for-test:selected?) false)
  (check-equal? (send VC1 add-to-scene EMPTY-CANVAS)
                (place-image IMAGE 300 250 EMPTY-CANVAS))
  (send VC1 after-key-event "l")
  (check-equal? (send VC1 for-test:selected?) false)
  (send VC2 after-button-down 300 260)
  (send VC2 after-key-event "left")
  (check-equal? (send VC2 for-test:get-particle-vx) -5)
  (send VC2 after-key-event "right")
  (send VC2 after-key-event "right")
  (check-equal? (send VC2 for-test:get-particle-vx) 5)
  (send VC2 after-key-event "up")
  (check-equal? (send VC2 for-test:get-particle-vy) -5)
  (send VC2 after-key-event "down")
  (check-equal? (send VC2 for-test:get-particle-vy) 0))

;;============================================================================;;
;;                                END OF FILE                                 ;;
;;============================================================================;;