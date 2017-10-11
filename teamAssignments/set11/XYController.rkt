;; XYController.rkt

;;============================================================================;;
;;                                  DESCRIPTION                               ;;
;;============================================================================;;
;; An XY controller, which shows a representation of the particle bouncing
;; in the rectangle. With this controller, the user can drag the particle
;; using the mouse. Dragging the mouse causes the particle to follow the
;; mouse pointer via a Smooth Drag.
;; The controller is draggable with the help of 10x10 handler at top left corner
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
(provide XYController%)

;;============================================================================;;
;;                             XYController% CLASS                            ;;
;;============================================================================;;
;; A XYController% is a (new XController% [model Model<%>]
;;                              [x Real][y Real][width PosInt][height PosInt]
;;                              [half-width PosInt] [half-height PosInt]
;;                              [handler-width PosInt][particle-x Real]
;;                              [particle-y Real][particle-vx Real]
;;                              [particle-vy real][selected? Boolean]
;;                              [handler-selelcted? Boolean][saved-mx Int]
;;                              [saved-my Int][x-min Real][x-max Real]
;;                              [y-min Real][y-max Real])

(define XYController%
  (class* Controller%
    (Controller<%> ControllerHooks<%>) ;; Include all methods of interfaces
                                       ;; Controller<%> & ControllerHooks<%>
    
    (inherit-field model)  ;; the model

    ;; x and y are the coordinates of the center of the xycontroller
    (inherit-field x y) ;; Real  

    ;; controller-width and controller-height are the dimensions of the
    ;; xycontroller
    (init-field [controller-width CON-W-XC][controller-height CON-H-YC])
    ;; PosInt

    ;; width and height are the dimensions of the outbounding rectangle
    ;; which is at a difference of 50 units on all sides of the controller
    (init-field [width WIDTH-XC] [height HEIGHT-YC]) ;; PosInt

    ;; half-width and half-height are computed from the width and height of the
    ;; rectangle
    (inherit-field half-width)   ;; PosInt
    (inherit-field half-height)  ;; PosInt

    (inherit-field handler-width)
    
    ;; radius of particle
    (inherit-field radius) ;; PosInt
    
    ;; the particle-x and particle-y are the coordinates of the position of
    ;; the particle
    (inherit-field particle-x particle-y) ;; Real

    ;; the particle-vx and particle-vy of the particle give the velocities in
    ;; the x and y directions, respectively
    (inherit-field particle-vx particle-vy) ;; Real
    
    ;; fields for dragging
    ;; is the controller selected? ,handler-selected? Default is false
    (inherit-field selected? handler-selected?) ;; Boolean

    ;; the x and y coordinates of the mouse event on the controller
    ;; If there has ever been a button-down in this object, then these
    ;; contain the position of last button-down relative to
    ;; center of viewer.  Else any value
    (inherit-field saved-mx saved-my) ;; Int

    ;; boundary values in both x and y directions
    (field [xmin 0] [xmax 150] [ymin 0] [ymax 100])
    
    (super-new)

    ;; set the values of particle-x and particle-y
    (set! particle-x PART-X-PC)
    (set! particle-y PART-Y-PC)

    ;; set the values of half-width and half-height
    (set! half-width (/ width 2))
    (set! half-height (/ height 2))
    
    ;; register the xycontroller in the model
    (send model register this)

    ;; key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the current controller state to the state that should
    ;; follow the specified key event
    ;; EXAMPLE: Call key-event method on xycontroller object and verify the
    ;; comparable data
    ;; (send XYC after-key-event "p")
    ;; (check-equal? (send XYC for-test:selected?) false)
    (define/override (key-event kev) this)
    
    ;; button-down : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position  on button down event
    ;; EFFECT: updates the current contoller state to the state after button
    ;; down event. If the viewer is selected, updates the mouse coordinates and
    ;; marks the viewer as selected.
    ;; EXAMPLE: Call button-down method on xycontroller object and verify the
    ;; comparable data
    ;; (send XYC after-button-down 280 230)
    ;; (check-equal? (send XYC for-test:selected?) true)
    ;; STRATEGY: Cases on whether the mouse position is inside the handler
    (define/override (button-down mx my)
      (if (in-handler? mx my)
          (begin
            (set! handler-selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          (if (in-this? mx my)
              (begin
                (set! selected? true)
                (send model execute-command
                      (make-set-selected? true))
                (set! saved-mx (- mx particle-x))
                (set! saved-my (- my particle-y)))
              5555)))

    ;; button-up : Int Int -> Void
    ;; GIVEN: x and y coordinates of the mouse position for the button up event
    ;; EFFECT: updates the state of the controller with the handler and
    ;; controller unselected.
    ;; EXAMPLE: Call button-up method on xycontroller object and verify the
    ;; comparable data
    ;; (send XYC after-button-up 200 200)
    ;; (check-equal? (send XYC for-test:handler-selected?) false)
    ;; the state of the selected? field is also updated in the model
    (define/override (button-up mx my)
      (set! selected? false)
       (send model execute-command
                      (make-set-selected? false))
      (set! handler-selected? false))
      

    ;; drag : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position
    ;; EFFECT: updates the current controller state to the state after mouse
    ;; drag event
    ;; EXAMPLE: Call drag method on xycontroller object and verify the
    ;; comparable data
    ;; (send XYC after-drag 500 500)
    ;; (check-equal? (send XYC for-test:handler-selected?) false)
    ;; STRATEGY: Cases on whether the handler is selected.
    ;; If it is selected, move it so that the vector from its position to
    ;; the drag event is equal to saved-mx.  Report the new x position to
    ;; the model
    (define/override (drag mx my)
      (if handler-selected?
        (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
        (if (in-this? mx my)
            (begin
              (set! selected? true)
              (set! particle-x (within-limits (- mx saved-mx) xmin xmax))
              (send model execute-command
                    (make-set-position-x particle-x))
              (set! particle-y (within-limits (- my saved-my) ymin ymax))
              (send model execute-command
                    (make-set-position-y particle-y)))
            2744)))

    ;; within-limits: Real Real Real -> Real
    ;; GVIEN: three real values representing the
    ;; current value, minimum value and the maximum value
    ;; RETURNS: the value like the given one but limited
    ;; to the boundary limits(min,max) for both x and y
    ;; coordiantes
    (define (within-limits val m1 m2)
      (min (max val m1) m2))

    ;; in-this? : Int Int -> Boolean
    ;; GIVEN: x and y coordinates of the mouse position
    ;; RETURNS: true iff the mouse event is inside the xycontroller
    ;; otherwise returns false
     (define/public (in-this? other-x other-y)
      (and
       (<= (- x (/ controller-width 2)) other-x (+ x (/ controller-width 2)))
       (<= (- y (/ controller-height 2)) other-y (+ y (/ controller-height 2))))
      )

    ;; in-handler? : Int Int -> Boolean
    ;; GIVEN: x and y coordinates of the mouse position
    ;; RETURNS: true iff the mouse event is inside the handler
    ;; otherwise returns false
    (define/override (in-handler? other-x other-y)
      (super in-handler? other-x other-y))
    
    ;; handler-color: -> String
    ;; GIVEN: no argument
    ;; RETURNS: the color string
    ;; STARTEGY: Cases on handler-selected?
    (define/public (handler-color)
      (if handler-selected? RED BLACK))

    ;; viewer-image: -> Scene
    ;; GIVEN: no argument
    ;; RETURNS: a scene with the specified viewer painted on it
    ;; assemble the image of the viewer
    ;; EXAMPLE: Call add-to-scene method on xycontroller object and verify the
    (define/override (viewer-image)
      (let
          ((particle-image
            (overlay (circle 2 "solid" BLACK) (circle radius "solid" RED)))
           (handle-image (square FONT1 "outline" (handler-color)))
           (rectangle-image (rectangle width height "outline" BLACK))
           (xycontroller-image
            (rectangle controller-width controller-height "outline" BLUE))) 
        (overlay/align LEFT TOP handle-image
                       (overlay
                        xycontroller-image
                        (place-image particle-image particle-x particle-y
                                     xycontroller-image)
                        rectangle-image
                        ))))

    ;; for-test:selected? : -> Boolean
    (define/public (for-test:selected?) selected?)

    ;; for-test:get-x : -> Real
    (define/public (for-test:get-x) x)

    ;;for-test:handler-selected? :-> Boolean
    (define/public (for-test:handler-selected?) handler-selected?)
    ))

;;============================================================================;;
;;                                   TESTS                                    ;;
;;============================================================================;;
;; Examples for tests
;; --------------------
(define MODEL (new Model%))
(define XYC (new XYController% [model MODEL]))
(define XYC1 (new XYController% [model MODEL]))
(define XYC2 (new XYController% [model MODEL]))

;; TESTS:
;; -------
(begin-for-test
  (send XYC after-tick)
  (send XYC after-key-event "p")
  (check-equal? (send XYC for-test:selected?) false)
  (send XYC after-button-down 400 500)
  (check-equal? (send XYC for-test:get-x) 300)
  (send XYC after-button-down 280 230)
  (check-equal? (send XYC for-test:selected?) true)
  (send XYC after-drag 280 230)
  (check-equal? (send XYC for-test:get-x) 300)
  (send XYC after-drag 500 500)
  (check-equal? (send XYC for-test:handler-selected?) false)
  (send XYC after-button-up 200 200)
  (check-equal? (send XYC for-test:handler-selected?) false)
  (send XYC2 after-button-down 205 180)
  (check-equal? (send XYC2 for-test:handler-selected?) true)
  (send XYC2 after-drag 205 180)
  (check-equal? (send XYC2 for-test:get-x) 300)
  (local
    [(define IMAGE
       (let
           ((particle-image
             (overlay (circle 2 "solid" BLACK) (circle 8 "solid" RED)))
            (handle-image (square FONT1 "outline" BLACK))
            (rectangle-image (rectangle WIDTH-XC HEIGHT-YC "outline" BLACK))
            (xycontroller-image
             (rectangle CON-W-XC CON-H-YC "outline" BLUE))) 
         (overlay/align
          LEFT
          TOP
          handle-image
          (overlay
           xycontroller-image
           (place-image particle-image 75 50 xycontroller-image)
           rectangle-image
      ))))
     (define IMAGE1
       (let
           ((particle-image
             (overlay (circle 2 "solid" BLACK) (circle 8 "solid" RED)))
            (handle-image (square FONT1 "outline" RED))
            (rectangle-image (rectangle WIDTH-XC HEIGHT-YC "outline" BLACK))
            (xycontroller-image
             (rectangle CON-W-XC CON-H-YC "outline" BLUE))) 
         (overlay/align
          LEFT
          TOP
          handle-image
          (overlay
           xycontroller-image
           (place-image particle-image 75 50 xycontroller-image)
           rectangle-image
           ))))]
  (check-equal? (send XYC1 add-to-scene EMPTY-CANVAS)
                (place-image IMAGE 300 250 EMPTY-CANVAS))
  (send XYC1 after-button-down 205 180)
  (check-equal? (send XYC2 for-test:get-x) 300)
  (check-equal? (send XYC1 add-to-scene EMPTY-CANVAS)
                (place-image IMAGE1 300 250 EMPTY-CANVAS))))

;;============================================================================;;
;;                                END OF FILE                                 ;;
;;============================================================================;;