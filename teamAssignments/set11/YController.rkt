;; YController.rkt

;;============================================================================;;
;;                                  DESCRIPTION                               ;;
;;============================================================================;;
;; An Y controller, which shows a representation of the particle bouncing in 
;; the rectangle. It displays only the y coordinate of the particle's motion.
;; With this controller, the user can drag the particle using the
;; mouse. Dragging the mouse in the Y controller alters the particle's position 
;; in the y direction.
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
(provide YController%)

;;============================================================================;;
;;                              YController% CLASS                            ;;
;;============================================================================;;
;; A YController% is a (new XController% [model Model<%>]
;;                              [x Real][y Real][width PosInt][height PosInt]
;;                              [half-width PosInt] [half-height PosInt]
;;                              [handler-width PosInt][particle-x Real]
;;                              [particle-y Real][particle-vx Real]
;;                              [particle-vy real][selected? Boolean]
;;                              [handler-selelcted? Boolean][saved-mx Int]
;;                              [saved-my Int][x-min Real][x-max Real]
;;                              [y-min Real][y-max Real])

(define YController%
  (class* Controller%
    (Controller<%> ControllerHooks<%>) ;; Include all methods of interfaces
                                       ;; Controller<%> & ControllerHooks<%>
    
    (inherit-field model)  ;; the model
    
    ;; x and y are the coordinates of the center of the ycontroller
    (inherit-field x y) ;; Real
    
    ;; controller-width and controller-height are the dimensions of the
    ;; ycontroller
    (init-field [controller-width CON-W-YC] [controller-height CON-H-YC])
    ;; PosInt

    ;; width and height are the dimensions of the outbounding rectangle
    ;; which is at a difference of 50 units on all sides of the ycontroller
    (init-field [width CON-W-YC] [height HEIGHT-YC])

    ;; half-width and half-height are computed from the width and height of the
    ;; rectangle
    (inherit-field half-width)   ;; PosInt
    (inherit-field half-height)  ;; PosInt

    ;; handler-width is the side of the 10X10 handler which is present at the
    ;; top left corner of the outbounding rectangle
    (inherit-field handler-width) ;; PosInt
    
    ;; radius of particle
    (inherit-field radius) ;; PosInt

    ;; the particle-x and particle-y are the coordinates of the position of
    ;; the particle
    (inherit-field particle-x particle-y) ;; Real

    ;; the particle-vx and particle-vy of the particle give the velocities in
    ;; the x and y directions, respectively
    (inherit-field particle-vx particle-vy) ;; Real
    
    (inherit-field selected? handler-selected?) ;; Boolean
    (inherit-field saved-mx saved-my) ;; Int

    ;; boundary values in both x and y directions
    (field [xmin 0] [xmax 150] [ymin 0] [ymax 100])
    
    (super-new)

    ;; set the values of particle-x and particle-y
    (set! particle-x PART-X-YC)
    (set! particle-y PART-Y-YC)

    ;; set the values for half-width and half-heght
    (set! half-width (/ width 2))
    (set! half-height (/ height 2))
    
    ;; register the ycontroller in the model
    (send model register this)
    
    ;; key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the current controller state to the state that should
    ;; follow the specified key event
    ;; EXAMPLE: Call key-event method on ycontroller object and verify the
    ;; comparable data
    ;; (send YC after-key-event "n")
    ;; (check-equal? (send YC for-test:handler-selected?) false)
    (define/override (key-event kev) this)
    
    ;; button-down : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position  on button down event
    ;; EFFECT: updates the current contoller state to the state after button
    ;; down event. If the viewer is selected, updates the mouse coordinates and
    ;; marks the viewer as selected.
    ;; EXAMPLE: Call button-down method on ycontroller object and verify the
    ;; comparable data
    ;; (send YC after-button-down 400 500)
    ;; (check-equal? (send YC for-test:get-x) 250)
    ;; (send YC after-button-down 230 280)
    ;; (check-equal? (send YC for-test:selected?) true)
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
    ;; the state of the selected? field is also updated in the model
    ;; EXAMPLE: Call button-up method on ycontroller object and verify the
    ;; comparable data
    ;; (send YC after-button-up 200 200)
    ;; (check-equal? (send YC for-test:handler-selected?) false)
    (define/override (button-up mx my)
      (set! selected? false)
      (send model execute-command
                      (make-set-selected? false))
      (set! handler-selected? false))
    
    
    ;; drag : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position
    ;; EFFECT: updates the current controller state to the state after mouse
    ;; drag event
    ;; EXAMPLE: Call drag method on ycontroller object and verify the
    ;; comparable data
    ;; (send YC2 after-drag 230 230)
    ;; (check-equal? (send YC2 for-test:get-x) 250)
    ;; STRATEGY: Cases on whether the handler is selected.
    ;; If it is selected, move it so that the vector from its position to
    ;; the drag event is equal to saved-mx.  Report the new y position to
    ;; the model
    (define/override (drag mx my)
      (if handler-selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
           (if (in-this? mx my)
             (begin
               (set! selected? true)
               (set! particle-x (- mx saved-mx))
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
    ;; RETURNS: true iff the mouse event is inside the ycontroller
    ;; otherwise returns false
    (define/public (in-this? other-x other-y)
      (and
       (<=
        (- x (/ controller-width 2))
        other-x
        (+ x (/ controller-width 2)))
       (<=
        (- y (/ controller-height 2))
        other-y
        (+ y (/ controller-height 2)))))

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
    ;; EXAMPLE: Call add-to-scene method which will internally call viwer-image
    ;; of this class on xcontroller object and verify the
    (define/override (viewer-image)
      (let
          ((particle-image
            (overlay (circle 2 "solid" BLACK) (circle radius "solid" RED)))
           (handle-image (square FONT1 "outline" (handler-color)))
           (rectangle-image (rectangle width height "outline" BLACK))
           (ycontroller-image
            (rectangle controller-width controller-height "outline" BLUE)))
        (overlay/align LEFT TOP handle-image
                       (overlay ycontroller-image
                        (place-image
                         particle-image
                         (/ controller-width 2) particle-y
                         ycontroller-image)
                        rectangle-image))))
    
    ;;for-test:handler-selected? :-> Boolean
    (define/public (for-test:handler-selected?) handler-selected?)

    ;; for-test:selected? : -> Boolean
    (define/public (for-test:selected?) selected?)

    ;; for-test:get-x : -> Real
    (define/public (for-test:get-x) x)
    ))

;;============================================================================;;
;;                                   TESTS                                    ;;
;;============================================================================;;
;; Examples for tests
;; --------------------
(define MODEL (new Model%))
(define YC (new YController% [model MODEL]))
(define YC1 (new YController% [model MODEL]))
(define YC2 (new YController% [model MODEL]))

;; TESTS
;; -------
(begin-for-test
  (send YC after-tick)
  (check-equal? (send YC for-test:handler-selected?) false)
  (send YC after-key-event "n")
  (check-equal? (send YC for-test:handler-selected?) false)
  (send YC after-button-down 400 500)
  (check-equal? (send YC for-test:get-x) 300)
  (send YC after-button-down 280 230)
  (check-equal? (send YC for-test:selected?) true)
  (send YC after-drag 280 230)
  (check-equal? (send YC for-test:get-x) 300)
  (send YC after-drag 500 500)
  (check-equal? (send YC for-test:handler-selected?) false)
  (send YC after-button-up 200 200)
  (check-equal? (send YC for-test:handler-selected?) false)
  (send YC2 after-button-down 280 180)
  (check-equal? (send YC2 for-test:handler-selected?) true)
  (send YC2 after-drag 280 180)
  (check-equal? (send YC2 for-test:get-x) 300)
  (local
    [(define IMAGE
      (let
          ((particle-image
            (overlay (circle 2 "solid" BLACK) (circle 8 "solid" RED)))
           (handle-image (square FONT1 "outline" BLACK))
           (rectangle-image (rectangle CON-W-YC HEIGHT-YC "outline" BLACK))
           (ycontroller-image
            (rectangle CON-W-YC CON-H-YC "outline" BLUE)))
        (overlay/align LEFT TOP handle-image
                       (overlay ycontroller-image
                        (place-image
                         particle-image
                         (/ CON-W-YC 2) PART-Y-YC
                         ycontroller-image)
                        rectangle-image))))
     (define IMAGE1
       (let
           ((particle-image
             (overlay (circle 2 "solid" BLACK) (circle 8 "solid" RED)))
            (handle-image (square FONT1 "outline" RED))
            (rectangle-image (rectangle CON-W-YC HEIGHT-YC "outline" BLACK))
            (ycontroller-image
             (rectangle CON-W-YC CON-H-YC "outline" BLUE)))
         (overlay/align LEFT TOP handle-image
                        (overlay ycontroller-image
                                 (place-image
                                  particle-image
                                  (/ CON-W-YC 2) PART-Y-YC
                                  ycontroller-image)
                                 rectangle-image))))]
    (check-equal?
     (send YC1 add-to-scene EMPTY-CANVAS)
     (place-image IMAGE 300 250 EMPTY-CANVAS))
    (send YC1 after-button-down 280 180)
    (check-equal?
     (send YC1 add-to-scene EMPTY-CANVAS)
     (place-image IMAGE1 300 250 EMPTY-CANVAS))))
;;============================================================================;;
;;                                END OF FILE                                 ;;
;;============================================================================;;
