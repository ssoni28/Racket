;; mvc.rkt

;;============================================================================;;
;;                                 GOAL                                       ;;
;;============================================================================;;

;; To simulate a dimensionless particle bouncing in a 150x100 rectangle.
;; For this system, you will produce 5 viewer-controllers:
;;
;; 1)  A position controller, similar to the one in the Examples, but using
;;     the arrow keys to move the particle in the x or y direction.
;; 2)  A velocity controller, similar to the one in the Examples, but using
;;     the arrow keys to alter the velocity of the particle in the x or y
;;     direction.
;; 3)  Both the position and velocity controllers display both the position
;;     and velocity of the particle, as in the demo.
;; 4)  An XY controller, which shows a representation of the particle 
;;     bouncing in the rectangle. With this controller, the user can drag 
;;     the particle using the mouse. Dragging the mouse causes the particle 
;;     to follow the mouse pointer via a Smooth Drag.
;; 4)  An X controller, which is like the XY controller, except that it
;;     displays only the x coordinate of the particle's motion. Dragging
;;     the mouse in the X controller alters the particle's position in the
;;     x direction.
;; 5)  A Y controller, which is like the X controller except that it works
;;     in the y direction.
;;============================================================================;;
;; START WITH (run rate)
;; EXAMPLES:
;; (run 0.5)
;;============================================================================;;

#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "ParticleWorld.rkt")
(require "Model.rkt")
(require rackunit)
(require "ControllerFactory.rkt")


(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
;;============================================================================;;
;;                              PROVIDE STATEMENTS                            ;;
;;============================================================================;;

(provide
 run)

;;============================================================================;;
;;                                    run                                     ;;
;;============================================================================;;
;; run : PosReal -> Void
;; GIVEN: a frame rate, in sec/tick
;; EFFECT: Creates and runs the MVC simulation with the given frame rate.
;; EXAMPLES: (run .5) -> Void
(define (run rate)
  (let* ((m (new Model%))
         (w (make-world m CANVAS-WIDTH CANVAS-HEIGHT)))
    (begin
      (send w add-widget
            (new ControllerFactory% [m m][w w]))
      (send w run rate))))

;;============================================================================;;
;;                                END OF FILE                                 ;;
;;============================================================================;;