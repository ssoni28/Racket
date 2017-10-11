;; constants.rkt
;; Include all the constants

#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "ParticleWorld.rkt")
(require rackunit)
;;============================================================================;;
;;                              PROVIDE STATEMENTS                            ;;
;;============================================================================;;

(provide
 CANVAS-WIDTH
 CANVAS-HEIGHT
 CANVAS-CENTER-X
 CANVAS-CENTER-Y
 EMPTY-CANVAS
 CONTROLLER-WIDTH
 CONTROLLER-HEIGHT
 PART-X-PC
 PART-Y-PC
 LEFT
 RIGHT
 UP
 DOWN
 FONT1
 FONT2
 PART-X-VC
 PART-Y-VC
 TOP
 POS-INC
 NEG-DEC
 CON-W-XC
 CON-H-XC
 WIDTH-XC
 HEIGHT-XC
 PART-X-XC
 PART-Y-XC
 BLACK
 BLUE
 RED
 CON-W-YC
 CON-H-YC
 HEIGHT-YC
 PART-X-YC
 PART-Y-YC
 PARTICLE-RADIUS
 ZERO
 HANDLER-WIDTH
 RECTANGLE-WIDTH
 RECTANGLE-HEIGHT
 TWO)

;;============================================================================;;
;;                               CONSTANTS                                    ;;
;;============================================================================;;
(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define CANVAS-CENTER-X (/ CANVAS-WIDTH 2))
(define CANVAS-CENTER-Y (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CONTROLLER-WIDTH 160)
(define CONTROLLER-HEIGHT 60)
(define PART-X-PC 75)
(define PART-Y-PC 50)
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")
(define FONT1 10)
(define FONT2 12)
(define PART-X-VC 50)
(define PART-Y-VC 75)
(define TOP "top")
(define POS-INC 5)
(define NEG-DEC -5)
(define CON-W-XC 150)
(define CON-H-XC 50)
(define WIDTH-XC 200)
(define HEIGHT-XC 50)
(define PART-X-XC 75)
(define PART-Y-XC 25)
(define BLACK "black")
(define RED "red")
(define BLUE "blue")
(define CON-W-YC 50)
(define CON-H-YC 100)
(define HEIGHT-YC 150)
(define PART-X-YC 25)
(define PART-Y-YC 50)
(define ZERO 0)
(define PARTICLE-RADIUS 8)
(define HANDLER-WIDTH 10)
(define RECTANGLE-WIDTH 150)
(define RECTANGLE-HEIGHT 100)
(define TWO 2)
;;============================================================================;;
;;                                END OF FILE                                 ;;
;;============================================================================;;