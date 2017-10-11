;; Model.rkt

;; Model consists of five controllers and a particle bouncing inside a rectangle
;; of 150X100. It accepts commands and report signals to the controllers when
;; their status changes.

#lang racket
(require "extras.rkt")
(require "Interfaces.rkt")
(require "constants.rkt")
(require rackunit)

;;============================================================================;;
;;                              PROVIDE STATEMENTS                            ;;
;;============================================================================;;
(provide Model%)

;;============================================================================;;
;;                                Model%  CLASS                               ;;
;;============================================================================;;
;; A Model% is (new Model% [xmin Real] [xmax Real] [ymin real] [ymax real]
;;                         [x Real] [y Real] [vx real] [vy Real]
;;                        [selected? Boolean] [controllers ListOfController<%>])

(define Model%
  (class* object%
    (Model<%>) ;; Includes all methods of interface Model<%>
    
    ;; xmin and xmax are the minimum and the maxmium values of the bounding
    ;; dimensions of x direction of the controller of size 150X100
    (field [xmin ZERO]) ;; Real
    (field [xmax RECTANGLE-WIDTH]) ;; Real
    
    ;; ymin and ymax are the minimum and the maxmium values of the bounding
    ;; dimensions of y direction of the controller of size 150X100
    (field [ymin ZERO]) ;; Real
    (field [ymax RECTANGLE-HEIGHT]) ;; Real
    
    ;; the position of the particle
    ;; the x and y are the coordinates of the position of
    ;; the particle
    (init-field [x (/ (+ xmin xmax) TWO)]) ;; Real
    (init-field [y (/ (+ ymin ymax) TWO)]) ;; Real
    
    ;; the vx and vy of the particle give the velocities in the x and y
    ;; directions, respectively
    (init-field [vx ZERO]) ;; Real
    (init-field [vy ZERO]) ;; Real
    
    ;; is the object selected? Default is false
    (init-field [selected? false]) ;; Boolean
    
    ; ListOfController<%>
    (init-field [controllers empty]) ;; ListOfController<%>
    
    (super-new)
    
    ;; after-tick: -> Void
    ;; GIVEN: no argument
    ;; EFFECT: updates the current model state to the state
    ;; after a tick
    ;; moves the object by vx and vy in the x and y directions,respectively
    ;; reports x,y,vx,vy at ever tick
    ;; it calls the particle-after-tick function from PerfectBounce.rkt
    (define/public (after-tick)
      (local
        ;; new-particle is the particle state after the tick which calls
        ;; particle-after-tick function of PerfectBounce with a particle
        ;; and a rectangle as its arguments
        ((define new-particle
           (particle-after-tick
            (make-particle x y vx vy) 
            (make-rect xmin xmax ymin ymax))))
        (if selected?
            5555
            (begin (set! x (particle-x new-particle))
                   (publish-position-x)
                   (set! y (particle-y new-particle))
                   (publish-position-y)
                   (set! vx (particle-vx new-particle))
                   (publish-velocity-x)
                   (set! vy (particle-vy new-particle))
                   (publish-velocity-y)))))
    
    ;; register: Controller -> Void
    ;; GIVEN: a controller
    ;; EFFECT: register the new controller and send it some data
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position-x x))
        (send c receive-signal (make-report-position-y y))
        (send c receive-signal (make-report-velocity-x vx))
        (send c receive-signal (make-report-velocity-y vy))))
    
    ;; execute-command: Command -> Void
    ;; GIVEN: a command to be executed
    ;; EFFECT: decodes the command, executes it, and sends updates to the
    ;; controllers. 
    (define/public (execute-command cmd)
      (cond
        [(set-position-x? cmd)
         (begin
           (set! x (set-position-x-pos cmd))
           (publish-position-x))]
        [(set-position-y? cmd)
         (begin
           (set! y (set-position-y-pos cmd))
           (publish-position-y))]
        [(incr-velocity-x? cmd)
         (begin
           (set! vx (+ vx (incr-velocity-x-dv cmd)))
           (publish-velocity-x))]
        [(incr-velocity-y? cmd)
         (begin
           (set! vy (+ vy (incr-velocity-y-dv cmd)))
           (publish-velocity-y))]
        [(set-selected? cmd)
         (begin
           (set! selected? (set-selected?-s cmd)))]))
    
    
    ;; publish-position-x: -> Void
    ;; GIVEN: no argument
    ;; EFFECT: report x position of the particle to each controller
    (define/public (publish-position-x)
      (let ((msg (make-report-position-x x)))
        (for-each
         ;; Controller -> Void
         ;; GIVEN: a controller
         ;; EFFECT: notify the new x position of the particle to the controller
         (lambda (obs) (send obs receive-signal msg))
         controllers)))
    
    ;; publish-position-y: -> Void
    ;; GIVEN: no argument
    ;; EFFECT: report y position of the particle to each controller
    (define/public (publish-position-y)
      (let ((msg (make-report-position-y y)))
        (for-each
         ;; Controller -> Void
         ;; GIVEN: a controller
         ;; EFFECT: notify the new y position of the particle to the controller
         (lambda (obs) (send obs receive-signal msg))
         controllers)))
    
    ;; publish-velocity-x: -> Void
    ;; GIVEN: no argument
    ;; EFFECT: report velocity of the particle in x direction to each controller
    (define/public (publish-velocity-x)
      (let ((msg (make-report-velocity-x vx)))
        (for-each
         ;; Controller -> Void
         ;; GIVEN: a controller
         ;; EFFECT: notify the new vx velocity of the particle to the controller
         (lambda (obs) (send obs receive-signal msg))
         controllers)))
    
    ;; publish-velocity-y: -> Void
    ;; GIVEN: no argument
    ;; EFFECT: report velocity of the particle in y direction to each controller
    (define/public (publish-velocity-y)
      (let ((msg (make-report-velocity-y vy)))
        (for-each
         ;; Controller -> Void
         ;; GIVEN: a controller
         ;; EFFECT: notify the new vy velocity of the particle to the controller
         (lambda (obs) (send obs receive-signal msg))
         controllers)))
    
    ;; -> Real Real Real Real Boolean
    (define/public (for-test:particle-data) (list x y vx vy selected?))
    ))

;;============================================================================;;
;;                                 TESTS                                      ;;
;;============================================================================;;

(begin-for-test
  (local
    [(define MODEL (new Model%))
     (define MODEL1 (new Model% [selected? true]))
     (define PARTICLE
       (particle-after-tick
        (make-particle 75 50 0 0) 
        (make-rect 0 150 0 100)))
     ]
    (send MODEL after-tick)
    (check-equal? (send MODEL for-test:particle-data) (list 75 50 0 0 false))
    (send MODEL1 after-tick)
    (check-equal? (send MODEL1 for-test:particle-data) (list 75 50 0 0 true))
    (send MODEL execute-command (make-incr-velocity-x 5))
    (check-equal? (send MODEL for-test:particle-data) (list 75 50 5 0 false))
    (send MODEL execute-command (make-incr-velocity-y -5))
    (check-equal? (send MODEL for-test:particle-data) (list 75 50 5 -5 false))
    (send MODEL execute-command (make-set-position-x -5))
    (check-equal? (send MODEL for-test:particle-data) (list -5 50 5 -5 false))
    (send MODEL execute-command (make-set-position-y 155))
    (check-equal? (send MODEL for-test:particle-data) (list -5 155 5 -5 false))
    (send MODEL execute-command (make-set-selected? true))
    (check-equal? (send MODEL for-test:particle-data) (list -5 155 5 -5 true))
    (local
      [(define class-for-testing%
         (class* object% ()
           (super-new)
           (define/public (receive-signal s) 'just-for-test-coverage)))]
      (send MODEL register (new class-for-testing%))
      (send MODEL publish-position-x)
      (send MODEL publish-position-y)
      (send MODEL publish-velocity-x)
      (send MODEL publish-velocity-y)
      (check-equal?
       (send MODEL for-test:particle-data) (list -5 155 5 -5 true))))
  )
;;============================================================================;;
;;                                END OF FILE                                 ;;
;;============================================================================;;