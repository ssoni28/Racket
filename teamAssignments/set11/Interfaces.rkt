;; Interfaces.rkt
#lang racket
(require "PerfectBounce.rkt")

;;============================================================================;;
;;                              PROVIDE STATEMENTS                            ;;
;;============================================================================;;

(provide
 (struct-out particle)
 (struct-out rect)
 particle-after-tick
 World<%>
 SWidget<%>
 Controller<%>
 Model<%>)

;;============================================================================;;
;;                                   INTERFACES                               ;;
;;============================================================================;;

;; World Interface
;; ---------------------

(define World<%>
  (interface ()
    
    ;; SWidget<%> -> Void
    ;; GIVEN: no argument
    ;; EFFECT: adds the widget to the world
    add-widget                         
    
    ;; PosReal -> Void
    ;; GIVEN: a framerate, in secs/tick
    ;; EFFECT: runs this world at the given framerate
    run
    ))

;===============================================================================

;; SWidget Interface
;;--------------------------

(define SWidget<%>
  (interface ()

    ; Scene -> Scene
    ;; GIVEN : a scene
    ;; RETURNS: a scene like the given one with updates from
    ;; objects of stateful widgets
    add-to-scene
    
    ;; after-tick: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this widget to the state it should have
    ;; following a tick.
    after-tick
    
    ;; after-button-down: Nat Nat -> Void
    ;; after-button-up: Nat Nat -> Void
    ;; after-drag: Nat Nat -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the specified mouse event at the given location.
    after-button-up        
    after-button-down      
    after-drag
    
    ;; KeyEvent : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this widget to the state it should have
    ;; following the given key event
    after-key-event        
    ))

;===============================================================================

;; Controller Interface
;;-------------------------

(define Controller<%>    
  (interface (SWidget<%>)
    
    ;; receive-signal: Signal -> Void
    ;; GIVEN: a signal from the model
    ;; EFFECT: adjust the controller accordingly based on the received signal
    receive-signal
    ))

;===============================================================================

;; Model Interface
;;-----------------------

(define Model<%>
  (interface ()
    
    ;; after-tick: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this model to the state it should have
    ;; following a tick.
    after-tick        
    
    ;; register: Controller<%> -> Void
    ;; GIVEN: a controller
    ;; EFFECT: Registers the given controller to receive signal
    register          
    
    ;; execute-command: Command -> Void
    ;; GIVEN: a command
    ;; EFFECT: Executes the given command
    execute-command   
    ))

;===============================================================================
;; protocol: 
;; model sends the controller an initialization signal as soon as it registers.

;;==============================================================================

;;============================================================================;;
;;                               DATA DEFINITIONS                             ;;
;;============================================================================;;

;; DATA DEFINITIONS FOR COMMUNICATING WITH MODEL

;; A Command is one of 
;; -- (make-set-position-x n)
;; -- (make-set-position-y n)
;; -- (make-incr-velocity-x dv)
;; -- (make-incr-velocity-y dv)
;; -- (make-selected? s)

;; A Signal is one of
;; -- (make-report-position-x n)
;; -- (make-report-position-y n)
;; -- (make-report-velocity-x v)
;; -- (make-report-velocity-y v)

;; provide the structs for Command and Signal
;; the syntax of provide in #lang racket has more options in it.
(provide 
 (struct-out set-position-x)
 (struct-out set-position-y) 
 (struct-out incr-velocity-x)
 (struct-out incr-velocity-y)
 (struct-out report-position-x)
 (struct-out report-position-y)
 (struct-out report-velocity-x)
 (struct-out report-velocity-y)
 (struct-out set-selected?)
 )

;; structs for all the commands and signals

(define-struct set-position-x (pos) #:transparent)
;; Interpretation:
;; A set-position-x is a
;; (make-set-position-x Real)
;; pos is the current value of the x coordinate of the position of
;; the particle on the controller

;; TEMPLATE:
;; set-position-x-fn : Set-position-x -> ??
;; (define (set-position-x-fn r)
;; (...(set-position-x-pos r)

(define-struct set-position-y (pos) #:transparent)
;; Interpretation:
;; A set-position-y is a
;; (make-set-position-x Real)
;; pos is the current value of the y coordinate of the position of
;; the particle on the controller

;; TEMPLATE:
;; set-position-y-fn : Set-position-y -> ??
;; (define (set-position-y-fn r)
;; (...(set-position-y-pos r)

(define-struct set-selected? (s) #:transparent)
;; Interpretation:
;; A set-selected? is a
;; (make-set-selected? Boolean)
;; s is the current value of the selected? field of
;; the particle on the controller

;; TEMPLATE:
;; set-selected?-fn : set-selected? -> ??
;; (define (set-selected?-fn r)
;; (...(set-selected?-s r)

(define-struct incr-velocity-x (dv) #:transparent)
;; Interpretation:
;; A incr-velocity-x is a
;; (make-incr-velocity-x Real)
;; dv is the current value of the velocity vx of the particle on the controller

;; TEMPLATE:
;; incr-velocity-x-fn : incr-velocity-x -> ??
;; (define (incr-velocity-x-fn r)
;; (...(incr-velocity-x-dv r)

(define-struct incr-velocity-y (dv) #:transparent)
;; Interpretation:
;; A incr-velocity-y is a
;; (make-incr-velocity-y Real)
;; dv is the current value of the velocity vy of the particle on the controller

;; TEMPLATE:
;; incr-velocity-y-fn : incr-velocity-y -> ??
;; (define (incr-velocity-y-fn r)
;; (...(incr-velocity-y-dv r)

(define-struct report-position-x (pos) #:transparent)
;; Interpretation:
;; A report-position-x is a
;; (make-report-position-x Real)
;; pos is the x coordinate of the position of
;; the particle on the controller

;; TEMPLATE:
;; report-position-x-fn : report-position-x -> ??
;; (define (report-position-x-fn r)
;; (...(report-position-x-pos r)

(define-struct report-position-y (pos) #:transparent)
;; Interpretation:
;; A report-position-y is a
;; (make-report-position-y Real)
;; pos is the y coordinate of the position of
;; the particle on the controller

;; TEMPLATE:
;; report-position-y-fn : report-position-y -> ??
;; (define (report-position-y-fn r)
;; (...(report-position-y-pos r)

(define-struct report-velocity-x (vx) #:transparent)
;; Interpretation:
;; A report-position-x is a
;; (make-report-position-x Real)
;; vx is the x direction velocity of the particle on the controller

;; TEMPLATE:
;; report-position-x-fn : report-position-x -> ??
;; (define (report-position-x-fn r)
;; (...(report-position-x-vx r)

(define-struct report-velocity-y (vy) #:transparent)
;; Interpretation:
;; A report-position-y is a
;; (make-report-position-y Real)
;; vy is the y direction velocity of the particle on the controller

;; TEMPLATE:
;; report-position-y-fn : report-position-y -> ??
;; (define (report-position-y-fn r)
;; (...(report-position-y-vy r)
;===============================================================================

;; Template for ListOfController<%>
;; INTERPRETATION:
;; A ListOfController<%> is one of:
;; -- empty
;; -- (cons Controller<%> ListOfController<%>)

;; TEMPLATE:
;; loc-fn: ListOfController<%> -> ??
;; (define (loc-fn loc)
;;   (cond
;;    [(empty? loc) ...]
;;    [else (... 
;;            (controller-fn(first loc))
;;            (loc-fn (rest loc)))]))

;;============================================================================;;
;;                                END OF FILE                                 ;;
;;============================================================================;;