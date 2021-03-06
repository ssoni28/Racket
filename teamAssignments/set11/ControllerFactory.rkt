;; ControllerFactory.rkt
#lang racket

(require "Interfaces.rkt")
(require "Model.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "XYController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "constants.rkt")
(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")

;;============================================================================;;
;;                              PROVIDE STATEMENTS                            ;;
;;============================================================================;;

(provide ControllerFactory%)

;;============================================================================;;
;;                         ControllerFactory% CLASS                           ;;
;;============================================================================;;

;; A ControllerFactory% is a (new ControllerFactory%
;;                             [w World<%>] [m Model<%>])

(define ControllerFactory%
  (class* object% (SWidget<%>)

    ;;w is the world in which the controllers will live
    (init-field w) ;; World<%>

    ;; m is the model to which the controllers will be connected
    (init-field m) ;; Model<%>

    (super-new)

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the current state of the controller factory to the state
    ;; that should follow the specified key event
    ;; "v" adds a new VelocityController%
    ;; "p" adds a new PositionController%
    ;; "z" adds a new XYController<%
    ;; "x" adds a new XController%
    ;; "y" adds a new YController%
    ;; STRATEGY: Cases on key event
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "v") (add-viewer VelocityController%)]
        [(key=? kev "p") (add-viewer PositionController%)]
        [(key=? kev "z") (add-viewer XYController%)]
        [(key=? kev "x") (add-viewer XController%)]
        [(key=? kev "y") (add-viewer YController%)]
        [else this]))

    ;; add-viewer: -> Void
    ;; GIVEN: a viewer class
    ;; EFFECT: add the newly created viewer-class the list of widgets in the
    ;; world
    (define/public (add-viewer viewer-class)
      (send w add-widget (new viewer-class [model m])))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one
    (define/public (add-to-scene s) s)

    ;; after-tick : -> Void
    ;; GIVEN: no argument 
    ;; EFFECT: updates the current controllerfactory state to the state
    ;; after a tick
    (define/public (after-tick) 'controller-factory-after-tick-trap)

    ;; after-button-down : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position on button down event
    ;; EFFECT: updates the current contollerfactory state to the state after
    ;; button down event. 
    (define/public (after-button-down mx my)
      'controller-factory-after-button-down-trap)

    ;; after-drag : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position
    ;; EFFECT: updates the current controllerfactory state to the state after
    ;; mouse drag event
    (define/public (after-drag mx my)
      'controller-factory-after-drag-trap)

    ;; after-button-up : Int Int -> Void
    ;; GIVEN: x and y coordinates of the mouse position for the button up event
    ;; EFFECT: updates the state of the controllerfactory to the state that
    ;; should be after button-up event
    (define/public (after-button-up mx my)
      'controller-factory-after-button-up-trap)
    ))

;;============================================================================;;
;;                                   TESTS                                    ;;
;;============================================================================;;

;; Examples for testing
;; ----------------------
;; Examples for tests

(begin-for-test
  (local
    [(define MODEL (new Model%))
     (define class-for-testing%
         (class* object% ()
           (super-new)
           (define/public (add-widget s) 'just-for-test-coverage)))
     (define new-fact (new ControllerFactory% [w (new class-for-testing%)]
                           [m MODEL]))]
    (send new-fact after-key-event "v")
    (send new-fact after-key-event "p")
    (send new-fact after-key-event "x")
    (send new-fact after-key-event "y")
    (send new-fact after-key-event "z")
    (send new-fact after-key-event "up")
    (send new-fact after-tick)
    (send new-fact after-button-down 100 100)
    (send new-fact after-button-up 100 100)
    (send new-fact after-drag 100 100)
    (check-equal? (send new-fact add-to-scene EMPTY-CANVAS) EMPTY-CANVAS)))

;;============================================================================;;
;;                                END OF FILE                                 ;;
;;============================================================================;;
