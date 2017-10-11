;; ControllerHooks.rkt
;; This interface has been inherited by all the five controllers.
#lang racket

;;============================================================================;;
;;                              PROVIDE STATEMENTS                            ;;
;;============================================================================;;

(provide ControllerHooks<%>)

;;============================================================================;;
;;                                   INTERFACES                               ;;
;;============================================================================;;

;; ControllerHooks Interface
;; ---------------------------

(define ControllerHooks<%>
  (interface()

    ;; Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position on button down event
    ;; EFFECT: updates the current contoller state to the state after button
    ;; down event. If the viewer is selected, updates the mouse coordinates and
    ;; marks the viewer as selected.
    button-down

    ;; Int Int -> Void
    ;; GIVEN: x and y coordinates of the mouse position for the button up event
    ;; EFFECT: updates the state of the controller with the handler and
    ;; controller unselected.
    button-up

    ;; Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position
    ;; EFFECT: updates the current controller state to the state after mouse
    ;; drag event
    drag

    ;; KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the current position controller state to the state
    ;; that should follow the specified key event
    key-event

    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with the position controller
    ;; painted on it.
    viewer-image
    ))
;;============================================================================;;
;;                                END OF FILE                                 ;;
;;============================================================================;;