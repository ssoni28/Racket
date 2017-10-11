;; cubelets.rkt

;;============================================================================;;
;;                                      GOAL                                  ;;
;;============================================================================;;

;; Implement a toy with the following description:
;; The toy consists of a canvas that is 600 pixels high and 500 pixels wide.
;; When the child types "b", a new block pops up on the screen at the
;; location of the last button-down or button-up. The block appears as a
;; 20x20 outline square. The square is initially green. If the child
;; types a "b" before the first button-down or button-up event, then
;; the first block appears in an unspecified but fixed place on the canvas.
;; A block does not move by itself, but the child can move it around
;; using Smooth Drag. When the block is selected, it appears as red rather
;; than green.
;; If a block is dragged so that it contacts or overlaps another block,
;; the two blocks become connected. We say that the blocks are teammates..
;; The property of being a teammate is symmetric and transitive. So if
;; block A is moved to touch block B, then a new team is formed consisting
;; of A and all its teammates, and B and all its teammates.
;; Two blocks overlap if they intersect at any point. For this purpose, the
;; edges of the block are considered part of the block.
;; Once two blocks become teammates, they remain teammates forever.
;; When a block is moved, all its teammates move along with it. If A and B
;; are teammates, and A is dragged in some direction, then B moves the same way.
;; Only the selected block accumulates teammates. If A is being dragged, and
;; B is a teammate of A, and A's motion causes B to come into contact with C,
;; C does not become a teammate of A and B. In the video below, we call the
;; selected block the "leader." But you can drag a team by selecting any block
;; in the team, so the leader may be different on different drags.
;;============================================================================;;

;; HOW TO RUN THE PROGRAM
;; ------------------------
;; start with
;; (initial-world 0.5)

#lang racket
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

;;============================================================================;;
;;                              INTERFACES                                    ;;
;;============================================================================;;

(require "block-interfaces.rkt")

;;============================================================================;;
;;                               CLASSES                                      ;;
;;============================================================================;;

(require "block-class.rkt")

;;============================================================================;;
;;                           PROVIDE STATEMENTS                               ;;
;;============================================================================;;

(provide
 initial-world
 make-block
 Block<%>
 Block%
 BlockFactory<%>
 BlockFactory%)
  
;;============================================================================;;
;;                                     CLASS                                  ;;
;;============================================================================;;

;; The BlockFactory% class
;; -----------------------
;; A blockfactory is a (new BlockFactory% [world WorldState%]
;;                                        [blocks LisOfBlock<%>])

(define BlockFactory%
  (class* object%(BlockFactory<%>)
    
    (init-field world) ;; the world to which the blocks will be added

    ;; blocks represents the list of the blocks in the blockfactory
    (init-field [blocks empty]) ;; ListOfBlock<%>

    ;; button-down-x and button-down-y are the coordinates of the mouse
    ;; event which is initially set to (0,0)
    (field [button-down-x SEL-INIT-X]) ;; Int
    (field [button-down-y SEL-INIT-Y]) ;; Int

    (super-new)
    
    ;; after-tick : -> Void
    ;; GIVEN: no argument 
    ;; EFFECT: updates the current state of the BlockFactory<%> to the state
    ;; that should be after tick. Doesnt change any fields of the blockfactory
    ;; EXAMPLE: Call the after-tick method on the blockfactory object
    ;; verify any of the comparable data
    ;; (send BLOCK-FACTORY after-tick)
    ;; (check-equal? (send BLOCK-FACTORY for-test:get-button-down-x) 0)
    (define/public (after-tick) 60)

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the state of the blockfactory that should follow the
    ;; specified key event
    ;; EXAMPLE: Call after-key-event on the blockfactory object and verify
    ;; any of the comparable data
    ;; (send BLOCK-FACTORY after-key-event "b")
    ;; (check-equal? (send BLOCK-FACTORY for-test:get-button-down-x) 70)
    ;; STRATEGY: Cases on key event kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "b") (add-block-to-factory)]
        [else this]))

    ;; add-block-to-factory: -> Void
    ;; GIVEN: no argument
    ;; EFFECT: updates the list of blocks and list of stateful objects with
    ;; the new block created. Conditions as below:
    ;; at the default position (center of the canvas in our case) if there 
    ;; is no prior button down/button up event occurred
    ;; or at the position of the last button down event
    ;; STRATEGY: Cases on whether a button-down event occured in
    ;; the blockfactory
    (define (add-block-to-factory)
      (if (and
           (equal? button-down-x SEL-INIT-X)
           (equal? button-down-y SEL-INIT-Y))
          (add-block-to-list-and-world-defpos)
          (add-block-to-list-and-world)))

    ;; add-block-to-list-and-world-defpos: -> Void
    ;; GIVEN: no argument
    ;; EFFECT: adds the new block created at the default position in the canvas
    ;; to the stateful-widgets of the world as well as to the list of the blocks
    ;; maintained in the blockfactory
    (define/public (add-block-to-list-and-world-defpos)
      (send world add-stateful-widget (add-block-to-blocks-list
                                       (make-block
                                        CENTER-X
                                        CENTER-Y
                                        (get-blocks)))))

    ;; add-block-to-blocks-list: Block<%>-> Void
    ;; GIVEN: the newly created block in the blockfactory
    ;; EFFECT: adds the current block to the list of blocks
    (define/public (add-block-to-blocks-list block)
      (begin
        (set! blocks (cons block blocks))
        (for-each
         ;; Block<%> -> Void
         ;; GIVEN: A block from the list of blocks in the blockfactory
         ;; EFFECT: Adds the given bock to the list of blocks
         (lambda (obj)
           (send obj add-block-to-block-list block))
         blocks)
        block))
    
    ;; add-block-to-list-and-world: -> Void
    ;; GIVEN: no argument
    ;; EFFECT: adds the current block to the list of stateful widgets of the
    ;; world
    (define/public (add-block-to-list-and-world)
      (send world add-stateful-widget (add-block-to-blocks-list
                                       (make-block
                                        button-down-x
                                        button-down-y
                                        (get-blocks)))))
    

    ;; after-button-down : Int Int -> Void
    ;; GIVEN: x and y coordinates of mouse position  on button down event
    ;; EFFECT: updates the blockfactory state to the state that should be
    ;; after button down mouse event
    ;; EXAMPLE: See tests below
    (define/public (after-button-down mx my)
      (begin
        (set! button-down-x mx)
        (set! button-down-y my)))

    ;; after-button-up : Int Int -> Void
    ;; GIVEN: x and y coordinates of the mouse position for the button up event
    ;; EFFECT: updates the state of the blockfactory that should be after the
    ;; button up event
    ;; EXAMPLE: See tests below
    (define/public (after-button-up mx my) 60)

    ;; after-drag : Int Int -> Void
    ;; GIVEN: x and y coordinates of the mouse position for the mouse drag event
    ;; EFFECT: updates the state of the blockfactory that should be after the
    ;; drag event
    ;; EXAMPLE: See tests below
    (define/public (after-drag mx my) 60)

    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene that depicts this BlockFactory<%>
    ;; EXAMPLE: (send BLOCK-FACTORY add-to-scene EMPTY-CANVAS)->EMPTY-CANVAS
    (define/public (add-to-scene s) s)

    ;; RETURNS:the list of the blocks in the BlockFactory<%>
    (define/public (get-blocks) blocks) ;; ListOfBlock<%>

    ;; Returns the x coordinate of the button down event
    ;; -> Int
    (define/public (for-test:get-button-down-x) button-down-x)
    ))

;;============================================================================;;
;;                           initial-world                                    ;;
;;============================================================================;;

;; initial-world: PosInt -> WorldState<%>
;; GIVEN: the framerate value
;; RETURNS: the world with a empty canvas and no blocks initially
(define (initial-world frameRate)
  (local
    (;; Create a new world with the specified canvas width and height
     (define the-world (make-world CANVAS-WIDTH CANVAS-HEIGHT))
     ;; Create a new BlockFactory<%> with the world defined above
     (define the-blockfactory
       (new BlockFactory% [world the-world])))
    (begin
      ;; Add the blockfactory to the stateful widget of the world
      (send the-world add-stateful-widget the-blockfactory)
      ;; Call the run method of the world with the framerate
      (send the-world run frameRate))))

;;============================================================================;;
;;                                  TESTS                                     ;;
;;============================================================================;;

;; Examples for testing
;; ---------------------
(define BLOCK-FACTORY
  (local ((define the-world (make-world 500 600)))
  (new BlockFactory% [world the-world])))

(begin-for-test
 (send BLOCK-FACTORY after-tick)
 (check-equal? (send BLOCK-FACTORY for-test:get-button-down-x) 0)
 (send BLOCK-FACTORY after-button-up 100 40)
 (check-equal? (send BLOCK-FACTORY for-test:get-button-down-x) 0)
 (send BLOCK-FACTORY after-drag 100 40)
 (check-equal? (send BLOCK-FACTORY for-test:get-button-down-x) 0)
 (send BLOCK-FACTORY after-key-event "b")
 (check-equal? (length(send BLOCK-FACTORY get-blocks)) 1)
 (send BLOCK-FACTORY after-button-down 70 80)
 (send BLOCK-FACTORY after-key-event "b")
 (check-equal? (send BLOCK-FACTORY for-test:get-button-down-x) 70)
 (send BLOCK-FACTORY after-key-event "n")
 (check-equal? (length(send BLOCK-FACTORY get-blocks)) 2)
 (check-equal? (send BLOCK-FACTORY add-to-scene EMPTY-CANVAS) EMPTY-CANVAS))

