;; block-class.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "block-interfaces.rkt")
(require "sets.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

;;============================================================================;;
;;                           PROVIDE STATEMENTS                               ;;
;;============================================================================;;

(provide
 Block%
 make-block)

;;============================================================================;;
;;                                     CLASS                                  ;;
;;============================================================================;;

;; Block% Class
;; -------------

;; A block is a 20X20 square which does not move on its own, but
;; moves when the user does a smooth drag on it.It is selectable
;; and draggable.
;; A block is a (new Block% [x PosInt] [y PosInt] [blocks LisOfBlock<%>]
;;                          [selected? Boolean] [block-sel-x Int]
;;                          [block-sel-y Int])

(define Block%
  (class* object%(Block<%>)
    
    ;; x and y represents the coordinates of the center of the block
    ;; blocks represent the list of blocks in the given block factory
    (init-field x y) ;; Int
    (init-field blocks) ;; ListOfBlock<%>

    ;; is the block selected? Default is false.
    (init-field [selected? false]) ;; Boolean

    ;; block-sel-x and block-sel-y represents the position of
    ;; the last button-down event inside the block relative to the block's
    ;; center.(if the block is selected).  Else any value.
    (init-field [block-sel-x SEL-INIT-X]) ;; Int
    (init-field [block-sel-y SEL-INIT-Y]) ;; Int

    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    ;; add a new field,the block's side length initialized to 20
    (field [side SIDE]) ;; PosInt

    ;; is the block following the leader block? Default is false.
    (field [following-leader false]) ;; Boolean

    ;; subscribers represent the list of the blocks which get connected
    ;; to the current block when its dragged and overlaps any block in the
    ;; world. Initialised to empty by default
    (field [subscribers empty]) ;; ListOfBlock<%>

    ;; team represents the list of the blocks which gets added as a teammate
    ;; to the current block. Initialized to empty by default.
    (field [team empty]) ;; ListOfBlock<%>

    ;; moved represents whether the block has been already moved. Set to false
    ;; by default
    (field [moved false]) ;; Boolean
   
    (super-new)

    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates the current state of the block to the state that
    ;; should be after a tick
    ;; Doesnt change any fields of the block
    ;; EXAMPLE: Call after-tick method by the object of the BLOCK% class
    ;; and verify any of the comparbale fields of the block
    ;; (send BLOCK1 after-tick)
    ;; (check-equal? (send BLOCK1 block-x) 230)
    (define/public (after-tick) 50)

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the current state of the block to the state that
    ;; should be after the given key event.
    ;; Doesnt change any fields of the block
    ;; EXAMPLE: Call after-key-event method by the object of the BLOCK% class
    ;; and verify any of the comparable fields of the block
    ;; (send BLOCK1 after-key-event "b")
    ;; (check-equal? (send BLOCK1 block-y) 40)
    (define/public (after-key-event kev) 50)

    ;; after-button-down : Int Int -> Void
    ;; GIVEN: the location of mouse in button-down event
    ;; EFFECT: updates the current state of the block to the state that
    ;; should be after button down event
    ;; updates the list of subscribers of the current block, sets the mouse
    ;; position to the new vallue of mx and my and marks the block selected
    ;; EXAMPLE: Call after-button-down method by the block object and verify 
    ;; any of the comparable fields
    ;; (send BLOCK1 after-button-down 235 45)
    ;; (check-equal? (send BLOCK1 is-selected) true)
    ;; STRATEGY: Cases on whether the new mouse position is inside the block
     (define/public (after-button-down mx my)
      (if (in-block? mx my) 
          (begin
            (set! selected? true)
            (set! block-sel-x mx)
            (set! block-sel-y my)
            (set-subscribers))
          50))
  
    ;; after-button-up : Int Int -> Void
    ;; GIVEN: the location of a mouse
    ;; EFFECT: updates the current state of the block to the state that
    ;; should be after button up event.
    ;; updates the subscribers of the block,marks the moved field to false
    ;; and unselects the block. Also update the team of teammates to include
    ;; the leader and leader's team.
    ;; EXAMPLE: Call after-button-up method by the block object and verify
    ;; the result by checking any of the comparable fields of the block
    ;; (send BLOCK1 after-button-up 280 40)
    ;; (check-equal? (send BLOCK1 is-selected) false)
    (define/public (after-button-up mx my)
      (begin
        (set! subscribers empty)
        (set! moved false)
        (if selected?
            (begin
              (for-each
               ;; Block<%> -> Void
               ;; GIVEN: a block which is the teammate of the leader
               ;; EFFECT: adds the leader and its team to the team of
               ;; the given teammate
               (lambda(obj)
                 (send obj set-teammates (set-cons this team)))
               team)
              (set! selected? false))
            (set! selected? false))))
    
    ;; after-drag : Int Int -> Void
    ;; GIVEN : x and y coordinates of mouse of the drag event
    ;; EFFECT: updates the current state of the block to the state that
    ;; should be after button drag event
    ;; EXAMPLE: Call after-drag method on block object and verify any of the
    ;; comparable fields
    ;; (send BLOCK1 after-drag 300 210)
    ;; (check-equal? (send BLOCK1 block-x) 295)
    ;; STRATEGY: Cases on whether the block is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (for-each
             ;; Block<%> -> Void
             ;; GIVEN: a block which is a teammate of the leader
             ;; EFFECT: sets moved field of all the teammates
             ;; of leader to false
             (lambda(obj)
               (send obj set-not-moved))
             team)
            (local ((define move-diff-x (- mx block-sel-x)) 
                    (define move-diff-y (- my block-sel-y)))
              
              (set! x (+ x (- mx block-sel-x)))
              (set! y (+ y (- my block-sel-y)))
              (set! selected? true)
              (set! block-sel-x mx)   
              (set! block-sel-y my)
              (set-subscribers)
              (publish x y)
              (move-teammates move-diff-x move-diff-y)))
          50))

    ;; get-team: -> ListOfBlock<%>
    ;; GIVEN: no argument
    ;; RETURNS: the teammates of this block
    (define/public (get-team) team)
    
    ;; add-teammate: Block<%> -> Void
    ;; GIVEN: a block
    ;; EFFECT: adds the given block to this block's team
    (define/public (add-teammate block) (set! team (set-cons block team)))
    
    ;; add-block-to-block-list: Block<%>->Void
    ;; GIVEN: a block
    ;; EFFECT: adds the given block to the block list of this block
    (define/public (add-block-to-block-list block)
      (set! blocks (set-cons block blocks)))
    
    ;; block-x: -> Int
    ;; GIVEN: no argument
    ;; RETURNS: the x coordinate of the center of this block
    (define/public (block-x) x)
    
    ;; block-x: -> Int
    ;; GIVEN: no argument
    ;; RETURNS: the y coordinate of the center of this block
    (define/public (block-y) y)
    
    ;; is-selected : -> Boolean
    ;; GIVEN: no argument
    ;; RETURNS: the selected? value of this block
    (define/public (is-selected) selected?)
    
    ;; set-subscribers: -> Void
    ;; GIVEN: no argument
    ;; EFFECT: updates the list of subscribers of this block
    ;; as and when the this block intersects with any other block 
    ;; present in the blockfactory while being dragged
    (define/public (set-subscribers) 
      (set! subscribers
            (filter
             ;; ListofBlock<%> -> ListofBlock<%>
             ;; GIVEN: a list of all the blocks present on the canvas
             ;; RETURN: a list of blocks except the leader block
             ;; which is selected
             (lambda(obj)
               (not (send obj is-selected)))
             blocks)))
    
    ;; set-not-moved: -> Void
    ;; GIVEN: no argument
    ;; EFFECT: updates the moved field of the block to false
    (define/public (set-not-moved)
      (set! moved false))

    ;; move-teammates: Int Int -> Void
    ;; GIVEN: the displacements by which the teammates have to be moved during
    ;; a drag event
    ;; EFFECT: moves the teammates of this block along with this block
     (define/public (move-teammates move-x move-y) 
       (if (empty? team) 
           this
           (for-each
            ;; Block<%> -> Void
            ;; GVIEN: a block which is a team mate of leader
            ;; EFFECT: the given teammate is moved with the leader
            (lambda(obj)
              (send obj move-by-leader move-x move-y this))
            team)))
    
    ;; publish: PosInt PosInt -> Void
    ;; GIVEN: the values of the coordinates of the center of the block
    ;; EFFECT: for each of the subscribers of the current block, check if the
    ;; leader block intersects with it , if so connect it to the leader block
    (define/public (publish x y)
      (for-each
       ;; Block<%> -> Void
       ;; GIVEN: a block which is present on the canvas
       ;; EFFECT: calls the leader-moved function on all
       ;; the blocks present on the canvas to check
       ;; whether they insect leader or not
       (lambda(obj)
         (send obj leader-moved x y this))
       subscribers))

    ;; leader-moved: Int Int Block<%> -> Void
    ;; GIVEN: the x and y coordinates of the leader block and the leader block
    ;; itself
    ;; EFFECT: checks if the leader block intersects with any of the blocks in
    ;; the list of subscribers, that is, the blocks present on the canvas. If
    ;; it intersects, add the block to leader's team and also add the teammates,
    ;; if any, of the intersecting block to the team of the leader.
    (define/public (leader-moved lead-x lead-y leader)
      (if (intersect? lead-x lead-y)
          (begin
            (send leader add-teammate this)
            (for-each
             ;; Block<%> -> Void
             ;; GIVEN: a block which is the team mate of this block
             ;; EFFECT: adds the teammates of intersecting block to the
             ;; current leader's team
             (lambda(obj) (send leader add-teammate obj))
             team)
            )
          50))

    ;; intersect? : PosInt PosInt -> Boolean
    ;; GIVEN: the x and y coordinates of the leader block
    ;; RETURNS: a boolean value based on whether the current block
    ;; intersects/overlaps with the leader block
    (define (intersect? l-x l-y)  
      (not (or 
            (or ( < (+ l-x LEFT-BOUNDARY) (- x LEFT-BOUNDARY))
                ( > (- l-x LEFT-BOUNDARY) (+ x LEFT-BOUNDARY)))
            (or ( < (+ l-y TOP-BOUNDARY) (- y TOP-BOUNDARY))
                ( > (- l-y TOP-BOUNDARY) (+ y TOP-BOUNDARY))))))

    ;; set-teammates: ListOfBlock<%> -> Void
    ;; GIVEN: a list of block that represents a team of the leader block
    ;; EFFECT: updates the team field of this block
    (define/public (set-teammates teammates)
      (begin (set! team teammates)))

    ;; move-by-leader: Int Int Block<%> -> Void
    ;; GIVEN: the displacements in the x and y directions by which the current
    ;; block has to be moved on drag event and the leader block
    ;; EFFECT: updates the x and y fields of this block when this block is
    ;; being dragged by the leader and marks the moved field to true
    ;; STRATEGY: Divide into cases if this block is not selected and not
    ;; being moved so far
    (define/public (move-by-leader move-x move-y leader)
      (if (and (not selected?) (not moved))
          (begin
            (set! x (+ x move-x)) 
            (set! y (+ y move-y))
            (set! moved true))
          50))

    ;; in-block? : Int Int -> Boolean
    ;; GIVEN: the x and y coordinates of the mouse position
    ;; RETURNS: true iff the given new coordinates is inside the bounding box of
    ;; the given canvas.
    (define (in-block? other-x other-y)
      (and (<= (- x LEFT-BOUNDARY) other-x (+ x LEFT-BOUNDARY))
           (<= (- y TOP-BOUNDARY) other-y (+ y TOP-BOUNDARY))))

    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with the block
    ;; painted on it.
    ;; EXAMPLE:
    ;; (send BLOCK1 add-to-scene EMPTY-CANVAS) ->
    ;; (place-image UNSELECTED_BLOCK 295 205 EMPTY-CANVAS)
    ;; STRATEGY: Cases on whether the block is selected
    (define/public (add-to-scene canvas)
      (if selected?
          (place-image SELECTED_BLOCK x y canvas)
          (place-image UNSELECTED_BLOCK x y canvas)))
    ))

;;============================================================================;;
;;                             make-block                                     ;;
;;============================================================================;;

;; make-block : NonNegInt NonNegInt ListOfBlock<%> -> Block<%>
;; GIVEN: an x and y position, and a list of blocks
;; WHERE: the list of blocks is the list of blocks already on the playground.
;; RETURNS: a new block, at the given position, with no teammates
(define (make-block x y blocks)
  (new Block% [x x] [y y] [blocks blocks]))

;;============================================================================;;
;;                                  TESTS                                     ;;
;;============================================================================;;

;; Examples for testing
;; ---------------------
(define BLOCK1 (make-block 230 40 empty))
(define BLOCK2 (make-block 250 30 empty))
(define BLOCK3 (new Block% [x 270] [y 100] [blocks (list BLOCK1 BLOCK2)]
                    [selected? true]))
(define BLOCK4 (make-block 230 40 empty))
(define BLOCK5 (make-block 250 30 empty))
(define BLOCK6 (new Block% [x 30] [y 40] [blocks (list BLOCK4 BLOCK5)]
                    [selected? true]))
(define block-list1 (list BLOCK1))
(define block-list2 (list BLOCK1 BLOCK2))

;; TEST CASES
(begin-for-test
  (send BLOCK2 add-teammate BLOCK3)
  (check-equal? (length(send BLOCK2 get-team)) 1)
  (send BLOCK2 after-drag 90 80)
  (check-equal? (send BLOCK2 is-selected) false)
  (send BLOCK2 after-button-up 100 100)
  (send BLOCK3 set-subscribers)
  (send BLOCK3 publish 270 100)
  (check-equal? (send BLOCK3 is-selected) true)
  (check-equal? (send BLOCK2 is-selected) false)
  (send BLOCK2 after-button-down 255 35)
  (send BLOCK2 after-drag 275 105)
  (check-equal? (length(send BLOCK2 get-team)) 1)
  (send BLOCK2 after-button-up 90 80)
  (check-equal? (send BLOCK2 is-selected) false))

(begin-for-test
  (send BLOCK1 after-tick)
  (check-equal? (send BLOCK1 block-x) 230)
  (send BLOCK1 after-key-event "b")
  (check-equal? (send BLOCK1 block-y) 40)
  (send BLOCK1 after-button-down 235 45)
  (check-equal? (send BLOCK1 is-selected) true)
  (check-equal? (send BLOCK1 add-to-scene EMPTY-CANVAS)
                 (place-image SELECTED_BLOCK 230 40 EMPTY-CANVAS))
  (send BLOCK1 after-button-down 280 80)
  (check-equal? (send BLOCK1 block-x) 230)
  (send BLOCK1 after-drag 300 210)
  (check-equal? (send BLOCK1 block-x) 295)
  (send BLOCK1 after-button-up 280 40)
  (check-equal? (send BLOCK1 is-selected) false)
  (send BLOCK1 after-drag 310 200)
  (check-equal? (send BLOCK1 block-y) 205)
  (check-equal? (send BLOCK1 add-to-scene EMPTY-CANVAS)
                (place-image UNSELECTED_BLOCK 295 205 EMPTY-CANVAS)))

(begin-for-test
  (send BLOCK1 add-block-to-block-list BLOCK2)
  (send BLOCK1 set-teammates block-list2)
  (check-equal? (length(send BLOCK1 get-team)) 2)
  (send BLOCK5 add-teammate BLOCK3)
  (send BLOCK6 after-drag 200 0)
  (check-equal? (length(send BLOCK6 get-team)) 3))