;; block-interfaces.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

;;============================================================================;;
;;                           PROVIDE STATEMENTS                               ;;
;;============================================================================;;

(provide
 CANVAS-WIDTH
 CANVAS-HEIGHT
 EMPTY-CANVAS
 UNSELECTED_BLOCK
 SIDE
 SELECTED_BLOCK
 RIGHT-BOUNDARY
 LEFT-BOUNDARY
 TOP-BOUNDARY
 SEL-INIT-X
 SEL-INIT-Y
 CENTER-X
 CENTER-Y
 Block<%>
 BlockFactory<%>)

;;============================================================================;;
;;                               CONSTANTS                                    ;;
;;============================================================================;;

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define UNSELECTED_BLOCK (square 20 "outline" "green"))
(define SIDE 20)
(define SELECTED_BLOCK (square SIDE "outline" "red"))
(define RIGHT-BOUNDARY (- CANVAS-WIDTH (/ SIDE 2)))
(define LEFT-BOUNDARY (/ SIDE 2))
(define TOP-BOUNDARY (/ SIDE 2))
(define SEL-INIT-X 0)
(define SEL-INIT-Y 0)
(define CENTER-X (/ CANVAS-WIDTH 2))
(define CENTER-Y (/ CANVAS-HEIGHT 2))

;;============================================================================;;
;;                             DATA DEFINITIONS                               ;;
;;============================================================================;;
;; A BlockFactory is an object whose class implements BlockFactory<%>

;; A Block is an object whose class implements Block<%>

;; A Block is a
;; (make-block NonNegInt NonNegInt ListOfBlock<%>)

;; INTERP:
;; (make-block x y blocks)
;; represents a block state containing the
;; x - the x coordinate of the center of block on the canvas
;; y - the y coordinate of the center of block on the canvas
;; blocks- the list of blocks present on the canvas

;; Template for ListOfBlock<%>
;; INTERPRETATION:
;; A ListOfBlock<%> is one of:
;; -- empty
;; -- (cons Block<%> ListOfBlock<%>)

;; TEMPLATE:
;; lob-fn: ListOfBlock<%> -> ??
;; (define (lob-fn lob)
;;   (cond
;;    [(empty? lob) ...]
;;    [else (... 
;;            (block-fn (first lob))
;;            (lob-fn (rest lob)))]))
;;============================================================================;;
;;                              INTERFACES                                    ;;
;;============================================================================;;

;; Block Interface
;; -----------------

;; Block implements the Block<%> interface

(define Block<%>
  (interface(SWidget<%>) ;; this means: include all the methods in
    ;; SWidget<%>. 
    
    ;; -> ListOfBlock<%>
    ;; RETURNS: the teammates of this block
    get-team
    
    ;; Block<%> -> Void
    ;; EFFECT: adds the given block to this block's team
    add-teammate
    
    ;; Block<%> -> Void
    ;; EFFECT: adds the given block to the block list
    add-block-to-block-list
    
    ;; -> Integer
    ;; RETURNS: the x coordinate of this block
    block-x
    
    ;; -> Integer
    ;; RETURNS: the y coordinate of this block
    block-y

    ;; -> Boolean
    ;; RETURNS: the selected? value of the block
    is-selected

    ;; -> Void
    ;; EFFECT: updates the list of subscribers of the current block
    ;; as and when the current block intersects with any other block in the
    ;; blockfactory while being dragged
    set-subscribers

    ;; -> Void
    ;; EFFECT: updates the moved field of the block to false
    set-not-moved

    ;; Int Int -> Void
    ;; EFFECT: moves the teammates of this block along with the leader block
    move-teammates

    ;; PosInt PosInt -> Void
    ;; EFFECT: for each of the subscribers of the current block, check if the
    ;; leader block intersects with it , if so connect it to the leader block
    publish

    ;; Int Int Block<%> -> Void
    ;; EFFECT: checks if the leader block intersects with any of the blocks in
    ;; the list of subscribers. If it intersects, add the block to its team and
    ;; also add it to the team of already existing teammates.
    leader-moved

    ;; ListOfBlock<%> -> Void
    ;; EFFECT: updates the team field of the current block
    set-teammates

    ;; Int Int Block<%> -> Void
    ;; EFFECT: updates the x and y fields of the current block and marks the
    ;; moved field to true
    move-by-leader
    ))


;; BlockFactory Interface
;; --------------------------

;; BlockFactory implements the BlockFactory<%> interface

(define BlockFactory<%>
  (interface (SWidget<%>) ;; this means: include all the methods in
    ;; SWidget<%>.

    ;; -> Void
    ;; EFFECT: adds the new block created at the default position in the canvas
    ;; to the stateful-widgets of the world as well as to the list of the blocks
    ;; maintained in the blockfactory
    add-block-to-list-and-world-defpos

    ;; Block<%> -> Void
    ;; EFFECT: adds the current block to the list of blocks
    add-block-to-blocks-list

    ;; -> Void
    ;; EFFECT: adds the current block to the list of stateful widgets of the
    ;; world as well as to the list of the blocks in the blockfactory
    add-block-to-list-and-world

    ;; -> LisOfBlock<%>
    ;; RETURNS:the list of the blocks in the BlockFactory<%>
    get-blocks

    ;; -> Int
    ;; RETURNS:the x coordinate of the button down event
    for-test:get-button-down-x
    ))
