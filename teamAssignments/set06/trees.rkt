;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (trees.rkt).

;; GOALS:
;; Design and implement a system for a graphical interface for trees.
;; The canvas starts empty.
;; Nodes of the tree are rendered as green outline circles of a fixed radius.
;; There should be a straight blue line from the center of a node
;; to the center of each of its sons.
;; Implementing Mouse Event with button-down, drag, button-up.
;; Implementing Key Event with "t", "n", "d", "l".

;; start with (run any-value)
;; examples:
;; (run 0)
;; (run 1)

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(check-location "06" "trees.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS:                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mouse pointer
(define INITIAL-MOUSE-POINTER-X 0)
(define INITIAL-MOUSE-POINTER-Y 0)

; dimensions of the canvas
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

;; dimensions of the node
(define NODE-RADIUS 10)

; images of the node
(define NODE-IMAGE (circle NODE-RADIUS "outline" "green"))
(define SELECTED-NODE-IMAGE (circle NODE-RADIUS "solid" "green"))

; color of the lines
(define LINE-COLOR "blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF CONSTANTS                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct world (nodes))

;; Interpretation:
;; A World is a
;; (make-world ListOfNodes)
;; where
;; nodes is a ListOfNodes which are the root nodes of the canvas

;; Template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-nodes w)))

;; A Tree is considered as a Node

(define-struct node (x-pos y-pos mx my selected? sons))

;; Interpretation:
;; A Node is a
;; (make-node Integer Integer Integer Integer Boolean ListOfNodes)
;; where
;; x-pos is the x coordinate of the center of the node
;; y-pos is the y coordiante of the center of the node
;; mx is the position vector between the mouse x coordinate and
;; the x coordinate of the center of the node 
;; my is the position vector between the mouse y coordinate and
;; the y coordinate of the center of the node
;; selected? returns true iff the node is selected by a mouse
;; sons is a list of nodes which represents the sons of the node.

;; Template:
;; node-fn : Node -> ??
;; (define (node-fn n)
;;   (... (node-x-pos n)
;;        (node-y-pos n)
;;        (node-mx n)
;;        (node-my n)
;;        (node-selected? n)
;;        (lon-fn (node-sons n))))

;; A ListOfTree is considered as a ListOfNodes

;; ListOfNodes
;; A ListOfNodes (LON) is either
;; -- empty
;; -- (cons Node LON)

;; Template:
;; lon-fn : LON -> ??
;; (define (lon-fn lon)
;;   (cond
;;     [(empty? lon) ...]
;;     [else (...
;;             (node-fn (first lon))
;;             (lon-fn (rest lon)))]))

;; KeyEvent is defined in the 2htdp/universe module. A KeyEvent
;; represents key board events. Every KeyEvent is a string, but
;; not all strings are key events. The predicate for comparing
;; two KeyEvent is key=? .

;; MouseEvent is defined in the 2htdp/universe module. A MouseEvent
;; represents mouse events. Every MouseEvent is a string, but
;; not all strings are mouse events. The predicate for comparing
;; two MouseEvent is mouse=? .

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF DATA DEFINITIONS                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES FOR TEST                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; example KeyEvents for testing
(define new-root-kev "t")
(define new-son-kev "n")
(define left-delete-kev "l")
(define delete-node-kev "d")

;; example MouseEvents for testing:
(define button-down-event "button-down")
(define drag-event "drag")
(define button-up-event "button-up")
(define other-event "enter")

;; examples of worlds 
(define initial-world-state
  (make-world empty))

(define world-with-one-root
  (make-world (list
               (make-node HALF-CANVAS-WIDTH
                          NODE-RADIUS
                          INITIAL-MOUSE-POINTER-X
                          INITIAL-MOUSE-POINTER-Y
                          false
                          empty))))

(define world-with-left-sons
  (make-world (list
               (make-node 450 240 52 44 true
                          (list (make-node 56 348 0 0 false empty)
                                (make-node 30 40 0 0 false empty)))
               (make-node 450 240 52 44 false
                          (list (make-node 60 55 0 0 false empty)
                                (make-node 67 58 0 0 false empty))))))

(define world-with-deleted-left-sons
  (make-world (list (make-node 450 240 52 44 true empty)
                    (make-node 450 240 52 44 false empty))))

(define world-with-deleted-selected-node
  (make-world (list
               (make-node 450 240 52 44 false
                          (list (make-node 60 55 0 0 false empty)
                                (make-node 67 58 0 0 false empty))))))

;; examples of nodes
(define node-with-left-sons
  (make-node 450 240 52 44 true
             (list (make-node 56 348 0 0 false empty)
                   (make-node 30 40 0 0 false empty))))

(define node-with-deleted-left-sons
  (make-node 450 240 52 44 true empty))

(define node-with-selected-sons
  (make-node 450 240 52 44 false
             (list (make-node 56 348 0 0 true empty)
                   (make-node 30 40 0 0 false empty))))

(define node-with-deleted-sons
  (make-node 450 240 52 44 false
             (list (make-node 30 40 0 0 false empty))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial-world                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value (ignored)
;; RETURNS: the initial world with EMPTY-CANVAS having no nodes.
;; EXAMPLES: (initial-world 1) -> initial-world-state
;; STRATEGY: combine simpler functions
(define (initial-world value)
  (make-world empty))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (initial-world 1)
   initial-world-state
   "The initial world state should be returned having
    empty canvas and no nodes")
  (check-equal?
   (initial-world "anb")
   initial-world-state
   "The initial world state should be returned having
    empty canvas and no nodes"))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : Any -> World
;; GIVEN: any value 
;; EFFECT: runs a copy of an initial world.
;; RETURNS: the final state of the world. The given value is ignored.
;; STRATEGY: combine simpler functions
(define (run value)
  (big-bang (initial-world value)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-key-event                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helper functions for key event :

;; is-new-root-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents
;;           a t KeyEvent
;; STRATEGY: combine simpler functions
(define (is-new-root-key-event? ke)
  (key=? ke new-root-kev))

;; is-left-canvas-delete-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents
;;           a l key event 
;; STRATEGY: combine simpler functions
(define (is-left-canvas-nodes-delete-key-event? ke)
  (key=? ke left-delete-kev))

;; is-delete-node-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents
;;           a d key event 
;; STRATEGY: combine simpler functions
(define (is-delete-node-key-event? ke)
  (key=? ke delete-node-kev))

;; is-new-son-key-event? : KeyEvent -> Boolean
;; GIVEN : a KeyEvent
;; RETURNS : true iff the KeyEvent represents
;;           a n key event 
;; STRATEGY: combine simpler functions
(define (is-new-son-key-event? ke)
  (key=? ke new-son-kev))

;; make-node-with-modified-sons : Node Function -> Node
;; GIVEN: a node and a function
;; RETURNS: the given node after implementing
;;          the given function on its sons.
;; EXAMPLES: (make-node-with-modified-sons n delete-nodes)
;;           -> node-with-deleted-sons
;; STRATEGY: Use template for Node on node
(define (make-node-with-modified-sons n fn)
  (make-node (node-x-pos n)
             (node-y-pos n)
             (node-mx n)
             (node-my n)
             (node-selected? n)
             fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a World and a key event
;; RETURNS: the state of the world as it should be
;;          following the given key event
;; EXAMPLES:
;; (world-after-key-event w "t") -> creates a new root node in the center of
;;                                  the top of the canvas.
;; (world-after-key-event w "l") -> deletes every node whose center is in the
;;                                  left half of the canvas.(If a node is
;;                                  deleted, all of its children are also
;;                                  deleted, as with "d")
;; (world-after-key-event w "n") -> to add a new son to selected nodes
;; (world-after-key-event w "d") -> deletes the selected node and its whole
;;                                  subtree.
;; (world-after-key-event w "x") -> -- ignore all others
;; STRATEGY: Divided into cases on KeyEvent kev

(define (world-after-key-event w kev)
  (cond 
    [(is-new-root-key-event? kev)
     (world-with-new-root w)]
    [(is-left-canvas-nodes-delete-key-event? kev)
     (world-with-left-canvas-nodes-deleted w)]
    [(is-delete-node-key-event? kev)
     (world-with-deleted-nodes w)]
    [(is-new-son-key-event? kev)
     (world-with-new-sons w)]
    [else w]))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (world-after-key-event initial-world-state new-root-kev)
   world-with-one-root
   "The given world should have a new root in the center
    of the top of the canvas")
  (check-equal?
   (world-after-key-event world-with-roots left-delete-kev)
   initial
   "The given world should be empty canvas having no nodes")
  (check-equal?
   (world-after-key-event world-with-left-sons delete-node-kev)
   world-with-deleted-selected-node
   "The given world should not have given selected node
    and its sub-tree")
  (check-equal?
   (world-after-key-event initial-world-state "x")
   initial-world-state
   "The given world should be returned unchanged"))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-root-kev "t"                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-new-root : World -> World
;; GIVEN: a world
;; RETURNS: a new world with a new root added in the
;;          center of the top of the canvas which
;;          initially has no sons.
;; EXAMPLES : (world-with-new-root initial-world-state)
;;            -> world-with-one-root
;; STRATEGY: Use Template for World on w
(define (world-with-new-root w)
  (make-world
   (cons (make-node HALF-CANVAS-WIDTH
                    NODE-RADIUS
                    INITIAL-MOUSE-POINTER-X
                    INITIAL-MOUSE-POINTER-Y
                    false
                    empty)
         (world-nodes w))))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (world-with-new-root initial-world-state)
   world-with-one-root
   "The initial empty world should now have one root")
  (check-equal?
   (world-after-key-event initial-world-state new-root-kev)
   world-with-one-root))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; left-delete-kev "l"                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-left-canvas-nodes-deleted : World -> World
;; GIVEN: a world
;; RETURNS: a world like the given one, that now does 
;;          not have every node whose center was in 
;;          the left half of the canvas. The sons of 
;;          the root node having its center in the 
;;          left half of the canvas also gets deleted.
;; EXAMPLES: (world-with-left-canvas-nodes-deleted world-with-roots) -> initial
;; STRATEGY: Use template for World on w
(define (world-with-left-canvas-nodes-deleted w)
  (make-world (nodes-with-left-canvas-deleted (world-nodes w))))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (world-with-left-canvas-nodes-deleted world-with-roots)
   initial
   "The world does not have nodes whose center was in the
    left half of th canvas")
  (check-equal?
   (world-with-left-canvas-nodes-deleted world-with-roots-and-sons)
   initial
   "The world does not have nodes whose center was in the
    left half of th canvas")
  (check-equal?
   (world-with-left-canvas-nodes-deleted world-with-left-sons)
   world-with-deleted-left-sons
   "The world does not have nodes whose center was in the
    left half of th canvas")
  (check-equal?
   (world-with-left-canvas-nodes-deleted
    (make-world
     (list
      (make-node 450 240 52 44 true empty))))
   (make-world (list (make-node 450 240 52 44 true empty)))
   "The world does not have nodes whose center was in the
    left half of th canvas"))
;===============================================================================

;; nodes-with-left-canvas-deleted : ListOfNodes -> ListOfNodes
;; GIVEN: a list of nodes
;; RETURNS: a list of nodes which does not contain any node
;;          having its center in the left half of the canvas
;;          and its sons.
;; EXAMPLES: (nodes-with-left-canvas-deleted world-with-roots-and-sons-nodes)
;;           -> initial
;; STRATEGY: Use HOF filter on lon followed by HOF map
(define (nodes-with-left-canvas-deleted lon)
  (map
   delete-sub-nodes
   (filter
    ;; Node -> Boolean
    ;; GIVEN: a node
    ;; RETURNS: true iff the given node does not have its
    ;;          center in the left half of the canvas
    ;; STRATEGY: combine simpler functions
    (lambda (node) (not (is-node-at-the-left-of-canvas node)))
    lon)))

;; is-node-at-the-left-of-canvas : Node -> Boolean
;; GIVEN: a node
;; RETURNS: true iff the center of the given node
;;          is in the left half of the canvas
;; EXAMPLES: (is-node-at-the-left-of-canvas
;;                        (make-node 450 240 52 44 true empty))->false
;; STRATEGY: Use template for Node on node
(define (is-node-at-the-left-of-canvas node)
  (< (node-x-pos node) HALF-CANVAS-WIDTH))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (is-node-at-the-left-of-canvas (make-node 450 240 52 44 true empty))
   false
   "Return false because the node is not at the left of the canvas"))
;===============================================================================

;; delete-sub-nodes : Node -> Node
;; GIVEN: the state of a node
;; RETURNS: the state of the given node after
;;          deleting its sons(if any) which are having
;;          their center in the left half of the canvas
;; EXAMPLES: (delete-sub-nodes node-with-left-sons)
;;           -> node-with-deleted-left-sons
;; STRATEGY: call a more generic function
(define (delete-sub-nodes node)
  (make-node-with-modified-sons node
                                (nodes-with-left-canvas-deleted
                                 (node-sons node))))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (delete-sub-nodes node-with-left-sons)
   node-with-deleted-left-sons
   "The node should not have its sons which had 
    their center on the left half of the canvas"))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-node-kev "d"                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-deleted-nodes : World -> World
;; GIVEN: a world
;; RETURNS: a world like the given one, after 
;;          deleting the selected node on 
;;          keyevent "d" and its sons, if any.
;; EXAMPLES: (world-with-deleted-nodes world-with-left-sons)
;;           -> world-with-deleted-selected-node
;; STRATEGY: Use template for World on w
(define (world-with-deleted-nodes w)
  (make-world (delete-nodes (world-nodes w))))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (world-with-deleted-nodes world-with-left-sons)
   world-with-deleted-selected-node
   "The world should not have the selected node
    and its sons,if any"))
;===============================================================================

;; delete-nodes : ListOfNodes -> ListOfNodes
;; GIVEN: a list of nodes
;; RETURNS: the given list of nodes without
;;          the selected node and its sons, 
;;          if any.
;; EXAMPLES: (delete-nodes roots-and-sons-nodes) -> deleted-sons
;; STRATEGY: Use HOF filter on lon followed by HOF map on lon
(define (delete-nodes lon)
  (map delete-sons
       (filter
        ;; Node -> Boolean
        ;; GIVEN: a node
        ;; RETURNS: true iff the given node is not selected
        ;; STRATEGY: combine simpler functions
        (lambda (node) (not (node-selected? node)))
        lon)))

;===============================================================================
;; EXAMPLES FOR TEST:
(define roots-and-sons-nodes
  (list (make-node 50 40 52 44 false
                   (list (make-node 56 48 0 0 true empty)
                         (make-node 30 40 0 0 false empty)))))
(define deleted-sons
  (list (make-node 50 40 52 44 false
                   (list (make-node 30 40 0 0 false empty)))))
;; TEST:
(begin-for-test
  (check-equal?
   (delete-nodes roots-and-sons-nodes)
   deleted-sons
   "The selected nodes should be deleted"))
;===============================================================================

;; delete-sons : Node -> Node
;; GIVEN: a node
;; RETURNS: the given node after deleting
;;          the one of its selected sons 
;; EXAMPLES: (delete-sons node-with-selected-sons)
;;           -> node-with-deleted-sons
;; STRATEGY: call a more generic function
(define (delete-sons n)
  (make-node-with-modified-sons n (delete-nodes (node-sons n))))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (delete-sons node-with-selected-sons)
   node-with-deleted-sons
   "The node should not have the selected son"))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-son-kev "n"                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-new-sons : World -> World
;; GIVEN: a world state
;; RETURNS: the world like the given one, with 
;;          new sons added to the selected node.
;; EXAMPLES:
;; (world-with-new-sons world-with-selected-node) ->
;;                                          world-with-new-sons-to-selected-node
;; STRATEGY: Use Template for World on w
(define (world-with-new-sons w)
  (make-world
   (nodes-with-new-sons (world-nodes w))))
;===============================================================================
;; EXAMPLES:
(define world-with-selected-node
  (make-world (list (make-node 100 100 0 0 false
                               (list (make-node 200 200 0 0 true empty))))))

(define world-with-new-sons-to-selected-node
  (make-world
   (list
    (make-node 100 100 0 0 false
               (list
                (make-node 200 200 0 0 true
                           (list
                            (make-node 200 230 0 30 false empty))))))))

;; TESTS:
(begin-for-test
  
  (check-equal?
   (world-after-key-event world-with-selected-node "n")
   (world-with-new-sons world-with-selected-node))
  
  (check-equal?
   (world-with-new-sons
    (make-world (list (make-node 100 100 0 0 false
                                 (list (make-node 200 200 0 0 true empty))))))
   (make-world
    (nodes-with-new-sons (list
                          (make-node 100 100 0 0 false
                                     (list
                                      (make-node 200 200 0 0 true empty)))))))
  (check-equal?
   (world-after-key-event
    (make-world
     (list(make-node 100 100 0 0 true
                     (list
                      (make-node 200 200 100 100 false empty)))))
    "n")
   (world-with-new-sons
    (make-world (list(make-node 100 100 0 0 true
                                (list
                                 (make-node 200 200 100 100 false empty)))))))
  (check-equal?
   (son-x-pos
    (make-node 100 100 0 0 true
               (list (make-node 200 200 0 0 false empty))))
   (+ 200 (* 3 NODE-RADIUS))))
;===============================================================================

;; nodes-with-new-sons : ListOfNodes -> ListOfNodes
;; GIVEN: a list of root nodes
;; RETURNS: a list of root nodes with new sons
;;          added to the selected node
;; EXAMPLES: (nodes-with-new-sons
;;            (list (make-node 100 100 0 0 true
;;                   (list (make-node 200 200 0 0 false empty)))))
;;           ->(list (make-node 100 100 0 0 true
;;              (list (make-node 230 130 130 30 false empty)
;;                    (make-node 200 200 0 0 false empty))))
;; STRATEGY: Use HOF map on lon
(define (nodes-with-new-sons lon)
  (map
   (lambda (n) (node-with-new-sons n))
   lon))

;; node-with-new-sons : Node -> Node
;; GIVEN: the state of a node
;; RETURNS: the state of the node after adding a 
;;          new son iff the given node is selected.
;; EXAMPLES:
;; (node-with-new-sons
;;  (make-node 100 100 0 0 true
;;            (list (make-node 200 200 0 0 false empty))))
;; -> (make-node 100 100 0 0 true
;;           (list (make-node 230 130 130 30 false empty)
;;                 (make-node 200 200 0 0 false empty)))        
;; STRATEGY: Use template for Node on n                  
(define (node-with-new-sons n)
  (if (node-selected? n)
      (add-new-son (node-with-added-son n))
      (node-with-added-son n)))

;; node-with-added-son : Node -> Node
;; GIVEN: the state of a node
;; RETURNS: the state of the node after adding a new
;;          son to its sons iff any of the sons is
;;          selected. If the node doesn't have any
;;          sons, return the node unchanged.
;; EXAMPLES:
;(node-with-added-son
;    (make-node 100 100 0 0 false
;                    (list (make-node 200 200 0 0 true empty))))
;->  (make-node 100 100 0 0 false
;                (list (make-node 200 200 0 0 true
;                            (list (make-node 200 230 0 30 false empty)))))
;; STRATEGY: call a more generic function
(define (node-with-added-son n)
  (make-node-with-modified-sons n (nodes-with-new-sons (node-sons n))))

;; add-new-son : Node -> Node
;; GIVEN: a node
;; RETURNS: the given node after adding a new son
;; EXAMPLES:
;(add-new-son
;    (make-node 100 100 0 0 true
;                    (list (make-node 200 200 0 0 false empty))))
;->  (make-node 100 100 0 0 true
;                 (list (make-node 230 130 130 30 false empty)
;                       (make-node 200 200 0 0 false empty)))
;; STRATEGY: Use template for Node on n
(define (add-new-son n)
  (if (empty? (node-sons n))
      (add-son n (node-x-pos n))      
      (add-son n (son-x-pos n))))

;; son-x-pos : Node -> Integer
;; GIVEN: a node
;; RETURNS: the x-coordinate of a new son
;; EXAMPLES:
;(son-x-pos
;    (make-node 100 100 0 0 true
;                    (list (make-node 200 200 0 0 false empty))))
;->  230)
;; STRATEGY: Use template for Node on n
(define (son-x-pos n)
  (+ (max-x-pos (node-sons n))
     (* 3 NODE-RADIUS)))

;; max-x-pos : ListOfNodes -> Integer
;; GIVEN: a list of nodes                                                     
;; RETURNS: the x-coordinate of the rightmost son of a node
;; WHERE:   retrun 0, if the node doesn't have any son 
;; EXAMPLE:
;; (max-x-pos (list (make-node 56 348 0 0 false empty)
;;                           (make-node 30 40 0 0 false empty)))
;;                                                      -> 56
;; STRATEGY: Use HOF foldr on lon
(define (max-x-pos lon)
  (foldr
   ;; Node Integer -> Integer
   ;; RETURNS: the max x-coordinate of the list of nodes
   (lambda (n max-x-pos) (max (node-x-pos n) max-x-pos))
   0
   lon))

;===============================================================================
;; TEST for max-x-pos :
(begin-for-test
  (check-equal?
   (max-x-pos (list (make-node 56 348 0 0 false empty)
                    (make-node 30 40 0 0 false empty)))
   56)
  (check-equal?
   (son-mx 100 200 0)
   100)
  (check-equal?
   (son-my 100 200 0)
   100))
;===============================================================================

;; Helper Functions:
;; son-mx : Integer Integer Integer -> Integer                            
;; GIVEN: the node's x-coordinate, the son's x-coordinate,
;;        and the position vector between the mouse's 
;;        x-coordinate and the node's x-coordinate
;; RETURNS: the position vector between the mouse's 
;;          x-coordinate and the node's x-coordinate
;; EXAMPLES: (son-mx 100 200 0) -> 100
;; STRATEGY: combine simpler functions
(define (son-mx node-x son-x node-mx)                             
  (+ node-mx (- son-x node-x)))

;; son-my : Integer Integer Integer -> Integer                               
;; GIVEN: the node's y-coordinate, the son's y-coordinate,
;;        and the position vector between the mouse's y-coordinate
;;        and the node's y-coordinate
;; RETURNS: the position vector between the mouse's y-coordinate
;;          and the node's y-coordinate
;; EXAMPLES:(son-my 100 200 0) -> 100
;; STRATEGY: combine simpler functions
(define (son-my node-y son-y-pos node-my)
  (+ node-my (- son-y-pos node-y)))

;; add-son : Node Integer -> Node
;; GIVEN: a node and the x-coordinate of the center of its
;;        new son
;; RETURNS: a node with a new son added according to the given x-coordinate
;; EXAMPLES:
;(add-son (make-node 100 100 0 0 true empty) 100)
;-> (make-node 100 100 0 0 true
;              (list (new-son (make-node 100 100 0 0 true empty) 100)))
;; STRATEGY: Use Template for Node on n
(define (add-son n son-x-pos)
  (make-node-with-modified-sons n (cons (new-son n son-x-pos) (node-sons n))))

;; new-son : Node Integer -> Node
;; GIVEN: a node and the x-coordinate of the center of its new son
;; RETURNS: a node with a new son added at a distance
;;          (3*radii of the node) away from the rightmost
;;          son and at a distance of (3*radii of the node)
;;          down from the node
;; EXAMPLES:
;(add-son (make-node 100 100 0 0 true empty) 100)
;-> (make-node 100 100 0 0 true
;              (list (new-son (make-node 100 100 0 0 true empty) 100)))
;; STRATEGY: Use Template for Node on n
(define (new-son n son-x-pos)
  (make-node son-x-pos
             (+ (node-y-pos n) (* 3 NODE-RADIUS))
             (son-mx (node-x-pos n)
                     son-x-pos
                     (node-mx n))
             (son-my (node-y-pos n)
                     (+ (node-y-pos n) (* 3 NODE-RADIUS))
                     (node-my n))
             false empty))
;===============================================================================
;; TEST for max-x-pos :
(begin-for-test
  (check-equal?
   (add-son (make-node 100 100 0 0 true empty) 100)
   (make-node 100 100 0 0 true
              (list (new-son (make-node 100 100 0 0 true empty) 100)))))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-to-scene                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; GIVEN: a world w
;; RETURNS: a Scene that portrays the given world state.
;; EXAMPLE: (world-to-scene initial-world-state) -> initial-scene
;; STRATEGY: Use template for World on w
(define (world-to-scene w)
  (place-nodes
   (world-nodes w)
   EMPTY-CANVAS))

;===============================================================================
;; EXAMPLES FOR TESTING
(define initial
  (make-world empty))

(define world-with-roots
  (make-world
   (list (make-node 50 40 0 0 false empty)
         (make-node 30 40 0 0 false empty))))

(define world-with-roots-nodes
  (list (make-node 50 40 0 0 false empty)
        (make-node 30 40 0 0 false empty)))

(define world-with-roots-and-sons
  (make-world (list
               (make-node 50 40 52 44 true
                          (list
                           (make-node 56 48 0 0 false empty)
                           (make-node 30 40 0 0 false empty)))
               (make-node 55 47 57 49 true
                          (list
                           (make-node 60 55 0 0 false empty)
                           (make-node 67 58 0 0 false empty))))))

(define world-with-roots-and-sons-nodes
  (list (make-node 50 40 52 44 true
                   (list (make-node 56 48 0 0 false empty)
                         (make-node 30 40 0 0 false empty)))
        (make-node 55 47 57 49 true
                   (list (make-node 60 55 0 0 false empty)
                         (make-node 67 58 0 0 false empty)))))

(define single-node
  (make-node 50 40 52 44 true
             (list (make-node 56 48 0 0 false empty)
                   (make-node 30 40 0 0 false empty))))

(define world-with-roots-and-sons-sons
  (make-world (list
               (make-node 50 40 52 44 true
                          (list
                           (make-node 56 48 0 0 false
                                      (list
                                       (make-node 59 50 0 0 false empty)
                                       (make-node 67 52 0 0 false empty)))
                           (make-node 30 40 0 0 false empty)))
               (make-node 55 47 57 49 true
                          (list
                           (make-node 60 55 0 0 false empty)
                           (make-node 67 58 0 0 false empty))))))

(define world-with-roots-and-sons-sons-nodes
  (list
   (make-node 50 40 52 44 true
              (list
               (make-node 56 48 0 0 false
                          (list
                           (make-node 59 50 0 0 false empty)
                           (make-node 67 52 0 0 false empty)))
               (make-node 30 40 0 0 false empty)))
   (make-node 55 47 57 49 true
              (list
               (make-node 60 55 0 0 false empty)
               (make-node 67 58 0 0 false empty)))))
;; TEST:
(begin-for-test
  (check-equal?
   (world-to-scene initial)
   EMPTY-CANVAS)
  (check-equal?
   (world-to-scene world-with-roots)
   (place-nodes world-with-roots-nodes EMPTY-CANVAS))
  (check-equal?
   (world-to-scene world-with-roots-and-sons)
   (place-nodes world-with-roots-and-sons-nodes EMPTY-CANVAS))
  (check-equal?
   (world-to-scene world-with-roots-and-sons-sons)
   (place-nodes world-with-roots-and-sons-sons-nodes EMPTY-CANVAS)))
;===============================================================================

;; place-nodes : ListofNodes Scene -> Scene
;; GIVEN: a list of nodes and a scene
;; RETURNS: a scene like the given one, but with
;;          the given list of nodes painted
;;          on it.
;; EXMAPLES: (place nodes empty EMPTY-CANVAS) -> EMPTY-CANVAS
;; STRATEGY: Use HOF foldr on lor
(define (place-nodes lon canvas)
  (foldr place-node canvas lon))

;; place-node : Node Scene -> Scene
;; GIVEN: the state of a node and a scene
;; RETURNS: a scene like the given one, but with
;;          the given node painted on it.
;; EXMAPLES: (place-node single-node EMPTY-CANVAS) -> canvas-with-single-node
;; STRATEGY: Use template for Node on node
(define (place-node node canvas)
  (place-image (get-node-image node)
               (node-x-pos node)
               (node-y-pos node)
               (place-sons
                (node-sons node)
                (node-x-pos node)
                (node-y-pos node)
                canvas)))

;; place-sons : ListOfNodes Integer Integer Scene -> Scene
;; GIVEN : a list of sons, center coordinates of
;;         root node and a scene
;; RETURNS: a scene like the given one, but
;;          with the given list of sons
;;          painted on it, if any.
;; EXAMPLES:
;; (place-sons lon node-x-pos node-y-pos canvas) -> canvas-with-sons-and-line
;; STRATEGY: Use HOF foldr on lon
(define (place-sons lon root-center-x root-center-y canvas)
  (foldr
   ;; Node Scene -> Scene
   ;; RETURNS: a scene like the given one,
   ;;          but with the given son painted
   ;;          on it and having a line from
   ;;          its center to the center of its
   ;;          root node.
   ;; STRATEGY: call simpler functions
   (lambda (node canvas) (place-son node root-center-x root-center-y canvas))
   canvas
   lon))

;; place-son : Node Integer Integer Scene -> Scene
;; GIVEN : a son node, center coordinates of
;;         root node and a scene
;; RETURNS: a scene like the given one, but
;;          with the given son painted on it
;;          and having a line from its center
;;          to the center of its root node.
;;  EXAMPLES:
;; (place-son son node-x-pos node-y-pos canvas) -> canvas-with-son-and-line
;; STRATEGY: Use template for Node on node
(define (place-son node root-center-x root-center-y canvas)
  (scene+line (place-node node canvas)
              root-center-x
              root-center-y
              (node-x-pos node)
              (node-y-pos node)
              LINE-COLOR))

;; get-node-image : Node  -> Image
;; GIVEN: the state of a node 
;; RETURNS: an image of the node
;; WHERE: image is solid green if node is
;;        selected otherwise it is outline
;; EXAMPLE: (get-node-image selected-node) -> SELECTED-NODE-IMAGE
;; STRATEGY: Use template for Node on node
(define (get-node-image node)
  (if (node-selected? node)
      SELECTED-NODE-IMAGE
      NODE-IMAGE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-mouse-event                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: A World, the x- and y
;;        coordinates of a mouse
;;        event, and the mouse event
;; RETURNS: the world that should
;;          follow the given world
;;          after the given mouse event.
;; EXMAPLES:
;; (world-after-mouse event selected-world x y "button-up") -> unselected-world
;; (world-after-mouse-event
;; (make-world (list (make-node 100 130 0 0 false empty)))
;; 100 140 button-down-event)
;;                   -> (make-world (list (make-node 100 130 0 -10 true empty)))
;; STRATEGY: Use Template for World on w
(define (world-after-mouse-event w mx my mev)
  (make-world
   (nodes-after-mouse-event (world-nodes w) mx my mev)))

;; nodes-after-mouse-event : ListOfNodes Integer Integer MouseEvent
;;                           -> ListOfNodes
;; GIVEN: a list of nodes, the vector mx
;;        and my and the mouse event
;; RETURNS: the state of a list of nodes
;;          after the mouse event
;; STRATEGY: Use HOF map on lon
(define (nodes-after-mouse-event lon mx my mev)
  (map
   ;; Node -> Node
   ;; GIVEN: the state of a node
   ;; RETURNS: the state of a node
   ;;          after the given
   ;;          mouse event.
   ;; STRATEGY: combine simpler functions
   (lambda (n)
     (node-after-mouse-event
      (node-with-sons-after-mev n mx my mev) mx my mev))
   lon))

;; node-with-sons-after-mev : Node Integer Integer MouseEvent -> Node
;; GIVEN : a node, the vector mx my and the
;;         mouse event
;; RETURNS: the node with its sons altered
;;          after the given mouse event
;; STRATEGY: Use Template for Node on n
(define (node-with-sons-after-mev n mx my mev)
  (make-node-with-modified-sons
   n
   (nodes-after-mouse-event (node-sons n) mx my mev)))

;; node-after-mouse-event : Node Int Int MouseEvent -> Node                    
;; GIVEN: A node, the x- and y-coordinates of
;;        a mouse event, and the mouse event
;; RETURNS: the node that should follow the
;;          given node after the given mouse
;;          event
;; STRATEGY: Cases on MouseEvent mev
(define (node-after-mouse-event n mx my mev)
  (cond
    [(mouse=? mev button-down-event) (node-after-button-down n mx my)]
    [(mouse=? mev drag-event) (node-after-drag n mx my)]
    [(mouse=? mev button-up-event) (node-after-button-up n)]
    [else n]))

;===============================================================================
;; TESTS for node-after-button-down :
(begin-for-test
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list (make-node 100 100 0 0 false
                      (list
                       (make-node 100 130 0 0 false
                                  (list
                                   (make-node 100 160 0 0 false empty)))
                       (make-node 130 130 0 0 false empty)))))
    100 (+ 130 NODE-RADIUS) button-down-event)
   (make-world
    (nodes-after-mouse-event
     (list
      (make-node 100 100 0 0 false
                 (list
                  (make-node 100 130 0 0 false
                             (list
                              (make-node 100 160 0 0 false empty)))
                  (make-node 130 130 0 0 false empty))))
     100 (+ 130 NODE-RADIUS) button-down-event))
   "button-down-event : only change mx and my of the node and
    the node's sons")
  
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list
      (make-node 100 100 0 0 false
                 (list
                  (make-node 100 130 0 -10 true
                             (list (make-node 100 160 0 20 false empty)))
                  (make-node 130 130 0 0 false empty)))))
    200 200 drag-event)
   (make-world
    (list (make-node 100 100 0 0 false
                     (list
                      (make-node 200 190 0 -10 true
                                 (list (make-node 200 220 0 20 false empty)))
                      (make-node 130 130 0 0 false empty)))))
   "drag-event")
  
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list (make-node 100 100 0 0 false
                      (list
                       (make-node 200 190 0 -10 true
                                  (list (make-node 200 220 0 20 false empty)))
                       (make-node 130 130 0 0 false empty)))))
    200 200 ;any value
    button-up-event)
   (make-world
    (list
     (make-node 100 100 0 0 false
                (list
                 (make-node 200 190 0 0 false
                            (list (make-node 200 220 0 20 false empty)))
                 (make-node 130 130 0 0 false empty)))))
   "button-up-event : only change the selected node's mx and my to 0")
  
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list
      (make-node 100 100 0 0 false
                 (list
                  (make-node 200 190 0 -10 true
                             (list (make-node 200 220 0 20 false empty)))
                  (make-node 130 130 0 0 false empty)))))
    200 200 ;any value
    other-event)
   (make-world
    (list
     (make-node 100 100 0 0 false
                (list (make-node 200 190 0 -10 true
                                 (list (make-node 200 220 0 20 false empty)))
                      (make-node 130 130 0 0 false empty)))))
   "other-event : do nothing"))
;===============================================================================

;; node-after-button-down : Node NonNegInteger NonNegInteger -> Node
;; GIVEN : a node, the x and y coordinate
;;         of the mouse
;; RETURNS: the state of the given node
;;          following a button down mouse
;;          event at the given location.
;; STRATEGY: Use Template for Node on n
(define (node-after-button-down n x y)
  (if (in-node? n x y)
      (make-node (node-x-pos n)
                 (node-y-pos n)      
                 (- (node-x-pos n) x)
                 (- (node-y-pos n)  y)
                 true
                 (sons-after-button-down (node-sons n) x y))
      n))

;; sons-after-button-down :
;;                        ListOfNodes NonNegInteger NonNegInteger -> ListOfNodes
;; GIVEN : a list of nodes and the x y
;;         coordinate of the mouse
;; RETURNS: a list of nodes with new
;;          position vector mx and my  
;; STRATEGY: Use HOF map on lon
(define (sons-after-button-down lon x y)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: the same node with new
   ;;          position vector mx and my 
   ;; STRATEGY: call simpler function
   (lambda (n) (son-after-button-down n x y))
   lon))

;; son-after-button-down : Node NonNegInteger NonNegInteger -> Node
;; GIVEN : a node and the x y coordinate
;;         of the mouse
;; RETURNS: the given node with new 
;;          position vector mx and my 
;; STRATEGY: Use Template for Node on n
(define (son-after-button-down n x y)
  (make-node (node-x-pos n)
             (node-y-pos n)      
             (- (node-x-pos n) x)
             (- (node-y-pos n)  y)
             (node-selected? n)
             (sons-after-button-down (node-sons n) x y)))

;===============================================================================
;; TESTS for node-after-button-down :
(begin-for-test
  (check-equal?
   (node-after-button-down (make-node 100 100 30 30 false empty) 200 200)
   (make-node 100 100 30 30 false empty)
   "the mouse is not in the node, return n")
  (check-equal?
   (node-after-button-down (make-node 100 100 30 30 false empty) 100 100)
   (make-node 100 100 0 0 true empty)
   "the mouse is in the node")
  (check-equal?
   (node-after-button-down (make-node 100 100 0 0 false empty) 100
                           (+ 100 NODE-RADIUS))
   (make-node 100 100 0 (- 0 NODE-RADIUS) true empty)
   "the mouse is in the node, return node with its mx and my changed")
  (check-equal?
   (node-after-button-down
    (make-node 100 100 0 0 false
               (list (make-node 100 130 0 0 false empty))) 100 100)
   (make-node 100 100 0 0 true
              (list (make-node 100 130 0 30 false empty)))
   "return the root selected not the son")
  (check-equal?
   (node-after-button-down
    (make-node 100 100 0 0 false
               (list (make-node 100 130 0 0 false empty))) 100
                                                           (+ 130 NODE-RADIUS))
   (make-node 100 100 0 0 false
              (list (make-node 100 130 0 0 false empty)))
   "if the mouse is not in the root, then return node unchaged
    even if the mouse is in the son "))
;===============================================================================

;; node-after-drag : Node NonNegInteger NonNegInteger -> Node
;; GIVEN : the state of a node and the x y
;;         coordinate of the mouse
;; RETURNS: the state of the given node following a
;;          drag mouse event at the given location
;; STRATEGY: Use Template for Node on n
(define (node-after-drag n x y)
  (if (node-selected? n)
      (node-with-new-location n x y)  
      n))

;; node-with-new-location : Node NonNegInteger NonNegInteger -> Node
;; GIVEN : the state of a node and the x y
;;         coordinates of the mouse
;; RETURNS: the given node having new center 
;;          coordinates after the drag mouse event
;; STRATEGY: Use Template for Node on n
(define (node-with-new-location n x y)
  (make-node
   (+ x (node-mx n))
   (+ y (node-my n))
   (node-mx n)
   (node-my n)
   (node-selected? n)
   (nodes-after-drag (node-sons n) x y)))

;; nodes-after-drag : ListOfNodes NonNegInteger NonNegInteger -> ListOfNodes
;; GIVEN : a list of nodes and the x y
;;         coordinate of the mouse
;; RETURNS: a list of nodes with new center
;;          coordinates after the drag
;; STRATEGY: Use HOF map on lon
(define (nodes-after-drag lon x y)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node with a new location
   ;; STRATEGY: combine simpler functions
   (lambda (n) (node-with-new-location n x y))
   lon))

;===============================================================================
;; TESTS for node-after-drag :
(begin-for-test
  (check-equal?
   (node-after-drag (make-node 100 100 30 30 false empty) 200 200)
   (make-node 100 100 30 30 false empty)
   "node after drag")
  (check-equal?
   (node-after-drag (make-node 100 100 6 8 true empty) 200 200)
   (make-node 206 208 6 8 true empty)
   "node after drag")
  (check-equal?
   (node-after-drag
    (make-node 100 100 0 0 true
               (list (make-node 100 130 0 30 false empty))) 200 200)
   (make-node 200 200 0 0 true
              (list (make-node 200 230 0 30 false empty)))
   "node after drag")
  (check-equal?
   (node-after-drag
    (make-node 100 100 0 40 false
               (list (make-node 100 130 0 10 true empty))) 200 140)
   (make-node 100 100 0 40 false
              (list (make-node 100 130 0 10 true empty)))
   "when the root node is not selected return nothing changed even
   if the son is selected "))
;===============================================================================

;; node-after-button-up : Node -> Node
;; GIVEN: the state of a node
;; RETURNS: the state of the given node 
;;          following the button up
;;          mouse event
;; EXMAPLE: (node-after-button-up selected-node) -> unselected-node
;;          (node-after-button-up unselected-node) -> unselected-node
;; STRATEGY: Use template for Node on n
(define (node-after-button-up n)
  (if (node-selected? n)
      (make-node (node-x-pos n)
                 (node-y-pos n)
                 INITIAL-MOUSE-POINTER-X
                 INITIAL-MOUSE-POINTER-Y
                 false
                 (node-sons n))
      n))
;===============================================================================
;; TESTS for node-after-button-up :
(begin-for-test
  (check-equal?
   (node-after-button-up (make-node 100 100 30 30 false empty))
   (make-node 100 100 30 30 false empty)
   "The given unselected node remains unselected after button up")
  (check-equal?
   (node-after-button-up (make-node 100 100 30 30 true empty))
   (make-node 100 100 0 0 false empty)
   "The given selected node is now unselected after button up"))
;===============================================================================

;; in-node? : Node NonNegInteger NonNegInteger -> Boolean
;; GIVEN : a node, the x-coordinate and y-coordinate of the mouse
;; RETURNS: true iff the given coordinate is inside the given node.
;; EXAMPLES: (in-node? (make-node 100 100 0 0 false empty) 110 110) -> false
;; STRATEGY: Use template for Node on n
(define (in-node? n x y)
  (<= (+ (sqr (- x (node-x-pos n)))
         (sqr (- y (node-y-pos n))))
      (sqr NODE-RADIUS)))

;===============================================================================
;; TESTS for in-node? :
(begin-for-test
  (check-equal?
   (in-node? (make-node 100 100 0 0 false empty)
             (+ 100 NODE-RADIUS)
             (+ 100 NODE-RADIUS))
   false
   "return true iff the given coordinate is inside the given node")
  (check-equal?
   (in-node? (make-node 100 100 0 0 false empty)
             (+ 100 NODE-RADIUS)
             100)
   true
   "return true iff the given coordinate is inside the given node"))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-to-center                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-to-center : Node -> Posn
;; GIVEN: a node
;; RETURNS: the center of the given node as it
;;          is to be displayed on the scene.
;; EXAMPLES: (node-to-center (make-node 100 100 0 0 true empty))
;;           -> (make-posn 100 100)
;; STRATEGY: Use Template for Node on n
(define (node-to-center n)
  (make-posn (node-x-pos n) (node-y-pos n)))
;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (node-to-center (make-node 100 100 0 0 true empty))
   (make-posn 100 100)
   "node-to-center return a posn of the center of the node")
  (check-equal?
   (node-to-center (make-node 200 200 0 0 false empty))
   (make-posn 200 200)
   "node-to-center return a posn of the center of the node"))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-to-selected?                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-to-selected? : Node -> Boolean
;; GIVEN: a node
;; RETURNS: true iff the given node is selected.
;; EXAMPLES:
;; (node-to-selected? (make-node 100 100 0 0 true empty)) -> true
;; STRATEGY: Use Template for Node on n
(define (node-to-selected? n)
  (node-selected? n))
;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (node-to-selected? (make-node 100 100 0 0 true empty))
   true
   "node-to-selected? return true iff the node is selected")
  (check-equal?
   (node-to-selected? (make-node 100 100 0 0 false empty))
   false
   "node-to-selected? return false iff the node is not selected"))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-to-trees                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-trees : World -> ListOfTree
;; GIVEN: a World
;; RETURNS: a list of all the trees in the given world.
;; EXAMPLE:                                          
;; (world-to-trees (make-world list-of-sons)) -> list-of-sons
;; STRATEGY: Use template for World on w
(define (world-to-trees w)
  (world-nodes w))
;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (world-to-trees (make-world list-of-sons))
   list-of-sons
   "The world-to-trees should return the list of roots"))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-to-root                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-to-root : Tree -> Node
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree
;; EXAMPLE: (tree-to-root tree) -> node
;; STRATEGY: combine simpler functions
(define (tree-to-root node)
  node)
;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (tree-to-root tree)
   tree
   "The root node is returned of the given tree"))
;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-to-sons                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-to-sons : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: the data associated with the 
;;          immediate subtrees of the given tree. 
;; EXAMPLE: (tree-to-sons tree) -> list-of-sons
;; STRATEGY: Use template for Node on node
(define (tree-to-sons node)
  (node-sons node))
;===============================================================================
;; EXAMPLES FOR TESTING
(define tree (make-node 50 40 52 44 true
                        (list
                         (make-node 56 48 0 0 false empty)
                         (make-node 30 40 0 0 false empty))))
(define list-of-sons (list
                      (make-node 56 48 0 0 false empty)
                      (make-node 30 40 0 0 false empty)))
(define tree-without-sons (make-node 40 30 0 0 false empty))
;; TEST:
(begin-for-test
  (check-equal?
   (tree-to-sons tree)
   list-of-sons
   "The list of sons of the given tree should be returned")
  (check-equal?
   (tree-to-sons tree-without-sons)
   '()
   "The list of sons should be empty if the given tree has no subtrees"))
;===============================================================================


