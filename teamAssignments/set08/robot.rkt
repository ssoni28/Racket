;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; robot.rkt

;; Given an infinite chessborad.  The  chessboard extends  infinitely in  all
;; directions.  The   positions   on  the  board  are  pairs   of   integers.
;; On the chessboard, we have  a robot and  some blocks.  The  robot occupies
;; a single square on the chessboard, as does each of  the blocks.  The robot
;; can move any number of squares in any diagonal direction, but it can never
;; move to or through a square occupied by a block. In this way, its behavior
;; is like that of a bishop in chess.

;; The goal is to implement  functions to determine the  possible plan of the
;; the robot in order to reach a  given target position and  to determine the
;; final possible position of the robot on executing a given plan.

(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROVIDE STATEMENTS                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide
 path
 eval-plan)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Position is a (list Integer Integer)
;; where
;; (list x y) represents the position (x,y) on the chessboard.
;; x and y are integers.

;; Template:
;; position-fn : Position -> ??
;; (define (position-fn pos)
;;   (... (first pos))
;;   (... (rest pos)))

;; A MaybePosition is one of
;; -- false
;; -- Position

;; Template
;; maybepos-fn : MaybePos -> ??
;; (define (maybepos-fn mp)
;;   (cond
;;     [(false? mp) ...]
;;     [else (...mp)]))

;; A ListOfPosition (LOP) is either
;; -- Position
;; -- (list Position LOP)

;; Template:
;; lop-fn : ListOfPosition -> ??
;; (define (lop-fn lop)
;;   (cond
;;     [(empty? (rest lop)) ...(first lop)]
;;     [else(...
;;            (position-fn (first los))
;;            (lop-fn (rest lop)))]))

;; A Direction is one of
;; -- "ne"
;; -- "se"
;; -- "sw"
;; -- "nw"
;; Interpretation:
;; "ne" is north-east direction
;; "se" is south-east direction
;; "sw" is south-west direction
;; "nw" is north-west direction

;; Template:
;; direction-fn : Direction -> ??
;; (define (direction-fn dir)
;;   (cond
;;     [(string=? dir "ne") ...]
;;     [(string=? dir "se") ...]
;;     [(string=? dir "sw") ...]
;;     [(string=? dir "nw") ...]))  

;; A Move is a (list Direction PosInt)
;; Interpretation:
;; a move of the specified number of steps in the indicated direction. 

;; Template:
;; move-fn : Move -> ??
;; (define (move-fn move)
;;   (direction-fn (first move))
;;   (position-fn (rest move)))

;; A Plan is a ListOfMove
;; WHERE: the list does not contain two consecutive moves in the same
;; direction.
;; INTERP: the moves are to be executed from the first in the list to
;; the last in the list.

;; A ListOfMove (LOM) is either
;; -- empty
;; -- (list Move LOM)

;; Template:
;; lom-fn : ListOfMove -> ??
;; (define (lom-fn lom)
;;   (cond
;;     [(empty? lom) ...]
;;     [else(...
;;            (move-fn (first lom))
;;            (lom-fn (rest lom)))]))

;; A MaybePlan is one of
;; -- false
;; -- Plan

;; Template
;; maybeplan-fn : MaybePlan -> ??
;; (define (maybeplan-fn mp)
;;   (cond
;;     [(false? mp) ...]
;;     [else (...mp)]))

;; An Symbol is either
;; - or +
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; path : Position Position ListOfPosition -> MaybePlan
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. the target position that robot is supposed to reach
;; 3. A list of the blocks on the board
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; STRATEGY: Divide into cases on targetpos and startpos
(define (path startpos targetpos lop)
  (cond
    [(and (= (first targetpos) (first startpos))
          (= (second targetpos) (second startpos)))
     empty]
    
    [(or
      (and (= (first targetpos) (first startpos))
          (< (second targetpos) (second startpos)))
      (and (< (first targetpos) (first startpos))
          (< (second targetpos) (second startpos))))
     (target-direction "sw" startpos targetpos lop)]
    
    [(or
      (and (= (first targetpos) (first startpos))
          (> (second targetpos) (second startpos)))
      (and (> (first targetpos) (first startpos))
          (> (second targetpos) (second startpos))))
     (target-direction "ne" startpos targetpos lop)]
    
    [(or
      (and (< (first targetpos) (first startpos))
          (= (second targetpos) (second startpos)))
      (and (< (first targetpos) (first startpos))
          (> (second targetpos) (second startpos))))
     (target-direction "nw" startpos targetpos lop)]
    
    [(or
      (and (> (first targetpos) (first startpos))
          (= (second targetpos) (second startpos)))
      (and (> (first targetpos) (first startpos))
          (< (second targetpos) (second startpos))))
     (target-direction "se" startpos targetpos lop)]))

;; TEST:
(define wall1
  (list (list 0 3) (list 2 3) (list 4 3) (list 0 5) (list 4 5) (list 0 7)
        (list 2 7) (list 4 7)))
(define wall2
  (list (list 2 3) (list 4 3) (list 0 5) (list 4 5) (list 0 7)
        (list 2 7) (list 4 7)))

(define two-walls
  (list (list 0 3) (list 4 3) (list 0 5) (list 4 5) (list 0 7) (list 4 7)
        (list 0 9) (list 4 9) (list 0 11) (list 4 11)))
  

(begin-for-test
  (check-equal?
   (path (list -5 -3) (list -8 -6)
         (list (list -7 -3) (list -7 -5) (list -5 -5)))
   (list (list "se" 2) (list "sw" 3) (list "nw" 2))
   "The possible plan")
  (check-equal?
   (path (list 2 5) (list 2 6) empty)
   false
   "no possible plan")
  (check-equal?
   (path (list 2 5) (list 4 9) wall1)
   false
   "no possible plan")
  (check-equal?
   (path (list 2 5) (list 4 9) wall2)
   (list (list "sw" 2) (list "se" 2) (list "ne" 1) (list "se" 1)
         (list "ne" 4) (list "nw" 4))
   "The possible plan")
  (check-equal?
   (path (list -5 -3) (list -5 -4) empty)
   false
   "no possible plan")
  (check-equal?
   (path (list 5 3) (list 4 8) empty)
   (list (list "nw" 3) "ne" 2))
  (check-equal?
   (path (list 2 3) (list 2 3) empty)
   empty)
  (check-equal?
   (path (list 4 4) (list 3 1) empty)
   (list (list "sw" 2) (list "se" 1)))
  (check-equal?
   (path (list 4 3) (list 6 3) empty)
   (list (list "se" 1) (list "ne" 1)))
  (check-equal?
   (path (list 1 4) (list 3 0) empty)
   (list (list "se" 3) (list "sw" 1)))
  (check-equal?
   (path (list 0 1) (list -4 5) (list (list 4 6) (list -4 0)))
   (list (list "nw" 4)))
  )
  
;; target-direction : String Position Position ListOfPosition -> MaybePlan
;; GIVEN: the direction of target from start position,
;;        start position of robot,
;;        target position of robot, and
;;        list of blocks on chessboard.
;; REUTNRS: the possible plan which when followed leads the robot to the given
;; target position, otherwise false if the target position is not rechable.
;; STRATEGY: Use template for Direction on dir
(define (target-direction dir startpos targetpos lop)
  (cond
    [(string=? dir "sw")
     (move-towards-south-west-target dir startpos targetpos lop)]
    [(string=? dir "ne")
     (move-towards-north-east-target dir startpos targetpos lop)]
    [(string=? dir "nw")
     (move-towards-north-west-target dir startpos targetpos lop)]
    [(string=? dir "se")
     (move-towards-south-east-target dir startpos targetpos lop)]))

;; TEST:
(define steps 0)

;; move-towards-south-west-target :
;; String Position Position ListOfPosition -> MAybePlan
;; GIVEN: direction of target position from start position,
;;        start position of robot,
;;        target position of robot, and
;;        list of blocks positions on the chessboard
;; RETUNRS: the possible plan
;; STRATEGY: cases on whether the target is reachable or not
(define (move-towards-south-west-target dir startpos targetpos lop)
  (cond
    [(move-towards-target-possible? startpos targetpos)
     (move-next-in-south-west dir startpos targetpos lop steps)]
    [else false]))

;; move-towards-target-possible? : Position Position -> Boolean
;; GIVEN: start position of robot, and
;;        target position of robot.
;; RETURNS: true iff the target position is in reach of start position
;; according to moving pattern of robot
;; STRATEGY: Use template for Position on startpos and targetpos
(define (move-towards-target-possible? startpos targetpos)
  (= (modulo (-
                 (+ (first startpos) (second startpos))
                 (+ (first targetpos) (second targetpos)))
                2)
        0))

;; next-move-in-south-west : Position -> Position
;; next-move-in-south-east : Position -> Position
;; next-move-in-north-west : Position -> Position
;; next-move-in-north-east : Position -> Position
;; GIVEN: a postion
;; RETURNS: the next position in south-west, south-east,
;;          north-west or north-east direction
;; STRATEGY: Use template for Position on pos
(define (next-move-in-south-west pos)
  (list (- (first pos) 1) (- (second pos) 1)))
(define (next-move-in-south-east pos)
  (list (+ (first pos) 1) (- (second pos) 1)))
(define (next-move-in-north-west pos)
  (list (- (first pos) 1) (+ (second pos) 1)))
(define (next-move-in-north-east pos)
  (list (+ (first pos) 1) (+ (second pos) 1)))

;; move-next-in-south-west :
;; move-next-in-south-east :
;; move-next-in-north-east :
;; move-next-in-north-west : 
;; String Position Position ListOfPosition Integer -> MaybePlan
;; GIVEN:
;; 1. the direction the robot is facing
;; 2. the starting position of the robot,
;; 3. the target position that robot is supposed to reach
;; 4. A list of the blocks on the board
;; 5. The number of steps the robot will move in the given direction
;; RETURNS: 
;; STRATEGY: Divide into cases on startpos and lop
(define (move-next-in-south-west dir startpos targetpos lop steps)
  (if (or (threat? (next-move-in-south-west startpos) lop)
          (deadlock-in-south-west? (next-move-in-south-west startpos) lop))
      (change-direction
       move-next-in-south-east dir "se" startpos targetpos lop steps)
      (move-in-south-west
       dir (next-move-in-south-west startpos) targetpos lop (+ steps 1))))

(define (move-next-in-south-east dir startpos targetpos lop steps)
  (if (or
        (threat? (next-move-in-south-east startpos) lop)
        (deadlock-in-south-east? (next-move-in-south-east startpos) lop))
      (change-direction
       move-next-in-north-east dir "ne" startpos targetpos lop steps)
      (move-in-south-east
       dir (next-move-in-south-east startpos) targetpos lop (+ steps 1))))

(define (move-next-in-north-east dir nextpos targetpos lop steps)
  (if (or
        (threat? (next-move-in-north-east nextpos) lop)
        (deadlock-in-north-east? (next-move-in-north-east nextpos) lop)) ;; chek dir
      (change-direction
       move-next-in-north-west dir "nw" nextpos targetpos lop steps)
      (move-in-north-east
       dir (next-move-in-north-east nextpos) targetpos lop (+ steps 1))))

(define (move-next-in-north-west dir startpos targetpos lop steps)
  (cond
    [(and
      (or
        (threat? (next-move-in-north-west startpos) lop)
        (deadlock-in-north-west? (next-move-in-north-west startpos) lop))
      (or
        (threat? (next-move-in-south-west startpos) lop)
        (deadlock-in-south-west? (next-move-in-south-west startpos) lop)))
     false]
  [else (if (or
        (threat? (next-move-in-north-west startpos) lop)
        (deadlock-in-north-west? (next-move-in-north-west startpos) lop))
      (change-direction move-next-in-south-west dir "sw" startpos targetpos lop steps)
      (move-in-north-west dir (next-move-in-north-west startpos) targetpos lop (+ steps 1)))]))

;; change-direction :
;; (String Position Position ListOfPosition Integer -> MaybePlan)
;; String String Position Position ListOfPosition Integer -> MaybePlan
;; GIVEN:
;; 1. function
;; 2. the direction the robot is facing
;; 3. a new direction
;; 4. the starting position of the robot,
;; 5. the target position that robot is supposed to reach
;; 6. A list of the blocks on the board
;; 7. The number of steps the robot will move in the given direction
;; RETURNS: A MaybePlan
;; STRATEGY: divide into cases on 
(define (change-direction func dir newdir startpos targetpos lop steps)
  (if (= steps 0)
      (func newdir startpos targetpos lop steps)
      (cons (list dir steps) (func newdir startpos targetpos lop 0))))
         
;; change-direction :
;; (String Position Position ListOfPosition Integer -> MaybePlan)
;; String String Position Position ListOfPosition Integer -> MaybePlan
;; GIVEN:
;; 1. function
;; 2. the direction the robot is facing
;; 3. a new direction
;; 4. the starting position of the robot,
;; 5. the target position that robot is supposed to reach
;; 6. A list of the blocks on the board
;; 7. The number of steps the robot will move in the given direction
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; STRATEGY: divide into cases on 
(define (check-next-direction dir nextpos targetpos lop steps)
  (cond
    [(and (< (first targetpos) (first nextpos))
          (> (second targetpos) (second nextpos)))
     (change-direction move-next-in-north-west dir "nw" nextpos targetpos lop steps)]
     [(and (> (first targetpos) (first nextpos))
          (< (second targetpos) (second nextpos)))
      (change-direction move-next-in-south-east dir "se" nextpos targetpos lop steps)]))

;; target-found? : Position Position -> Boolean
;; GIVEN: a start position and a target position of robot
;; RETURNS: true iff start position and target position are equal
;; STRATEGY: Use template for position on nextpos and targetpos
(define (target-found? nextpos targetpos)
  (and
       (= (first nextpos) (first targetpos))
       (= (second nextpos) (second targetpos))))

;; target-link-found? : Position Position -> Boolean
;; GIVEN: a start position and a target position of robot
;; RETURNS: true iff target position is reachable from start position
;; STRATEGY: Use template for position on nextpos and targetpos
(define (target-link-found? nextpos targetpos)
  (=
       (+ (first targetpos) (second targetpos))
       (+ (first nextpos) (second nextpos))))

;; target-link-found-in-different-direction? : Position Position -> Boolean
;; GIVEN: a start position and a target position of robot
;; RETURNS: true iff target position is reachable from start position
;; STRATEGY: Use template for position on nextpos and targetpos
(define (target-link-found-in-different-direction? nextpos targetpos)
  (=
       (- (first targetpos) (second targetpos))
       (- (first nextpos) (second nextpos))))

;; move-in-south-west : String Position Position LisOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-south-west dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found? nextpos targetpos)
     (check-next-direction dir nextpos targetpos lop steps)]
    [else (move-next-in-south-west dir nextpos targetpos lop steps)]))

;; move-in-south-east : String Position Position LisOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-south-east dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found? nextpos targetpos)
     (move-next-in-south-east dir nextpos targetpos lop steps)]
    [(target-link-found-in-different-direction? nextpos targetpos)
     (possible-next-direction dir nextpos targetpos lop steps)]
    [else (if (or (threat? (next-move-in-south-west nextpos) lop)
          (deadlock-in-south-west? (next-move-in-south-west nextpos) lop))
      (move-next-in-south-east dir nextpos targetpos lop steps)
      (change-direction move-next-in-south-west dir "sw" nextpos targetpos lop steps))]))

;; move-in-north-east
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-north-east dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found? nextpos targetpos)
     (check-next-direction dir nextpos targetpos lop steps)]
    [(and
      (> (first targetpos) (first (next-move-in-north-east nextpos)))
      (> (second targetpos) (first (rest (next-move-in-north-east nextpos)))))
     (move-next-in-north-east dir nextpos targetpos lop steps)]
    [else (if (or
               (threat? (next-move-in-south-east nextpos) lop)
               (deadlock-in-south-east? (next-move-in-south-east nextpos) lop))
          (move-next-in-north-east dir nextpos targetpos lop steps)
          (change-direction move-next-in-south-east dir "se" nextpos targetpos lop steps))]))

;; move-in-north-west
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-north-west dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found? nextpos targetpos)
     (move-next-in-north-west dir nextpos targetpos lop steps)]
     [(target-link-found-in-different-direction? nextpos targetpos)
     (possible-next-direction dir nextpos targetpos lop steps)]
    [else (if (or (threat? (next-move-in-south-west nextpos) lop)
          (deadlock-in-south-west? (next-move-in-south-west nextpos) lop))
      (move-next-in-north-west dir nextpos targetpos lop steps)
      (change-direction move-next-in-south-west dir "sw" nextpos targetpos lop steps))]))

;; possible-next-direction
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (possible-next-direction dir nextpos targetpos lop steps)
  (cond
    [(and (> (first targetpos) (first nextpos))
          (> (second targetpos) (second nextpos)))
     (change-direction move-next-in-north-east dir "ne" nextpos targetpos lop steps)]
     [(and (< (first targetpos) (first nextpos))
          (< (second targetpos) (second nextpos)))
      (change-direction move-next-in-south-west dir "sw" nextpos targetpos lop steps)]))

;; threat? : Position ListofPosition -> Boolean
;; GIVEN:
;; RETURNS:
;; STRATEGY: Use HOF ormap on lop
(define (threat? nextpos lop)
  (ormap
   ;; Position -> Boolean
   ;; GIVEN:
   ;; RETURNS:
   (lambda (block) (and
                    (= (first nextpos) (first block))
                    (= (first (rest nextpos)) (first (rest block)))))
   lop))

;; deadlock-in-south-west? : Position ListofPosition -> Boolean
;; deadlock-in-north-east? : Position ListofPosition -> Boolean
;; deadlock-in-north-west? : Position ListofPosition -> Boolean
;; deadlock-in-south-east? : Position ListofPosition -> Boolean
;; GIVEN: the next position 
;; RETURNS:
;; STRATEGY: combine simpler functions
(define (deadlock-in-south-west? nextpos lop)
  (and
   (threat? (next-move-in-north-west nextpos) lop)
   (threat? (next-move-in-south-east nextpos) lop)
   (threat? (next-move-in-south-west nextpos) lop)))

(define (deadlock-in-north-east? nextpos lop)
  (and
   (threat? (next-move-in-north-west nextpos) lop)
   (threat? (next-move-in-south-east nextpos) lop)
   (threat? (next-move-in-north-east nextpos) lop)))

(define (deadlock-in-north-west? nextpos lop)
  (and
   (threat? (next-move-in-north-west nextpos) lop)
   (threat? (next-move-in-south-west nextpos) lop)
   (threat? (next-move-in-north-east nextpos) lop)))

(define (deadlock-in-south-east? nextpos lop)
  (and
   (threat? (next-move-in-south-west nextpos) lop)
   (threat? (next-move-in-south-east nextpos) lop)
   (threat? (next-move-in-north-east nextpos) lop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-towards-north-east-target :
;;                         String Position Position ListOfPosition -> MAybePlan
;; GIVEN: direction of target position from start position,
;;        start position of robot,
;;        target position of robot, and
;;        list of blocks positions on the chessboard
;; RETUNRS: the possible plan
;; STRATEGY: cases on whether the target is reachable or not
(define (move-towards-north-east-target  dir startpos targetpos lop)
  (cond
    [(move-towards-target-possible? startpos targetpos)
     (move-next-in-north-east-ne dir startpos targetpos lop steps)]
    [else false]))

;; move-next-in-north-east-ne : String Position Position ListOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-next-in-north-east-ne dir startpos targetpos lop steps)
  (if (or (threat? (next-move-in-north-east startpos) lop)
          (deadlock-in-north-east? (next-move-in-north-east startpos) lop))
      (change-direction move-next-in-south-east-ne dir "se" startpos targetpos lop steps)
      (move-in-north-east-ne dir (next-move-in-north-east startpos) targetpos lop (+ steps 1))))

;; move-in-north-east-ne : String Position Position LisOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-north-east-ne dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found? nextpos targetpos)
     (check-next-direction-ne dir nextpos targetpos lop steps)]
    [else (move-next-in-north-east-ne dir nextpos targetpos lop steps)]))

;; check-next-direction-ne
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (check-next-direction-ne dir nextpos targetpos lop steps)
  (cond
    [(and (< (first targetpos) (first nextpos))
          (> (second targetpos) (second nextpos)))
     (change-direction move-next-in-north-west-ne dir "nw" nextpos targetpos lop steps)]
     [(and (> (first targetpos) (first nextpos))
          (< (second targetpos) (second nextpos)))
      (change-direction move-next-in-south-east-ne dir "se" nextpos targetpos lop steps)]))


; move-next-in-south-east
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-next-in-south-east-ne dir startpos targetpos lop steps)
  (if (or
        (threat? (next-move-in-south-east startpos) lop)
        (deadlock-in-south-east? (next-move-in-south-east startpos) lop))
      (change-direction move-next-in-south-west-ne dir "sw" startpos targetpos lop steps)
      (move-in-south-east-ne dir (next-move-in-south-east startpos) targetpos lop (+ steps 1))))

;; move-in-south-east-ne : String Position Position LisOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-south-east-ne dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found? nextpos targetpos)
     (move-next-in-south-east-ne dir nextpos targetpos lop steps)]
    [(target-link-found-in-different-direction? nextpos targetpos)
     (possible-next-direction-ne dir nextpos targetpos lop steps)]
    [else (if (or (threat? (next-move-in-north-east nextpos) lop)
          (deadlock-in-north-east? (next-move-in-north-east nextpos) lop))
      (move-next-in-south-east-ne dir nextpos targetpos lop steps)
      (change-direction move-next-in-north-east-ne dir "ne" nextpos targetpos lop steps))]))

;; move-next-in-south-west-ne
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-next-in-south-west-ne dir nextpos targetpos lop steps)
  (if (or
        (threat? (next-move-in-south-west nextpos) lop)
        (deadlock-in-south-west? (next-move-in-south-west nextpos) lop)) ;; chek dir
      (change-direction
       move-next-in-north-west-ne dir "nw" nextpos targetpos lop steps)
      (move-in-south-west-ne
       dir (next-move-in-south-west nextpos) targetpos lop (+ steps 1))))

;; move-in-south-west-ne
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-south-west-ne dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found? nextpos targetpos)
     (check-next-direction-ne dir nextpos targetpos lop steps)]
    [(and
      (< (first targetpos) (first (next-move-in-south-west nextpos)))
      (< (second targetpos) (first (rest (next-move-in-south-west nextpos)))))
     (move-next-in-south-west-ne dir nextpos targetpos lop steps)]
    [else (if (or
               (threat? (next-move-in-south-east nextpos) lop)
               (deadlock-in-south-east? (next-move-in-south-east nextpos) lop))
          (move-next-in-south-west-ne dir nextpos targetpos lop steps)
          (change-direction
           move-next-in-south-east-ne dir "se" nextpos targetpos lop steps))]))

;; move-next-in-north-west-ne
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-next-in-north-west-ne dir startpos targetpos lop steps)
  (cond
    [(and
      (or
        (threat? (next-move-in-north-west startpos) lop)
        (deadlock-in-north-west? (next-move-in-north-west startpos) lop))
      (or
        (threat? (next-move-in-north-east startpos) lop)
        (deadlock-in-north-east? (next-move-in-north-east startpos) lop)))
      false]
    [else
  (if (or
        (threat? (next-move-in-north-west startpos) lop)
        (deadlock-in-north-west? (next-move-in-north-west startpos) lop))
      (change-direction
       move-next-in-north-east-ne dir "ne" startpos targetpos lop steps)
      (move-in-north-west-ne
       dir (next-move-in-north-west startpos) targetpos lop (+ steps 1)))]))

;; move-in-north-west-ne
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-north-west-ne dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found? nextpos targetpos)
     (move-next-in-north-west-ne dir nextpos targetpos lop steps)]
     [(target-link-found-in-different-direction? nextpos targetpos)
     (possible-next-direction-ne dir nextpos targetpos lop steps)]
    [else (if (or (threat? (next-move-in-north-east nextpos) lop)
          (deadlock-in-north-east? (next-move-in-north-east nextpos) lop))
      (move-next-in-north-west-ne dir nextpos targetpos lop steps)
      (change-direction
       move-next-in-north-east-ne dir "ne" nextpos targetpos lop steps))]))

;; possible-next-direction-ne :
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (possible-next-direction-ne dir nextpos targetpos lop steps)
  (cond
    [(and (> (first targetpos) (first nextpos))
          (> (second targetpos) (second nextpos)))
     (change-direction
      move-next-in-north-east-ne dir "ne" nextpos targetpos lop steps)]
     [(and (< (first targetpos) (first nextpos))
          (< (second targetpos) (second nextpos)))
      (change-direction
       move-next-in-south-west-ne dir "sw" nextpos targetpos lop steps)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-towards-north-west-target :
;;                         String Position Position ListOfPosition -> MAybePlan
;; GIVEN: direction of target position from start position,
;;        start position of robot,
;;        target position of robot, and
;;        list of blocks positions on the chessboard
;; RETUNRS: the possible plan
;; STRATEGY: cases on whether the target is reachable or not
(define (move-towards-north-west-target dir startpos targetpos lop)
  (cond
    [(move-towards-target-possible? startpos targetpos)
     (move-next-in-north-west-nw dir startpos targetpos lop steps)]
    [else false]))

;; move-next-in-north-west-nw : String Position Position ListOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-next-in-north-west-nw dir startpos targetpos lop steps)
  (if (or (threat? (next-move-in-north-west startpos) lop)
          (deadlock-in-north-west? (next-move-in-north-west startpos) lop))
      (change-direction move-next-in-south-west-nw dir "sw" startpos targetpos lop steps)
      (move-in-north-west-nw dir (next-move-in-north-west startpos) targetpos lop (+ steps 1))))

;; move-in-north-west-nw : String Position Position LisOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-north-west-nw dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found-in-different-direction? nextpos targetpos)
     (check-next-direction-nw dir nextpos targetpos lop steps)]
    [else (move-next-in-north-west-nw dir nextpos targetpos lop steps)]))

;; check-next-direction-nw
(define (check-next-direction-nw dir nextpos targetpos lop steps)
  (cond
    [(and (> (first targetpos) (first nextpos))
          (> (first (rest targetpos)) (first (rest nextpos))))
     (change-direction move-next-in-north-east-nw dir "ne" nextpos targetpos lop steps)]
     [(and (< (first targetpos) (first nextpos))
          (< (first (rest targetpos)) (first (rest nextpos))))
      (change-direction move-next-in-south-west-nw dir "sw" nextpos targetpos lop steps)]))

;; move-next-in-south-west-nw
(define (move-next-in-south-west-nw dir startpos targetpos lop steps)
  (if (or
        (threat? (next-move-in-south-west startpos) lop)
        (deadlock-in-south-west? (next-move-in-south-west startpos) lop))
      (change-direction move-next-in-south-east-nw dir "se" startpos targetpos lop steps)
      (move-in-south-west-nw dir (next-move-in-south-west startpos) targetpos lop (+ steps 1))))

;; move-in-south-west-nw : String Position Position LisOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-south-west-nw dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found-in-different-direction? nextpos targetpos)
     (move-next-in-south-west-nw dir nextpos targetpos lop steps)]
    [(target-link-found? nextpos targetpos)
     (possible-next-direction-nw dir nextpos targetpos lop steps)]
    [else (if (or (threat? (next-move-in-north-west nextpos) lop)
          (deadlock-in-north-west? (next-move-in-north-west nextpos) lop))
      (move-next-in-south-west-nw dir nextpos targetpos lop steps)
      (change-direction move-next-in-north-west-nw dir "nw" nextpos targetpos lop steps))]))

;; possible-next-direction-nw
(define (possible-next-direction-nw dir nextpos targetpos lop steps)
  (cond
    [(and (< (first targetpos) (first nextpos))
          (> (first (rest targetpos)) (first (rest nextpos))))
     (change-direction move-next-in-north-west-nw dir "nw" nextpos targetpos lop steps)]
     [(and (> (first targetpos) (first nextpos))
          (< (first (rest targetpos)) (first (rest nextpos))))
      (change-direction move-next-in-south-east-nw dir "se" nextpos targetpos lop steps)]))

;; move-next-in-south-east-nw
(define (move-next-in-south-east-nw dir nextpos targetpos lop steps)
  (if (or
        (threat? (next-move-in-south-east nextpos) lop)
        (deadlock-in-south-east? (next-move-in-south-east nextpos) lop)) ;; chek dir
      (change-direction move-next-in-north-east-nw dir "ne" nextpos targetpos lop steps)
      (move-in-south-east-nw dir (next-move-in-north-east nextpos) targetpos lop (+ steps 1))))

;; move-in-south-east-nw
(define (move-in-south-east-nw dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found-in-different-direction? nextpos targetpos)
     (check-next-direction-nw dir nextpos targetpos lop steps)]
    [(and
      (> (first targetpos) (first (next-move-in-south-east nextpos)))
      (< (first (rest targetpos)) (first (rest (next-move-in-south-east nextpos)))))
     (move-next-in-south-east-nw dir nextpos targetpos lop steps)]
    [else (if (or
               (threat? (next-move-in-south-west nextpos) lop)
               (deadlock-in-south-west? (next-move-in-south-west nextpos) lop))
          (move-next-in-south-east-nw dir nextpos targetpos lop steps)
          (change-direction move-next-in-south-west-nw dir "sw" nextpos targetpos lop steps))]))

;; move-next-in-north-east-nw
(define (move-next-in-north-east-nw dir startpos targetpos lop steps)
  (cond
    [(and
      (or
        (threat? (next-move-in-north-east startpos) lop)
        (deadlock-in-north-east? (next-move-in-north-east startpos) lop))
      (or
        (threat? (next-move-in-north-west startpos) lop)
        (deadlock-in-north-west? (next-move-in-north-west startpos) lop)))
     false]
  [else (if (or
        (threat? (next-move-in-north-east startpos) lop)
        (deadlock-in-north-east? (next-move-in-north-east startpos) lop))
      (change-direction move-next-in-north-west dir "nw" startpos targetpos lop steps)
      (move-in-north-east-nw dir (next-move-in-north-east startpos) targetpos lop (+ steps 1)))]))

;; move-in-north-east-nw
(define (move-in-north-east-nw dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list dir steps)]
    [(target-link-found-in-different-direction? nextpos targetpos)
     (move-next-in-north-east-nw dir nextpos targetpos lop steps)]
     [(target-link-found? nextpos targetpos)
     (possible-next-direction-nw dir nextpos targetpos lop steps)]
    [else (if (or (threat? (next-move-in-north-west nextpos) lop)
          (deadlock-in-north-west? (next-move-in-north-west nextpos) lop))
      (move-next-in-north-east-nw dir nextpos targetpos lop steps)
      (change-direction move-next-in-north-west-nw dir "nw" nextpos targetpos lop steps))]))

;; TEST:
(begin-for-test
  (check-equal?
   (move-towards-north-west-target "nw" (list 1 1) (list 1 2) empty)
   false)
 (check-equal?
   (move-towards-north-west-target "nw" (list 1 1) (list -1 3) (list (list 0 2)))
   (list (list "sw" 1) (list "nw" 2) "ne" 1))
 (check-equal?
   (move-towards-north-west-target "nw" (list 1 1) (list -3 1) (list (list 0 2)))
   (list (list "sw" 1) (list "nw" 2) (list "sw" 1)))
 (check-equal?
   (move-towards-north-west-target "nw" (list 1 1) (list -3 1) (list (list 0 2) (list -2 2)))
   (list (list "sw" 1) (list "nw" 1) (list "sw" 1) (list "nw" 1)))
   )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-towards-south-east-target :
;;                         String Position Position ListOfPosition -> MAybePlan
;; GIVEN: direction of target position from start position,
;;        start position of robot,
;;        target position of robot, and
;;        list of blocks positions on the chessboard
;; RETUNRS: the possible plan
;; STRATEGY: cases on whether the target is reachable or not
(define (move-towards-south-east-target dir startpos targetpos lop)
  (cond
    [(move-towards-target-possible? startpos targetpos)
     (move-next-in-south-east-se dir startpos targetpos lop steps)]
    [else false]))

;; move-next-in-south-east-se : String Position Position ListOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-next-in-south-east-se dir startpos targetpos lop steps)
  (if (or (threat? (next-move-in-south-east startpos) lop)
          (deadlock-in-south-east? (next-move-in-south-east startpos) lop))
      (change-direction move-next-in-south-west-se dir "sw" startpos targetpos lop steps)
      (move-in-south-east-se dir (next-move-in-south-east startpos) targetpos lop (+ steps 1))))

;; move-in-south-east-se : String Position Position LisOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-south-east-se dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found-in-different-direction? nextpos targetpos)
     (check-next-direction-se dir nextpos targetpos lop steps)]
    [else (move-next-in-south-east-se dir nextpos targetpos lop steps)]))

;; check-next-direction-se
(define (check-next-direction-se dir nextpos targetpos lop steps)
  (cond
    [(and (> (first targetpos) (first nextpos))
          (> (first (rest targetpos)) (first (rest nextpos))))
     (change-direction move-next-in-north-east-se dir "ne" nextpos targetpos lop steps)]
     [(and (< (first targetpos) (first nextpos))
          (< (first (rest targetpos)) (first (rest nextpos))))
      (change-direction move-next-in-south-west-se dir "sw" nextpos targetpos lop steps)]))

;; move-next-in-south-west-se
(define (move-next-in-south-west-se dir startpos targetpos lop steps)
  (if (or
        (threat? (next-move-in-south-west startpos) lop)
        (deadlock-in-south-west? (next-move-in-south-west startpos) lop))
      (change-direction move-next-in-north-west-se dir "nw" startpos targetpos lop steps)
      (move-in-south-west-se dir (next-move-in-south-west startpos) targetpos lop (+ steps 1))))

;; move-in-south-west-se : String Position Position LisOfPosition Integer ->
;; GIVEN:
;; RETURNS:
;; STRATEGY:
(define (move-in-south-west-se dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found-in-different-direction? nextpos targetpos)
     (move-next-in-south-west-se dir nextpos targetpos lop steps)]
    [(target-link-found? nextpos targetpos)
     (possible-next-direction-se dir nextpos targetpos lop steps)]
    [else (if (or (threat? (next-move-in-south-east nextpos) lop)
          (deadlock-in-south-east? (next-move-in-south-east nextpos) lop))
      (move-next-in-south-west-se dir nextpos targetpos lop steps)
      (change-direction
       move-next-in-south-east-se dir "se" nextpos targetpos lop steps))]))

;; possible-next-direction-se
(define (possible-next-direction-se dir nextpos targetpos lop steps)
  (cond
    [(and (< (first targetpos) (first nextpos))
          (> (first (rest targetpos)) (first (rest nextpos))))
     (change-direction
      move-next-in-north-west-se dir "nw" nextpos targetpos lop steps)]
     [(and (> (first targetpos) (first nextpos))
          (< (first (rest targetpos)) (first (rest nextpos))))
      (change-direction
       move-next-in-south-east-se dir "se" nextpos targetpos lop steps)]))

;; move-next-in-north-west-se
(define (move-next-in-north-west-se dir nextpos targetpos lop steps)
  (if (or
        (threat? (next-move-in-north-west nextpos) lop)
        (deadlock-in-north-west?
         (next-move-in-north-west nextpos) lop)) ;; chek dir
      (change-direction
       move-next-in-north-east-se dir "ne" nextpos targetpos lop steps)
      (move-in-north-west-se
       dir (next-move-in-north-west nextpos) targetpos lop (+ steps 1))))

;; move-in-north-west-se
(define (move-in-north-west-se dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found-in-different-direction? nextpos targetpos)
     (check-next-direction-se dir nextpos targetpos lop steps)]
    [(and
      (< (first targetpos) (first (next-move-in-north-west nextpos)))
      (> (first (rest targetpos))
         (first (rest (next-move-in-north-west nextpos)))))
     (move-next-in-north-west-se dir nextpos targetpos lop steps)]
    [else (if (or
               (threat? (next-move-in-south-west nextpos) lop)
               (deadlock-in-south-west? (next-move-in-south-west nextpos) lop))
          (move-in-north-west-se dir nextpos targetpos lop steps)
          (change-direction
           move-next-in-south-west-se dir "sw" nextpos targetpos lop steps))]))

;; move-next-in-north-east-se
(define (move-next-in-north-east-se dir startpos targetpos lop steps)
  (cond
    [(and
      (or
        (threat? (next-move-in-north-east startpos) lop)
        (deadlock-in-north-east? (next-move-in-north-east startpos) lop))
      (or
        (threat? (next-move-in-south-east startpos) lop)
        (deadlock-in-south-east? (next-move-in-south-east startpos) lop)))
     false]
  [else (if (or
        (threat? (next-move-in-north-east startpos) lop)
        (deadlock-in-north-east? (next-move-in-north-east startpos) lop))
      (change-direction
       move-next-in-south-east dir "se" startpos targetpos lop steps)
      (move-in-north-east-se
       dir (next-move-in-north-east startpos) targetpos lop (+ steps 1)))]))

;; move-in-north-east-se
(define (move-in-north-east-se dir nextpos targetpos lop steps)
  (cond
    [(target-found? nextpos targetpos) (list (list dir steps))]
    [(target-link-found-in-different-direction? nextpos targetpos)
     (move-next-in-north-east-se dir nextpos targetpos lop steps)]
     [(target-link-found? nextpos targetpos)
     (possible-next-direction-se dir nextpos targetpos lop steps)]
    [else (if (or (threat? (next-move-in-south-east nextpos) lop)
          (deadlock-in-south-east? (next-move-in-south-east nextpos) lop))
      (move-next-in-north-east-se dir nextpos targetpos lop steps)
      (change-direction
       move-next-in-south-east-se dir "se" nextpos targetpos lop steps))]))

;; TEST:
(begin-for-test
  (check-equal?
   (move-in-north-east-se "ne" (list 1 3) (list 1 3) empty 2)
   (list (list "ne" 2)))
  (check-equal?
    (move-in-north-east-se "ne" (list 1 3) (list 3 1) (list (list -1 -1)) 2)
    (list (list "ne" 2) (list "se" 2)))
    (check-equal?
    (move-in-north-east-se "ne" (list 1 3) (list 3 1) (list (list -1 -1)) 0)
    (list (list "se" 2)))
    (check-equal?
    (move-in-north-east-se "ne" (list -1 0) (list 1 2) (list (list -1 -1)) 0)
    (list (list "ne" 2))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval-plan                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;eval-plan : Position ListOfPosition Plan ->  MaybePosition
;;GIVEN:
;;1. the starting position of the robot,
;;2. A list of the blocks on the board
;;3. A plan for the robot's motion
;;RETURNS:
;;The position of the robot at the end of executing the plan,
;;false if the plan sends the robot to or  through any block.
;;STRATEGY: Use Template for Plan on plan
(define (eval-plan startpos lop plan)
  (cond
    [(empty? plan) startpos]
    [else (eval-move startpos lop (first plan) (rest plan))]))

;; eval-move : Position ListOfPosition Move Plan -> MaybePosition
;; GIVEN:
;;1. the starting position of the robot
;;2. A list of the blocks on the board
;;3. A the next movement for robot from the given position
;;4. A plan for the robot's motion
;; RETURNS:
;;The position of the robot at the end of executing the move,
;;false if the plan sends the robot to or through any block.
;; STRATEGY: Divide into cases on move-direction
(define (eval-move startpos lop move plan)
  (cond
    [(string=? (first move) "sw")
     (calculate startpos lop (second move) plan - -)]
    [(string=? (first move) "nw")
     (calculate startpos lop (second move) plan - +)]
    [(string=? (first move) "se")
     (calculate startpos lop (second move) plan + -)]
    [(string=? (first move) "ne")
     (calculate startpos lop (second move) plan + +)]))

;; calculate : Position ListOfPosition PosInt Plan Symbol Symbol
;;             -> MaybePosition
;; GIVEN:
;;1. the starting position of the robot
;;2. A list of the blocks on the board
;;3. the steps left to go
;;4. A plan for the robot's motion
;;5. two symbols to help caculate the next possition
;; RETURNS:
;;The position of the robot at the end of executing the move,
;;false if the plan sends the robot to or  through any block.
;; STRATEGY: Recur on nextpos
;;           then use calculate
;;           Halt when count = 0
(define (calculate startpos lop count plan symbol1 symbol2)
  (local
    ((define nextpos (list (symbol1 (first startpos) 1)
                           (symbol2 (first (rest startpos)) 1))))
    (cond
      [(= count 0) (eval-plan startpos lop plan)]
      [(check-threat nextpos lop) false]
      [else (calculate nextpos lop (- count 1) plan symbol1 symbol2)])))

;; check-threat : Position ListOfPosition -> Boolean
;; GIVEN: a position and a list of the blocks on the board
;; RETURNS: true iff the position is sittiong on a block
;; STRATEGY: Use HOF ormap for ListOfPosition on lop
(define (check-threat nextpos lop)
  (ormap
   (lambda (block) (and
                    (= (first nextpos) (first block))
                    (= (second nextpos) (second block))))
   lop))
;;==============================================================================
;; TEST:
(define blocks (list (list 0 5) (list -3 5)))
(define plan (list (list "sw" 2) (list "se" 2) (list "nw" 1) (list "ne" 1)))
(begin-for-test
  (check-equal?
   (eval-plan (list -1 6) blocks plan)
   (list -1 4))
  (check-equal?
   (eval-plan (list 2 3) (list (list 3 4)) (list (list "ne" 2)))
   false))
;;==============================================================================