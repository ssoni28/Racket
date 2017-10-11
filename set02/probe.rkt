;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Implementing space probe
;; probe.rkt

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(provide
 probe-at
 probe-turned-left
 probe-turned-right
 probe-forward
 probe-north?
 probe-south?
 probe-east?
 probe-west?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS:
(define CIRCLE
  (circle 20 "solid" "blue"))
(define MAX-NORTH-BOUNDARY -173)
(define MAX-SOUTH-BOUNDARY 173)
(define MAX-WEST-BOUNDARY -173)
(define MAX-EAST-BOUNDARY 173)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DATA DEFINITIONS:
;;
(define-struct probe(CIRCLE x y direction))

;; A Probe is a
;;   (make-probe CIRCLE Integer Integer String).
;; It represents a space probe to Pluto which is just landed.
;; INTERPRETATION :
;;   CIRCLE is a constant which represents the structure of
;;   probe whose radius is 20 cm , which is blue in color and
;;   has solid mode.(Just used for more clarity of problem)
;;   x field represents the x coordinate of center of probe 
;;   y field represents the y coordinate of center of probe 
;;   direction represents the direction of probe
;;
;; TEMPLATE:
#|(define (probe-fn p)
  (...
   (probe-x p)
   (probe-y p)
   (probe-direcion p)
   (probe? p)))|#
;;
;; A TRAP is a square having sides of 347 cm which is construted
;; by the Plutonians having its center at origin.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; probe-at: Integer Integer -> Probe
;; GIVEN: an x-coordinate and a y-coordinate
;; WHERE: these coordinates leave the robot entirely inside the trap
;; RETURNS: a probe with its ceter at those coordinates, facing north.
;; EXAMPLES:
;; (probe-at 0 -40) -> (make-probe CIRCLE 0 -40 "north")
;; (probe-at 20 50) -> (make-probe CIRCLE 20 50 "north")
;; DESIGN-STRATEGY: Use template for Probe on p
(define (probe-at x y)
  (make-probe CIRCLE x y "north"))
;; TEST:
(begin-for-test
  (check-equal?
   (probe-at 0 -40)
   (make-probe CIRCLE 0 -40 "north")
   "The probe center is at x-coordinate 0 y-coordinate -40, facing north")
  (check-equal?
   (probe-at 20 50)
   (make-probe CIRCLE 20 50 "north")
   "The probe center is at x-coordinate 20 y-coordiante 50, facing north"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; probe-turned-left:  Probe -> Probe
;; probe-turned-right: Probe -> Probe
;; GIVEN: a probe
;; RETUNRS: a probe like the original, but turned 90 degrees
;;          either left or right.
;; EXAMPLES:
;; (probe-turned-left  (make-probe CIRCLE 20 30 "north"))  ->
;;                     (make-probe CIRCLE 20 30 "west")
;; (probe-turned-left  (make-probe CIRCLE 20 30 "south"))  ->
;;                     (make-probe CIRCLE 20 30 "east")
;; (probe-turned-right (make-probe CIRCLE 20 30 "north"))  ->
;;                     (make-probe CIRCLE 20 30 "east")
;; (probe-turned-right (make-probe CIRCLE 20 30 "east"))   ->
;;                     (make-probe CIRCLE 20 30 "south")
;; DESIGN-STRATEGY: Use template for Probe on p
;;
;; Function definition for probe-turned-left:
(define (probe-turned-left p)
  (make-probe CIRCLE (probe-x p)
              (probe-y p)
              (next-left-direction (probe-direction p))))
;;
;; Function definition for probe-turned-right:
(define (probe-turned-right p)
  (make-probe CIRCLE (probe-x p)
              (probe-y p)
              (next-right-direction (probe-direction p))))
;; TESTS:
(begin-for-test
  (check-equal?
   (probe-turned-left (make-probe CIRCLE 20 30 "north"))
   (make-probe CIRCLE 20 30 "west")
   "The probe originaly facing north should now face west after turning 90
    degrees to its left")
  (check-equal?
   (probe-turned-left (make-probe CIRCLE 20 30 "south"))
   (make-probe CIRCLE 20 30 "east")
   "The probe originaly facing south should now face east after turning 90
    degrees to its left.")
  (check-equal?
   (probe-turned-right (make-probe CIRCLE 20 30 "west"))
   (make-probe CIRCLE 20 30 "north")
   "The probe originaly facing west should now face north after turning 90
    degrees to its right.")
  (check-equal?
   (probe-turned-right (make-probe CIRCLE 20 30 "east"))
   (make-probe CIRCLE 20 30 "south")
   "The probe originaly facing east should now face south after turning 90
    degrees to its right."))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions:
;;
;; next-left-direction: String -> String
;; GIVEN:   a direction of probe
;; RETURNS: a direction after rotating probe
;;          90 degrees to left of given
;;          direction.
;; EXAMPLES:
;; (next-left-direction "north") -> "west"
;; (next-left-direction "east")  -> "north"
;; DESIGN STRATEGY: cases on probe direction
(define (next-left-direction str)
  (cond
    [(string=? str "north") "west"]
    [(string=? str "west") "south"]
    [(string=? str "south") "east"]
    [(string=? str "east") "north"]))
;; TEST:
(begin-for-test
  (check-equal?
   (next-left-direction "north")
   "west"
   "The direction after rotating probe 90 degrees to left of north
    should be west")
  (check-equal?
   (next-left-direction "east")
   "north"
   "The direction after rotating probe 90 degrees to left of east
    should be north"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; next-right-direction: String -> String
;; GIVEN:   a direction of probe
;; RETURNS: a direction after rotating probe
;;          90 degrees to right of given
;;          direction.
;; EXAMPLES:
;; (next-right-direction "south") -> "west"
;; (next-right-direction "west")  -> "north"
;; DESIGN STRATEGY: cases on probe direction
(define (next-right-direction str)
  (cond
    [(string=? str "north") "east"]
    [(string=? str "east") "south"]
    [(string=? str "south") "west"]
    [(string=? str "west") "north"]))
;; TEST:
(begin-for-test
  (check-equal?
   (next-right-direction "south")
   "west"
   "The direction after rotating probe 90 degrees to right of south
    should be west")
  (check-equal?
   (next-right-direction "west")
   "north"
   "The direction after rotating probe 90 degrees to right of west
    should be north"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; probe-forward: Probe PosInt -> Probe
;; GIVEN: a probe and a distance
;; RETURNS: a probe like the given one, but moved forward
;;          by the specified distance. If moving forward the
;;          specified distance would cause the probe to hit
;;          any wall of the trap, then the probe should move
;;          as far as it can inside the trap, and then stop.
;; EXAMPLES:
;; (probe-forward (make-probe CIRCLE 0 0 "north") 70) ->
;;                (make-probe CIRCLE 0 -70 "north")
;; (probe-forward (make-probe CIRCLE 170 170 "south") 4) ->
;;                (make-probe CIRCLE 170 173 "south")
;; (probe-forward (make-probe CIRCLE 0 0 "west") 70) ->
;;                (make-probe CIRCLE -70 0 "west")
;; (probe-forward (make-probe CIRCLE 170 170 "east") 4) ->
;;                (make-probe CIRCLE 173 170 "east")
;; DESIGN STRATEGY: cases on probe direction
(define (probe-forward p distance)
  (cond
    [(probe-north? p) (move-forward-north p distance)]
    [(probe-south? p) (move-forward-south p distance)]
    [(probe-west? p) (move-forward-west p distance)]
    [(probe-east? p) (move-forward-east p distance)]))
;; TEST:
(begin-for-test
  (check-equal?
   (probe-forward (make-probe CIRCLE 0 0 "north") 70)
   (make-probe CIRCLE 0 -70 "north")
   "The probe is forwarded in the north direction by the
    distance 70 cm having x-coordinate 0 and y-coordinate
    -70")
  (check-equal?
   (probe-forward (make-probe CIRCLE 170 170 "south") 4) 
   (make-probe CIRCLE 170 173 "south")
   "The probe should move forward as far as it can inside the
    trapin the south direction for the given distance 4 cm so
    that it would not hit the wall and then stop.")
  (check-equal?
   (probe-forward (make-probe CIRCLE 0 0 "west") 70)
   (make-probe CIRCLE -70 0 "west")
   "The probe is forwarded in the west direction by the
    distance 70 cm having x-coordinate -70 and y-coordinate
    0")
  (check-equal?
   (probe-forward (make-probe CIRCLE 170 170 "east") 4) 
   (make-probe CIRCLE 173 170 "east")
   "The probe should move forward as far as it can inside the
    trapin the east direction for the given distance 4 cm so
    that it would not hit the wall and then stop."))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions:
;;
;; move-forward-north : Probe PosInt -> Probe
;; GIVEN: a probe and a distance
;; RETURNS: a probe like the given one, but moved forward
;;          by the specified distance in the north direction.
;;          If moving forward the specified distance would
;;          cause the probe to hit any wall of the trap, then
;;          the probe should move as far as it can inside the
;;          trap, and then stop.
;; EXAMPLES:
;; (move-forward-north (make-probe CIRCLE -170 -170 "north") 4) ->
;;                     (make-probe CIRCLE -170 -173 "north")
;; DESIGN STRATEGY: cases
(define (move-forward-north p distance)
  (cond
    [(would-not-hit-wall? p distance) (probe-forwarded p distance)]
    [else (probe-stopped p MAX-NORTH-BOUNDARY)]))
;; TEST:
(begin-for-test
  (check-equal?
   (move-forward-north (make-probe CIRCLE -170 -170 "north") 4)
   (make-probe CIRCLE -170 -173 "north")
   " The probe should be forwarded to the maximum boundary limit
     -173 of trap in north direction and then it is stopped"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; would-not-hit-wall?: Probe PosInt -> Boolean
;; GIVEN:   a probe and a distance in cm
;; RETURNS: true iff the movement of probe in given direction
;;          to the specified distance would not cause the
;;          probe to hit the wall.
;; EXAMPLES:
;; (would-not-hit-wall? (make-probe CIRCLE 0 20 "north") 20)  -> true
;; (would-not-hit-wall? (make-probe CIRCLE 0 -173 "north") 1) -> false
;; DESIGN STRATEGY: combine simpler functions
(define (would-not-hit-wall? p distance)
  (if (>= (probe-new-coordinate p distance) MAX-NORTH-BOUNDARY)
      true
      false))
;; TEST:
(begin-for-test
  (check-equal?
   (would-not-hit-wall? (make-probe CIRCLE 0 20 "north") 20)
   true
   "The probe will not hit the wall so the result should be true")
  (check-equal?
   (would-not-hit-wall? (make-probe CIRCLE 0 -173 "north") 1)
   false
   "The probe would hit the wall so the result should be false"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; probe-forwarded : Probe PosInt -> Probe
;; GIVEN: a probe and a distance in cm
;; RETURNS: a probe like the given one, but forwarded to
;;          the specified distance in the given probe's
;;          direction without hitting the wall.
;; EXAMPLES:
;; (probe-forwarded (make-probe CIRCLE 0 20 "north") 10) ->
;;                  (make-probe CIRCLE 0 10 "north")
;; DESIGN STRATEGY: Use template for Probe on p
(define (probe-forwarded p distance) 
  (make-probe CIRCLE (probe-x p)
              (probe-new-coordinate p distance)
              (probe-direction p)))
;; TEST:
(begin-for-test
  (check-equal?
   (probe-forwarded (make-probe CIRCLE 0 20 "north") 10) 
   (make-probe CIRCLE 0 10 "north")
   " The probe is forwarded to a distance of 10 cm in north direction
     and should have x-coordinate 0 and y-coordinate 10"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; probe-stopped : Probe Integer -> Probe
;; GIVEN:   a probe and the coordinate for maximum
;;          boundary in north direction
;; RETURNS: a probe like the given one, but moved
;;          forward by the specified distance in
;;          the north direction as far as it can
;;          inside the trap, and then stop.
;; EXAMPLES:
;; (probe-stopped (make-probe CIRCLE 20 -172 "north") MAX-NORTH-BOUNDARY) ->
;;                (make-probe CIRCLE 20 -173 "north")
;; DESIGN STRATEGY: Use template for Probe on p
(define (probe-stopped p boundary)
  (make-probe CIRCLE (probe-x p) boundary (probe-direction p)))
;; TEST:
(begin-for-test
  (check-equal?
   (probe-stopped (make-probe CIRCLE 20 -172 "north") MAX-NORTH-BOUNDARY)
   (make-probe CIRCLE 20 -173 "north")
   "The probe is forwarded to the maximum boundary in north direction
    -173 and then stopped"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; probe-new-coordinate : Probe PosInt -> Integer
;; GIVEN: a probe and a distance in cm
;; RETURNS: the new coordinate of probe moved
;;          forward by the specified distance depending
;;          on the direction of given probe.
;; EXAMPLES:
;; (probe-new-coordinate (make-probe CIRCLE 20 -172 "north") 1) -> -173
;; DESIGN STRATEGY: cases 
(define (probe-new-coordinate p distance)
  (cond
    [(probe-north? p) (- (probe-y p) distance)]
    [(probe-south? p) (+ (probe-y p) distance)]
    [(probe-west? p) (- (probe-x p) distance)]
    [(probe-east? p) (+ (probe-x p) distance)]
    ))                    
;; TEST:
(begin-for-test
  (check-equal?
   (probe-new-coordinate (make-probe CIRCLE 20 -172 "north") 1)
   -173
   "The new y-coordinate of probe should be -173"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; move-forward-south : Probe PosInt -> Probe
;; GIVEN: a probe and a distance
;; RETURNS: a probe like the given one, but moved forward
;;          by the specified distance in the south direction.
;;          If moving forward the specified distance would
;;          cause the probe to hit any wall of the trap, then
;;          the probe should move as far as it can inside the
;;          trap, and then stop.
;; EXAMPLES:
;; (move-forward-south (make-probe CIRCLE 170 170 "south") 4) ->
;;                     (make-probe CIRCLE 170 173 "south")
;; DESIGN STRATEGY: cases
(define (move-forward-south p distance)
  (cond
    [(would-not-hit-south-east-wall? p distance) (probe-forwarded p distance)]
    [else (probe-stopped p MAX-SOUTH-BOUNDARY)]))
;; TEST:
(begin-for-test
  (check-equal?
   (move-forward-south (make-probe CIRCLE 170 170 "south") 4)
   (make-probe CIRCLE 170 173 "south")
   " The probe should be forwarded to the maximum boundary limit
     173 of trap in south direction and then it is stopped"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; would-not-hit-south-east-wall?: Probe PosInt -> Boolean
;; GIVEN:   a probe and a distance in cm
;; RETURNS: true iff the movement of probe in south direction
;;          to the specified distance would not cause the
;;          probe to hit the wall.
;; EXAMPLES:
;; (would-not-hit-south-east-wall? (make-probe CIRCLE 0 20 "south") 20)  -> true
;; (would-not-hit-south-east-wall? (make-probe CIRCLE 0 173 "south") 1) -> false
;; DESIGN STRATEGY: combine simpler functions
(define (would-not-hit-south-east-wall? p distance)
  (if (<= (probe-new-coordinate p distance) MAX-SOUTH-BOUNDARY)
      true
      false))
;; TEST:
(begin-for-test
  (check-equal?
   (would-not-hit-south-east-wall? (make-probe CIRCLE 0 20 "south") 20)
   true
   "The probe will not hit the south wall of trap so the result should be true")
  (check-equal?
   (would-not-hit-south-east-wall? (make-probe CIRCLE 0 173 "south") 1)
   false
   "The probe would hit the south wall of trap so the result should be false"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; move-forward-west : Probe PosInt -> Probe
;; GIVEN: a probe and a distance
;; RETURNS: a probe like the given one, but moved forward
;;          by the specified distance in the west direction.
;;          If moving forward the specified distance would
;;          cause the probe to hit any wall of the trap, then
;;          the probe should move as far as it can inside the
;;          trap, and then stop.
;; EXAMPLES:
;; (move-forward-west (make-probe CIRCLE -170 -170 "west") 4) ->
;;                     (make-probe CIRCLE -173 -170 "west")
;; DESIGN STRATEGY: cases
(define (move-forward-west p distance)
  (cond
    [(would-not-hit-wall? p distance) (probe-forwarded-x p distance)]
    [else (probe-stopped-x p MAX-WEST-BOUNDARY)]))
;; TEST:
(begin-for-test
  (check-equal?
   (move-forward-west (make-probe CIRCLE -170 -170 "west") 4)
   (make-probe CIRCLE -173 -170 "west")
   " The probe should be forwarded to the maximum boundary limit
     -173 of trap in west direction and then it is stopped"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; probe-forwarded-x : Probe PosInt -> Probe
;; GIVEN: a probe and a distance in cm
;; RETURNS: a probe like the given one, but forwarded to
;;          the specified distance in the given probe's
;;          direction(west or east) without hitting the wall.
;; EXAMPLES:
;; (probe-forwarded-x (make-probe CIRCLE 0 20 "west") 10) ->
;;                  (make-probe CIRCLE -10 20 "west")
;; DESIGN STRATEGY: Use template for Probe on p
(define (probe-forwarded-x p distance) 
  (make-probe CIRCLE (probe-new-coordinate p distance)
              (probe-y p)
              (probe-direction p)))
;; TEST:
(begin-for-test
  (check-equal?
   (probe-forwarded-x (make-probe CIRCLE 0 20 "west") 10) 
   (make-probe CIRCLE -10 20 "west")
   " The probe is forwarded to a distance of 10 cm in west direction
     and should have x-coordinate -10 and y-coordinate 20"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; probe-stopped-x : Probe Integer -> Probe
;; GIVEN:   a probe and the coordinate for maximum
;;          boundary in given direction(east or west)
;; RETURNS: a probe like the given one, but moved
;;          forward by the specified distance in
;;          the given direction as far as it can
;;          inside the trap, and then stop.
;; EXAMPLES:
;; (probe-stopped-x (make-probe CIRCLE -172 20 "west") MAX-WEST-BOUNDARY) ->
;;                (make-probe CIRCLE -173 20 "west")
;; DESIGN STRATEGY: Use template for Probe on p
(define (probe-stopped-x p boundary)
  (make-probe CIRCLE boundary (probe-y p) (probe-direction p)))
;; TEST:
(begin-for-test
  (check-equal?
   (probe-stopped-x (make-probe CIRCLE -172 20 "west") MAX-WEST-BOUNDARY)
   (make-probe CIRCLE -173 20 "west")
   "The probe is forwarded to the maximum boundary in west direction
    -173 and then stopped"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; move-forward-east : Probe PosInt -> Probe
;; GIVEN: a probe and a distance
;; RETURNS: a probe like the given one, but moved forward
;;          by the specified distance in the east direction.
;;          If moving forward the specified distance would
;;          cause the probe to hit any wall of the trap, then
;;          the probe should move as far as it can inside the
;;          trap, and then stop.
;; EXAMPLES:
;; (move-forward-east (make-probe CIRCLE 170 170 "east") 4) ->
;;                     (make-probe CIRCLE 173 170 "east")
;; DESIGN STRATEGY: cases
(define (move-forward-east p distance)
  (cond
    [(would-not-hit-south-east-wall? p distance) (probe-forwarded-x p distance)]
    [else (probe-stopped-x p MAX-EAST-BOUNDARY)]))
;; TEST:
(begin-for-test
  (check-equal?
   (move-forward-east (make-probe CIRCLE 170 170 "east") 4)
   (make-probe CIRCLE 173 170 "east")
   " The probe should be forwarded to the maximum boundary limit
     173 of trap in east direction and then it is stopped"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; probe-north? : Probe -> Boolean
;; probe-south? : Probe -> Boolean
;; probe-east? : Probe -> Boolean
;; probe-west? : Probe -> boolean
;; GIVEN: a probe
;; RETURNS: whether the probe is facing in the specified direction.
;; EXAMPLES:
;; (probe-north? (make-probe CIRCLE 10 20 "north")) -> true
;; (probe-south? (make-probe CIRCLE 10 20 "north")) -> false
;; (probe-east? (make-probe CIRCLE 0 0 "east")) -> true
;; (probe-west? (make-probe CIRCLE 170 170 "west")) -> true
;; DESIGN STRATEGY: use cases on probe direction
;;
;; Function definition for probe-north?:
(define (probe-north? p)
  (cond
    [(string=? (probe-direction p) "north") true]
    [else false]))
;;
;; Function definition for probe-south?:
(define (probe-south? p)
  (cond
    [(string=? (probe-direction p) "south") true]
    [else false]))
;;
;; Function definition for probe-east?:
(define (probe-east? p)
  (cond
    [(string=? (probe-direction p) "east") true]
    [else false]))
;;
;; Function definition for probe-west?:
(define (probe-west? p)
  (cond
    [(string=? (probe-direction p) "west") true]
    [else false]))
;; TEST:
(begin-for-test
  (check-equal?
   (probe-north? (make-probe CIRCLE 10 20 "north"))
   true
   "The direction of given probe is north so it should return true")
  (check-equal?
   (probe-south? (make-probe CIRCLE 10 20 "north"))
   false
   "The direction of given probe is not south so it should return false")
  (check-equal?
   (probe-east? (make-probe CIRCLE 0 0 "east"))
   true
   "The direction of given probe is east so it should return true")
  (check-equal?
   (probe-west? (make-probe CIRCLE 170 170 "west"))
   true
   "The direction of given probe is west so it should return true"))







