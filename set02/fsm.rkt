;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; fsm.rkt
;; Implementing FSM for sequence (a | b)* c (a | b)* d (e | f)*

(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)

(provide
 initial-state
 next-state
 accepting-state?
 error-state?
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITION:
;; A State is one of:
;; -- SS  Interp.: intermediate State accepting a,b or c
;; -- AB  Interp.: intermediate State accepting a,b or c
;; -- ABC Interp.: Intermediate State accepting a,b or d
;; -- EF  Interp.: Final State accepting only e and f 
;; -- ER  Interp.: Error State, Illegal sequence

(define SS  "Start, Expects to see a, b, or c next")
(define AB  "intermediate State, Expects to see a, b or c next")
(define ABC "expects to see a, b or d next")
(define EF  "Accepting State also expects e or f")
(define ER  "Error : Illegal sequence")

;; A MachineInput is precisely one of "a" "b" "c" "d" "e" "f" strings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CONSTANTS:
(define MACHINE-INPUT-A "a")
(define MACHINE-INPUT-B "b")
(define MACHINE-INPUT-C "c")
(define MACHINE-INPUT-D "d")
(define MACHINE-INPUT-E "e")
(define MACHINE-INPUT-F "f")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial-state : Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;; of your machine.  The given number is ignored.
;; EXAMPLES:
;; (initial-state 1000)    -> SS
;; (initial-state 10000)   -> SS
;; (initial-state 100000)  -> SS
;; DESIGN STRATEGY: Combine Simpler Functions
(define (initial-state number)
  SS)
;; UNIT-TEST:
(begin-for-test
  (check-equal?
   (initial-state 1000)
   SS
   "Should Return Initial State i.e SS"))

;; next-state : State MachineInput -> State
;; GIVEN: a state of the machine and a machine input
;; RETURNS: the state that should follow the given input.
;; EXAMPLES:
;;  (next-state SS "a")   -> AB
;;  (next-state SS "x")   -> ER
;;  (next-state SS "")    -> ER
;;  (next-state SS "ABC") -> ER
;; DESIGN STRATEGY: Condition on State state
(define (next-state state input)
  (cond
    [(string=? state SS)  (state-after-ss input) ]
    [(string=? state AB)  (state-after-ab input) ]
    [(string=? state ABC) (state-after-abc input)]
    [(string=? state EF)  (state-after-ef input) ]
    [else ER]))

(begin-for-test
  (check-equal?
   (next-state SS "a")
   AB
   "Next State should be AB since SS expects a, b and c")
  (check-equal?
   (next-state AB "b")
   AB
   "Next State should be AB since state AB Expects a, b and c")
  (check-equal?
   (next-state AB "c")
   ABC
   "Next State should be ABC since state AB Expects a,b and c")
  (check-equal?
   (next-state ABC "g")
   ER
   "Next State should be ER since state g is an invalid input")
  (check-equal?
   (next-state ABC "d")
   EF
   "Next State should be EF since state ABC Expects a,b and d")
  (check-equal?
   (next-state EF "a")
   ER
   "Next State should be ER since state EF Expects e and f")
  (check-equal?
   (next-state SS "")
   ER
   "Next State should be ER since empty string is undefined input")
  (check-equal?
   (next-state ER "abcabde")
   ER
   "Next State should be ER since there is no way out form ER state")
  (check-equal?
   (next-state SS "abc")
   ER
   "Next State should be ER since 'abc' is not a valid MachineInput")
  (check-equal?
   (next-state SS "x")
   ER
   "Next State should be ER since state x is invalid input")
  (check-equal?
   (next-state "ABCDEF" "x")
   ER
   "Next State should be ER since state 'ABCDEF' is invalid State"))

;; state-after-ss : String -> State
;; PURPOSE:
;; GIVEN: a MachineInput input.
;; RETURN: State that follows the SS State
;; depending on the input string following the FSM.
;; EXAMPLES:
;;  (state-after-ss "a") -> AB
;;  (state-after-ss "c") -> ABC
;;  (state-after-ss "e") -> ER
;; DESIGN-STRATEGY: Cases on MachineInput
(define (state-after-ss input)
  (cond
    [(string=? MACHINE-INPUT-A input) AB ]
    [(string=? MACHINE-INPUT-B input) AB ]
    [(string=? MACHINE-INPUT-C input) ABC]
    [else ER]))
;; Tests:
(begin-for-test
  (check-equal?
   (state-after-ss "a")
   AB
   "State After SS with MachineInput 'a' should be AB")
  (check-equal?
   (state-after-ss "b")
   AB
   "State After SS with MachineInput 'b' should be AB")
  (check-equal?
   (state-after-ss "c")
   ABC
   "State After SS with MachineInput 'c' should be ABC")
  (check-equal?
   (state-after-ss "e")
   ER
   "State After SS with MachineInput 'e' should be ER"))

;; state-after-ab : String -> State
;; PURPOSE:
;; GIVEN: a MachineInput input.
;; RETURN: State that follows the AB State
;; depending on the input string following the FSM.
;; EXAMPLES:
;;  (state-after-ab "a") -> AB
;;  (state-after-ab "c") -> ABC
;;  (state-after-ab "e") -> ER
;;  (state-after-ab "b") -> AB
;; DESIGN-STRATEGY: Cases on MachineInput
(define (state-after-ab input)
  (cond
    [(string=? MACHINE-INPUT-A input ) AB ]
    [(string=? MACHINE-INPUT-B input ) AB ]
    [(string=? MACHINE-INPUT-C input ) ABC]
    [else ER]))

;; TESTS:
(begin-for-test
  (check-equal?
   (state-after-ab "a")
   AB
   "State After AB with MachineInput 'a' should be AB")
  (check-equal?
   (state-after-ab "b")
   AB
   "State After AB with MachineInput 'b' should be AB")
  (check-equal?
   (state-after-ab "c")
   ABC
   "State After AB with MachineInput 'c' should be ABC")
  (check-equal?
   (state-after-ab "e")
   ER
   "State After AB with MachineInput 'e' should be ER"))


;; state-after-abc : String -> State
;; PURPOSE:
;; GIVEN: a MachineInput input.
;; RETURN: State that follows the ABC State
;; depending on the input string following the FSM.
;; EXAMPLES:
;;  (state-after-abc "a") -> ABC
;;  (state-after-abc "b") -> ABC
;;  (state-after-abc "d") -> EF
;;  (state-after-abc "e") -> ER
;; DESIGN-STRATEGY: Cases on MachineInput
(define (state-after-abc input)
  (cond
    [(string=? MACHINE-INPUT-A input ) ABC]
    [(string=? MACHINE-INPUT-B input ) ABC]
    [(string=? MACHINE-INPUT-D input ) EF]
    [else ER]))

;; TESTS:
(begin-for-test
  (check-equal?
   (state-after-abc "a")
   ABC
   "State After ABC with MachineInput 'a' should be ABC")
  (check-equal?
   (state-after-abc "b")
   ABC
   "State After ABC with MachineInput 'b' should be ABC")
  (check-equal?
   (state-after-abc "d")
   EF
   "State After ABC with MachineInput 'd' should be EF")
  (check-equal?
   (state-after-abc "e")
   ER
   "State After ABC with MachineInput 'e' should be ER"))

;; state-after-ef : String -> State
;; PURPOSE:
;; GIVEN: a MachineInput input.
;; RETURN: State that follows the EF State
;; depending on the input string following the FSM.
;; EXAMPLES:
;; (state-after-ef "a") -> ER
;; (state-after-ef "b") -> ER
;; (state-after-ef "f") -> EF
;; (state-after-ef "e") -> EF
;; DESIGN-STRATEGY: Cases on MachineInput
(define (state-after-ef input)
  (cond
    [(string=? MACHINE-INPUT-E input ) EF]
    [(string=? MACHINE-INPUT-F input ) EF]
    [else ER]))

;; TESTS:
(begin-for-test
  (check-equal?
   (state-after-ef "a")
   ER
   "State After EF with MachineInput 'a' should be ER")
  (check-equal?
   (state-after-ef "b")
   ER
   "State After EF with MachineInput 'b' should be ER")
  (check-equal?
   (state-after-ef "f")
   EF
   "State After EF with MachineInput 'f' should be EF")
  (check-equal?
   (state-after-ef "e")
   EF
   "State After EF with MachineInput 'e' should be EF"))


;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state
;; EXAMPLES:
;;  (accepting-state? EF) -> true
;;  (accepting-state? SS) -> false
;;  (accepting-state? ER) -> false
;;  (accepting-state? AB) -> false
;; DESIGN STRATEGY: Combine simpler functions
(define (accepting-state? state)
  (if (string=? state EF) true false))
;;TESTS:
(begin-for-test
  (check-equal?
   (accepting-state? EF)
   true
   "EF is an Accepting State return true")
  (check-equal?
   (accepting-state? SS)
   false
   "SS is not an Accepting State return false")
  (check-equal?
   (accepting-state? ER)
   false
   "ER is not an Accepting State return false")
  (check-equal?
   (accepting-state? AB)
   false
   "AB is not an Accepting State return false"))

;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff there is no path (empty or non-empty) from the given
;; state to an accepting state
;; EXAMPLES:
;;  (error-state? ER) -> true
;;  (error-state? EF) -> true
;;  (error-state? SS) -> true
;; DESIGN STRATEGY : Combine Simpler functions 
(define (error-state? state)
  (if (string=? state ER) true false))
;;TESTS:
(begin-for-test
  (check-equal?
   (error-state? ER)
   true
   "ER is an Error State return true")
  (check-equal?
   (error-state? EF)
   false
   "EF is not an Error State return false")
  (check-equal?
   (error-state? SS)
   false
   "SS is not an Error State return false"))


