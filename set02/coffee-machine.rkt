;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Implementing coffee machine
;; coffee-machine.rkt

(require rackunit)
(require "extras.rkt")

(provide
 initial-machine
 machine-next-state
 machine-output
 machine-remaining-coffee
 machine-remaining-chocolate
 machine-bank
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS :
(define EMPTY-BANK 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DATA DEFINITIONS :

(define-struct machine-state(cups-of-coffee cups-of-hot-chocolate bank-amount))

;; A MachineState is a
;;  (make-machine-state NonNegInt NonNegInt PosInt)
;; Interpretation:
;;  cups-of-coffee is the number of cups of coffee in the coffee machine
;;  cups-of-hot-chocolate is the number of cups of hot chocolate in the coffee
;;  machine.
;;  bank-amount is the amount of money in the machine container, called bank,
;;  that contains all the money it has kept from customers' purchase.
;;
;; Template :
#|(define (machine-state-fn ms)
  (...
   (machine-state-cups-of-coffee ms)
   (machine-state-cups-of-hot-chocolate ms)
   (machine-state-bank-amount ms))) |#

;; A CustomerInput is one of
;; -- a PosInt
;; interp:
;; insert the specified amount of money, in cents
;; -- "coffee"
;; interp:
;; request a coffee
;; -- "hot chocolate"
;; interp:
;; request a hot chocolate
;; -- "change"
;; interp:
;; return all the unspent money that the customer
;; has inserted.
;;
;; Template :
#|(define (ci-fn input)
    (cond
      [(integer? input) ...]
      [(string=? input "coffee") ...]
      [(string=? input "hot chocolate") ...]
      [(string=? input "change") ...])) |#

;; A MachineOutput is one of
;; -- "coffee"
;; interp:
;; machine dispenses cup of coffee
;; -- "hot chocolate"
;; interp:
;; machine dispenses a cup of hot chocolate
;; -- "Out of Item"
;; interp:
;; machine displays "Out of Item"
;; -- a PosInt
;; interp:
;; machine releases the specified amount of money, in cents
;; -- "Nothing"
;; interp:
;; the machine does nothing
;;
;; Template:
#|(define (mo-fn output)
  (cond
    [(string=? output "coffee") ...]
    [(string=? output "hot chocolate") ...]
    [(string=? output "Out of Item") ...]
    [(integer? output) ...]
    [(string=? output "Nothing") ...])) |#
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN : a number of cups of coffee and of hot chocolate
;; RETURNS : the state of a machine loaded with the given number of
;;           cups of coffee and of hot chocolate, with an empty bank.
;; EXAMPLES :
;; (initial-machine 12 20) -> (make-machine-state 12 20 0)
;; (initial-machine 0 2)   -> (make-machine-state 0 2 0)
;; DESIGN STRATEGY : Use template for MachineState
(define (initial-machine coffee chocolate)
  (make-machine-state coffee chocolate EMPTY-BANK))
;; TEST :
(begin-for-test
  (check-equal?
   (initial-machine 12 20)
   (make-machine-state 12 20 0)
   "The initial machine state should be 12 cups of coffee and 20 cups of
    hot chocolate and empty bank amount")
  (check-equal?
   (initial-machine 0 2)
   (make-machine-state 0 2 0)
   "The initial machine state should be 0 cups of coffee and 2 cups of
    hot chocolate and empty bank amount"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN : a machine state and a customer input
;; RETURNS : the state of the machine that should follow the customer's input
;; EXAMPLES :
;; (machine-next-state (make-machine-state 12 20 150) "coffee") ->
;; (make-machine-state 11 20 150)
;; (machine-next-state (make-machine-state 10 12 0) 60) ->
;; (make-machine-state 10 12 60)
;; (machine-next-state (make-machine-state 10 12 60) "hot chocolate") ->
;; (make-machine-state 10 11 60)
;; (machine-next-state (make-machine-state 10 11 70) "change") ->
;; (make-machine-state 10 11 0)
;; DESIGN STRTEGY : Use template for CustomerInput on input
(define (machine-next-state ms input)
  (cond
    [(integer? input)
     (make-machine-state (machine-state-cups-of-coffee ms)
                         (machine-state-cups-of-hot-chocolate ms)
                         (+ (machine-state-bank-amount ms) input))]
    [(string=? input "coffee") (dispense-coffee ms)]
    [(string=? input "hot chocolate") (dispense-chocolate ms)]
    [(string=? input "change")
     (make-machine-state (machine-state-cups-of-coffee ms)
                         (machine-state-cups-of-hot-chocolate ms)
                         0)]))
;; TEST :
(begin-for-test
  (check-equal?
   (machine-next-state (make-machine-state 12 20 150) "coffee")
   (make-machine-state 11 20 150)
   "The next machine state after customer input coffee should have number
    of cups of coffee 11 which is one less than the previous value 12.")
  (check-equal?
   (machine-next-state (make-machine-state 10 12 0) 60) 
   (make-machine-state 10 12 60)
   "The machine state after customer inserts 60 cents
    should have bank amount updated to 60 cents"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions:
;;
;; dispense-coffee : MachineState -> MachineState
;; GIVEN : a machine state
;; RETURNS : machine state after dispatching a cup of coffee
;; EXAMPLES :
;; (dispense-coffee (make-machine-state 2 20 150)) ->
;;                  (make-machine-state 1 20 150)
;; DESIGN STRATEGY : Use template for MachineState on ms
(define (dispense-coffee ms)
  (cond
    [(can-dispense-coffee? ms)
     (make-machine-state (- (machine-state-cups-of-coffee ms) 1)
                         (machine-state-cups-of-hot-chocolate ms)
                         (machine-state-bank-amount ms))]
    [else (make-machine-state (machine-state-cups-of-coffee ms)
                              (machine-state-cups-of-hot-chocolate ms)
                              (machine-state-bank-amount ms))]))
;; TEST:
(begin-for-test
  (check-equal?
   (dispense-coffee (make-machine-state 2 20 150))
   (make-machine-state 1 20 150)
   "The machine state after dispensing a cup of coffee has 1 cup of coffee"))
;;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; can-dispense-coffee? : MachineState -> Boolean
;; GIVEN : a machine state
;; RETURNS : true iff machine state has sufficient coffee and bank amount
;;           more than 150 cents to buy a coffee.
;; EXAMPLES :
;; (can-dispense-coffee? (make-machine-state 2 20 150)) ->
;;                      true
;; DESIGN STRATEGY: combine simpler functions
(define (can-dispense-coffee? ms)
  (if (and (> (machine-state-cups-of-coffee ms) 0)
           (>= (machine-state-bank-amount ms) 150))
      true
      false))
;; TEST :
(begin-for-test
  (check-equal?
   (can-dispense-coffee? (make-machine-state 2 20 150))
   true
   "The machine state has enough no of cups of coffee and bank amount
    so it shouls return true"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dispense-chocolate : MachineState -> MachineState
;; GIVEN : a machine state
;; RETURNS : machine state after dispatching a cup of hot chocolate
;; EXAMPLES :
;; (dispense-chocolate (make-machine-state 2 20 150)) ->
;;                     (make-machine-state 1 20 150)
;; DESIGN STRATEGY : Use template for MachineState on ms
(define (dispense-chocolate ms)
  (cond
    [(can-dispense-chocolate? ms)
     (make-machine-state (machine-state-cups-of-coffee ms)
                         (- (machine-state-cups-of-hot-chocolate ms) 1)
                         (machine-state-bank-amount ms))]
    [else (make-machine-state (machine-state-cups-of-coffee ms)
                              (machine-state-cups-of-hot-chocolate ms)
                              (machine-state-bank-amount ms))]))
;; TEST:
(begin-for-test
  (check-equal?
   (dispense-chocolate (make-machine-state 2 20 150))
   (make-machine-state 2 19 150)
   "The machine state after dispensing a cup of coffee has 1 cup of coffee"))
;;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; can-dispense-chocolate? : MachineState -> Boolean
;; GIVEN : a machine state
;; RETURNS : true iff machine state has sufficient hot chocolate
;;           and bank amount more than 60 cents to buy a chocolate.
;; EXAMPLES :
;; (can-dispense-chocolate? (make-machine-state 2 20 30)) -> false
;;                      
;; DESIGN STRATEGY: combine simpler functions
(define (can-dispense-chocolate? ms)
  (if (and (> (machine-state-cups-of-hot-chocolate ms) 0)
           (>= (machine-state-bank-amount ms) 60))
      true
      false))
;; TEST :
(begin-for-test
  (check-equal?
   (can-dispense-chocolate? (make-machine-state 2 20 30))
   false
   "The machine state has enough no of cups of hot chocolate but bank amount
    is less than 60 cents so it shouls return false"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN : a machine state and a customer input
;; RETURNS : a MachineOutput that describes the machine's response to the
;;           customer input
;; EXAMPLES :
;; (machine-output (make-machine-state 2 20 150) "coffee") -> "coffee"
;; (machine-output (make-machine-state 0 20 150) "coffee") -> "Out of Item"
;; (machine-output (make-machine-state 12 2 75) "hot chocolate") -> "hot chocolate"
;; (machine-output (make-machine-state 1 22 25) "change") -> 25
;; (machine-output (make-machine-state 11 12 10) "coffee") -> Nothing
;; DESIGN STRATEGY : use template for CustomerInput on input
(define (machine-output ms input)
  (cond
    [(integer? input) "nothing"]
    [(string=? input "coffee") (try-dispense-coffee ms)]
    [(string=? input "hot chocolate") (try-dispense-chocolate ms)]
    [(string=? input "change") (check-machine-state-bank-amount ms)]))
;; TEST :
(begin-for-test
  (check-equal?
   (machine-output (make-machine-state 2 20 150) "coffee")
   "coffee"
   "The machine output should be coffee")
  (check-equal?
   (machine-output (make-machine-state 0 20 150) "coffee")
   "Out of Item"
   "The machine output should be out of item")
  (check-equal?
   (machine-output (make-machine-state 12 2 75) "hot chocolate")
   "hot chocolate"
   "The machine output should be hot chocolate")
  (check-equal?
   (machine-output (make-machine-state 1 22 25) "change")
   25
   "The machine output should be 25 cents")
  (check-equal?
   (machine-output (make-machine-state 11 12 10) "coffee")
   "Nothing"
   "The machine output should be nothing"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; machine-remaining-coffee : MachineState -> NonNegInt
;; GIVEN : a machine state
;; RETURNS : the number of cups of coffee left in the machine
;; EXAMPLES :
;; (machine-remaining-coffee (make-machine-state 10 2 75)) -> 10
;; DESIGN STRATEGY : use template for MachineState on ms
(define (machine-remaining-coffee ms )
  (machine-state-cups-of-coffee ms))
;; TEST :
(begin-for-test
  (check-equal?
   (machine-remaining-coffee (make-machine-state 10 2 75))
   10
   "The remaining number of cups of coffee should be 10"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; machine-remaining-chocolate : MachineState -> NonNegInt
;; GIVEN : a machine state
;; RETURNS : the number of cups of hot chocolate left in the machine.
;; EXAMPLES :
;; (machine-remaining-chocolate (make-machine-state 10 2 75)) -> 2
;; DESIGN STRATEGY : use template for MachineState on ms
(define (machine-remaining-chocolate ms )
  (machine-state-cups-of-hot-chocolate ms))
;; TEST :
(begin-for-test
  (check-equal?
   (machine-remaining-chocolate (make-machine-state 10 2 75))
   2
   "The remaining number of cups of hot chocolate should be 2"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; machine-bank : MachineState -> NonNegInt
;; GIVEN : a machine state
;; RETURNS : the amount of money in the machine's bank, in cents.
;; EXAMPLES :
;; (machine-bank (make-machine-state 10 2 75)) -> 75 cents
;; DESIGN STRATEGY : use template for MachineState on ms
(define (machine-bank ms )
  (machine-state-bank-amount ms))
;; TEST :
(begin-for-test
  (check-equal?
   (machine-bank (make-machine-state 1 22 75))
   75
   "The amount of money in the machine's bank should be 75 cents"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions:
;;
;; try-dispense-coffee : MachineState -> String
;; GIVEN : a machine state
;; RETURNS : coffe if machine as sufficient coffee otherwise out of item
;; EXAMPLES :
;; (try-dispense-coffee (make-machine-state 1 22 150)) -> "coffee"
;; DESIGN STRATEGY : combine simpler functions
(define (try-dispense-coffee ms)
  (if (check-machine-state-coffee? ms) 
      (check-machine-bank-for-coffee ms)
      "Out of Item"))
;; TEST :
(begin-for-test
  (check-equal?
   (try-dispense-coffee (make-machine-state 1 22 150))
   "coffee"
   "The result should be coffee"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; check-machine-state-coffee? : MachineState -> Boolean
;; GIVEN : a machine state
;; RETURNS : true iff machine has the remaining
;;           number of cups of coffee greater than zero.
;; EXAMPLES :
;; (check-machine-state-coffee? (make-machine-state 0 12 150)) -> false
;; DESIGN STRATEGY : combine simpler functions
(define (check-machine-state-coffee? ms)
  (if (> (machine-remaining-coffee ms) 0)
      true
      false))
;; TEST :
(begin-for-test
  (check-equal?
   (check-machine-state-coffee? (make-machine-state 0 12 150))
   false
   "The result should be false"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; check-machine-bank-for-coffee : MachineState -> String
;; GIVEN : a machine state
;; RETURNS : coffee if the bank amount is sufficient to buy
;;           a coffee of 150 cents otherwise nothing.
;; EXAMPLES :
;; (check-machine-bank-for-coffee (make-machine-state 12 1 150)) -> "coffee"
;; DESIGN STRATEGY : combine simpler functions
(define (check-machine-bank-for-coffee ms)
  (if (>= (machine-bank ms) 150)
      "coffee"
      "Nothing"))
;; TEST :
(begin-for-test
  (check-equal?
   (check-machine-bank-for-coffee (make-machine-state 12 1 150))
   "coffee"
   "The result should be coffee"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; try-dispense-chocolate : MachineState -> String
;; GIVEN : a machine state
;; RETURNS : hot chocolate if machine as sufficient coffee
;;           otherwise out of item
;; EXAMPLES : 
;; (try-dispense-chocolate (make-machine-state 1 22 150)) -> "hot chocolate"
;; DESIGN STRATEGY : combine simpler functions
(define (try-dispense-chocolate ms)
  (if (check-machine-state-chocolate? ms)
      (check-machine-bank-for-chocolate ms)
      "Out of Item"))
;; TEST :
(begin-for-test
  (check-equal?
   (try-dispense-chocolate (make-machine-state 1 22 150))
   "hot chocolate"
   "the result should be hot chocolate"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; check-machine-state-chocolate? : MachineState -> Boolean
;; GIVEN : a machine state
;; RETURNS : machine output in string depending on the remaining
;;           number of cups of hot chocolate
;; EXAMPLES :
;; (check-machine-state-chocolate? (make-machine-state 1 0 150)) -> false
;; DESIGN STRATEGY : combine simpler functions
(define (check-machine-state-chocolate? ms)
  (if (> (machine-remaining-chocolate ms) 0)
      true
      false))
;; TEST :
(begin-for-test
  (check-equal?
   (check-machine-state-chocolate? (make-machine-state 1 0 150))
   false
   "The result should be false"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; check-machine-bank-for-chocolate : MachineState -> String
;; GIVEN : a machine state
;; RETURNS : hot chocolate if machine has enough bank amount
;;           to buy a hot chocolate of 60 cents.
;; EXAMPLES :
;; (check-machine-bank-for-chocolate (make-machine-state 1 2 60)) ->
;;                                   "hot chocolate"
;; DESIGN STRATEGY : combine simpler functions
(define (check-machine-bank-for-chocolate ms)
  (if (>= (machine-bank ms) 60)
      "hot chocolate"
      "Nothing"))
;; TEST :
(begin-for-test
  (check-equal?
   (check-machine-bank-for-chocolate (make-machine-state 1 2 60))
   "hot chocolate"
   "The result should be hot chocolate"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; check-machine-state-bank-amount : MachineState -> PosInt
;; GIVEN : a machine state
;; RETURNS : machine releases the specified amount of money, in cents
;;           (if available)      
;; EXAMPLES :
;; (check-machine-state-bank-amount (make-machine-state 12 1 0)) -> "Nothing"
;; DESIGN STRATEGY : combine simpler functions
(define (check-machine-state-bank-amount ms)
  (if (> (machine-state-bank-amount ms) 0)
      (machine-state-bank-amount ms)
      "Nothing"))
;; TEST :
(begin-for-test
  (check-equal?
   (check-machine-state-bank-amount (make-machine-state 12 1 0))
   "Nothing"
   "The result should be nothing"))



