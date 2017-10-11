;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; class-lists.rkt

(require rackunit)
(require "extras.rkt")

(provide
 felleisen-roster
 shivers-roster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS:                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct slip (color name1 name2))

;; Interpretation:
;; A Slip is a
;; (make-slip Color String String)
;; color is the color of the slip.
;; name1 can be the first or last name of a student.
;; name2 can be the first or last name of a student.

;; Template:
;; slip-fn : Slip -> ??
;; (define (slip-fn slip)
;;   (... (slip-color slip)
;;        (slip-name1 slip)
;;        (slip-name2 slip)))

;; A ListOfSlip (LOS) is either
;; -- empty
;; -- (cons Slip LOS)

;; Template:
;; los-fn : LOS -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else(...
;;            (slip-fn (first los))
;;            (los-fn (rest los)))]))

;; A Color is one of
;; -- "yellow"
;; -- "blue"

;; Template:
;; color-fn : Color -> ??
;; (define (color-fn color)
;;   (cond
;;     [(string=? color "yellow" ...]
;;     [(string=? color "blue" ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF DATA DEFINITIONS                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES FOR TESTING                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define los1
  (list (make-slip "yellow" "swati" "soni")
        (make-slip "blue" "ben" "vin")
        (make-slip "yellow" "soni" "swati")
        (make-slip "yellow" "vish" "kilewala")
        (make-slip "yellow" "arjun" "verma")
        (make-slip "blue" "vin" "ben")
        (make-slip "yellow" "vish" "kilewala")
        (make-slip "blue" "swati" "soni")
        (make-slip "blue" "shiva" "soni")
        (make-slip "blue" "loe" "ben")
        (make-slip "blue" "loe" "ben")))

(define felleisen-los
  (list (make-slip "yellow" "soni" "swati")
        (make-slip "yellow" "arjun" "verma")
        (make-slip "yellow" "vish" "kilewala")))

(define shivers-los
  (list (make-slip "blue" "vin" "ben")
        (make-slip "blue" "swati" "soni")
        (make-slip "blue" "shiva" "soni")
        (make-slip "blue" "loe" "ben")))

(define los2
  (list (make-slip "yellow" "swati" "soni")
        (make-slip "blue" "ben" "vin")
        (make-slip "yellow" "vish" "kilewala")
        (make-slip "yellow" "arjun" "verma")
        (make-slip "blue" "vin" "ben")
        (make-slip "blue" "swati" "soni")
        (make-slip "blue" "shiva" "soni")
        (make-slip "blue" "loe" "ben")
        (make-slip "blue" "loe" "ben")))

(define felleisen-los2
  (list (make-slip "yellow" "swati" "soni")
        (make-slip "yellow" "vish" "kilewala")
        (make-slip "yellow" "arjun" "verma")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF EXAMPLES                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION DEFINITION: felleisen-roster                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the
;;          students in Professor Felleisen's
;;          class, without duplication.
;; STRATEGY: Use template for ListOfSlip on los
(define (felleisen-roster los)
  (cond
    [(empty? los) empty]
    [else (if (is-felleisen-unique-slip? los)
              (cons (first los) (felleisen-roster (rest los)))
              (felleisen-roster (rest los)))]))
;; TEST: see test below

;; is-color-yellow? : Slip -> Boolean
;; GIVEN: a slip 
;; RETURNS: true iff the color of the
;;          given slip is yellow.
;; EXAMPLES:
;; (is-color-yellow? (make-slip "swati" "soni "yellow")) -> true
;; (is-color-yellow? (make-slip "swati" "soni "blue")) -> false
;; STRATEGY: Use template for Slip on slip
(define (is-color-yellow? slip)
  (string=? (slip-color slip) "yellow"))

;; is-felleisen-unique-slip? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff the slip is yellow and unique
;; STRATEGY: Use template for ListOfSlip on los
(define (is-felleisen-unique-slip? los)
  (and (is-color-yellow? (first los))
       (not (felleisen-duplicacy? (first los) (rest los)))))

;; felleisen-duplicacy? : Slip ListOfSlip -> Boolean
;; GIVEN: a slip and a list of slips
;; RETURNS: true iff the given slip is present in
;;          the given list of slips having yellow
;;          color and same names.
;; EXAMPLES:
;; (felleisen-duplicacy?
;;   (make-slip "blue" "swati" "soni") shivers-los)
;;   false
;; STRATEGY: Use template for ListOfSlip on los
(define (felleisen-duplicacy? slip los)
  (cond
    [(empty? los) false]
    [else (if (is-felleisen-names-equal? slip (first los))
          true
          (felleisen-duplicacy? slip (rest los)))]))
;; TEST:
(begin-for-test
  (check-equal?
   (felleisen-duplicacy? (make-slip "blue" "swati" "soni") shivers-los)
   false
   "The given slip is not present in yellow color"))

;; is-felleisen-names-equal? : Slip Slip -> Boolean
;; GIVEN: two slips
;; RETURNS: true iff both the lists have same names
;; STRATEGY: combine simpler functions
(define (is-felleisen-names-equal? slip1 slip2)
  (or (is-felleisen-names-equal-in-order? slip1 slip2)
      (is-felleisen-names-equal-in-unorder? slip1 slip2)))

;; is-felleisen-names-equal-in-order? : Slip Slip -> Boolean
;; GIVEN: two slips
;; RETURNS: true iff both the lists have same first 
;;          and last names and yellow in color
;; STRATEGY: combine simpler functions
(define (is-felleisen-names-equal-in-order? slip1 slip2)
  (and (string=? (slip-name1 slip1) (slip-name1 slip2))
       (string=? (slip-name2 slip1) (slip-name2 slip2))
       (is-color-yellow? slip2)))

;; is-felleisen-names-equal-in-unorder? : Slip Slip -> Boolean
;; GIVEN: two slips
;; RETURNS: true iff both the lists have first name
;;          same as last name and last name same as
;;          first name and yellow in color
;; STRATEGY: combine simpler functions
(define (is-felleisen-names-equal-in-unorder? slip1 slip2)
  (and (string=? (slip-name1 slip1) (slip-name2 slip2))
       (string=? (slip-name2 slip1) (slip-name1 slip2))
       (is-color-yellow? slip2)))

;; TEST:
(begin-for-test
  (check-equal?
   (felleisen-roster los1)
   felleisen-los
   "The list of slip for felleisen-roster is invalid")
  (check-equal?
   (felleisen-roster los2)
   felleisen-los2
   "The list of slip for felleisen-roster is invalid"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION DEFINITION: shivers-roster:                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all
;;          the students in Professor Shivers'
;;          class, without duplication.
;; STRATEGY: Use template for ListOfSlip on los
(define (shivers-roster los)
  (cond
    [(empty? los) empty]
    [else (if (is-shivers-unique-slip? los)
              (cons (first los) (shivers-roster (rest los)))
              (shivers-roster (rest los)))]))

;; is-color-blue? : Slip -> Boolean
;; GIVEN: a slip 
;; RETURNS: true iff the color of the
;;          given slip is blue.
;; EXAMPLES:
;; (is-color-blue? (make-slip "swati" "soni "yellow")) -> false
;; (is-color-blue? (make-slip "swati" "soni "blue")) -> true
;; STRATEGY: Use template for Slip on slip
(define (is-color-blue? slip)
  (string=? (slip-color slip) "blue"))

;; is-shivers-unique-slip? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff the slip is blue and unique
;; STRATEGY: Use template for ListOfSlip on los
(define (is-shivers-unique-slip? los)
  (and (is-color-blue? (first los))
       (not (shivers-duplicacy? (first los) (rest los)))))

;; shivers-duplicacy? : Slip ListOfSlip -> Boolean
;; GIVEN: a slip and a list of slips
;; RETURNS: true iff the given slip is present in
;;          the given list of slips having blue
;;          color and same names.
;; EXAMPLES:
;; (shivers-duplicacy?
;;   (make-slip "blue" "swati" "soni")
;;   shivers-los) -> true
;; STRATEGY: Use template for ListOfSlip on los
(define (shivers-duplicacy? slip los)
  (cond
    [(empty? los) false]
    [else (if (is-shivers-names-equal? slip (first los))
              true
              (shivers-duplicacy? slip (rest los)))]))
;; TEST:
(begin-for-test
  (check-equal?
   (shivers-duplicacy? (make-slip "blue" "swati" "soni") shivers-los)
   true
   "The given slip is already present in given list"))

;; is-shivers-names-equal? : Slip Slip -> Boolean
;; GIVEN: two slips
;; RETURNS: true iff both the lists have same names
;; STRATEGY: combine simpler functions
(define (is-shivers-names-equal? slip1 slip2)
  (or (is-shivers-names-equal-in-order? slip1 slip2)
      (is-shivers-names-equal-in-unorder? slip1 slip2)))

;; is-shivers-names-equal-in-order? : Slip Slip -> Boolean
;; GIVEN: two slips
;; RETURNS: true iff both the lists have same first 
;;          and last names and blue in color
;; STRATEGY: combine simpler functions
(define (is-shivers-names-equal-in-order? slip1 slip2)
  (and (string=? (slip-name1 slip1) (slip-name1 slip2))
       (string=? (slip-name2 slip1) (slip-name2 slip2))
       (is-color-blue? slip2)))

;; is-shivers-names-equal-in-unorder? : Slip Slip -> Boolean
;; GIVEN: two slips
;; RETURNS: true iff both the lists have first name
;;          same as last name and last name same as
;;          first name and blue in color
;; STRATEGY: combine simpler functions
(define (is-shivers-names-equal-in-unorder? slip1 slip2)
  (and (string=? (slip-name1 slip1) (slip-name2 slip2))
       (string=? (slip-name2 slip1) (slip-name1 slip2))
       (is-color-blue? slip2)))

;; TEST:
(begin-for-test
  (check-equal?
   (shivers-roster los1)
   shivers-los)
  "The list of slip for shivers-roster is invalid")



