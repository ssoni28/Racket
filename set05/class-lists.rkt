;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; class-lists.rkt

;; Professor Felleisen and Professor Shivers each keep their class lists on
;; slips of paper, one student on each slip. Professor Felleisen keeps his
;; list on slips of yellow paper. Professor Shivers keeps his list on slips
;; of blue paper.
;; Sometimes they have more than one slip for the same student. Sometimes
;; they record the student names first-name first; sometimes they record
;; the names last-name first.
;; One day, Professor Felleisen was walking up the stairs in WVH, talking 
;; to one of his graduate students. At the same time, Professor Shivers was
;; walking down the stairs, all the time talking to one of his graduate
;; students. They collided, and dropped all the slips containing their class
;; lists on the stairs, where they got all mixed up.

;; The aim of this program is to clean up this mess and return class-lists of
;; both Professors in which each student is represented only once. This is
;; implemented using HOFs wherever possible and appropriate.

(require rackunit)
(require "extras.rkt")

(provide
 felleisen-roster
 shivers-roster
 make-slip
 slip-color
 slip-name1
 slip-name2)

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

(define blue-slip
  (list (make-slip "blue" "ben" "vin")
        (make-slip "blue" "vin" "ben")
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
;; STRATEGY: combine simpler functions
(define (felleisen-roster los)
  (get-unique-slips (yellow-slips los)))
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

; yellow-slips : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: the given list of slips which are yellow in color
;; STRATEGY: Call more generic function
(define (yellow-slips los)
  (color-slips los "yellow"))
#|
(define (yellow-slips los)
  (filter
   ;; Slip -> Boolean
   (lambda (slip) (string=? (slip-color slip) "yellow"))
   los))|#

;; TEST:
(begin-for-test
  (check-not-equal?
   (yellow-slips los1)
   blue-slip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION DEFINITION: shivers-roster:                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all
;;          the students in Professor Shivers'
;;          class, without duplication.
;; STRATEGY: combine simpler functions
(define (shivers-roster los)
  (get-unique-slips (blue-slips los)))
;; TEST:
(begin-for-test
  (check-equal?
   (shivers-roster los1)
   shivers-los)
  "The list of slip for shivers-roster is invalid")

;; blue-slips : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: the given list of slips which are blue in color
;; STRATEGY: Call more generic function
(define (blue-slips los)
  (color-slips los "blue"))
#|
(define (blue-slips los)
  (filter
   ;; Slip -> Boolean
   (lambda (slip) (string=? (slip-color slip) "blue"))
   los))|#

;; TEST:
(begin-for-test
  (check-equal?
   (blue-slips los1)
   blue-slip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS DEFINITION:                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color-slips : ListOfSlip String -> ListOfSlip
;; GIVEN: a list of slips and a color
;; RETURNS: the given list of slips which are in given color
;; STRATEGY: Use HOF filter on los
(define (color-slips los color)
  (filter
   ;; Slip -> Boolean
   (lambda (slip) (string=? (slip-color slip) color))
   los))
;; TEST:
(begin-for-test
  (check-equal?
   (color-slips los1 "blue")
   blue-slip))

;; get-unique-slips : ListOfSlip -> ListOfSLip
;; GIVEN: a list of slips having blue color
;; RETURNS: the given list of slips without duplication
;; STRATEGY: Use HOF foldr on los
(define (get-unique-slips los)
 (foldr
  ;; Slip ListOfSlip -> ListOfSlip
  (lambda (x y) (add-unique-slips x y))
  empty
  los))

;; add-unique-slips : Slip ListOfSlip -> ListOfSlip
;; GIVEN: a slip and a list of unique slips
;; RETURNS: a list of unique slip after adding
;; given slip to it only iff it is not duplicate
;; STRATEGY: combine simpler functions
(define (add-unique-slips slip list)
  (if (is-duplicate-slip? slip list)
      list
      (cons slip list)))

;; is-duplicate-slip? : Slip ListOfSlip -> Boolean
;; GIVEN: a slip and a list of slips
;; RETURNS: true iff the given slip is present
;;          in the given list
;; STRATEGY: combine simpler functions
(define (is-duplicate-slip? slip list)
  (if (empty? list)
      false
      (is-duplicate-slip-in-non-empty-list? slip list)))

;; is-duplicate-slip-in-non-empty-list? : Slip ListOfSLip -> Boolean
;; GIVEN: a slip and a non-empty list of slips
;; RETURNS: true iff the given slip is present
;;          in the given non-empty list
;; STRATEGY: Use HOF ormap on los
(define (is-duplicate-slip-in-non-empty-list? slip los)
  (ormap
   (lambda (member) (is-names-equal? slip member))
   los))


;; is-names-equal? : Slip Slip -> Boolean
;; GIVEN: two slips 
;; RETURNS: true iff both the lists have same names
;; STRATEGY: combine simpler functions
(define (is-names-equal? slip1 slip2)
  (or (is-name-equal-in-order? slip1 slip2)
      (is-name-equal-not-in-order? slip1 slip2)))


;; is-name-equal-in-order? : Slip Slip  -> Boolean
;; GIVEN: two slips 
;; RETURNS: true iff both the lists have same first 
;;          and last names 
;; STRATEGY: combine simpler functions
(define (is-name-equal-in-order? slip1 slip2 )
  (and (string=? (slip-name1 slip1) (slip-name1 slip2))
       (string=? (slip-name2 slip1) (slip-name2 slip2))))


;; is-name-equal-not-in-order? : Slip Slip  -> Boolean
;; GIVEN: two slips 
;; RETURNS: true iff both the lists have first name
;;          same as last name and last name same as
;;          first name 
;; STRATEGY: combine simpler functions
(define (is-name-equal-not-in-order? slip1 slip2 )
  (and (string=? (slip-name1 slip1) (slip-name2 slip2))
       (string=? (slip-name2 slip1) (slip-name1 slip2))))


