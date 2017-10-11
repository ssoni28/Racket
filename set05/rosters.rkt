;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rosters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; rosters.rkt
;; Given a list of (student, class) pairs. Implementing
;; rosters.rkt to produces the class roster for each
;; class that has at least one student enrolled.

(require rackunit)
(require "extras.rkt")

(provide
 make-enrollment
 enrollment-student
 enrollment-class
 make-roster
 roster-classname
 roster-students
 roster=?
 rosterset=?
 enrollments-to-rosters)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A SetOfX is a ListOfX without duplication.

;; A ListOfX (LOX) is either
;; -- empty
;; -- (cons X LOX)

;; Interpretation:
;; Template:
;; lox-fn : ListOfX -> ??
;; (define (lox-fn lox)
;;   (cond
;;     [(empty? lox) ...]
;;     [else(...
;;            (first lox)
;;            (lox-fn (rest lox)))]))

(define-struct enrollment (student class))

;; Interpretation:              
;; An Enrollment is a
;; (make-enrollment Student Class)
;; student represents a Student enrolled in class
;; class represents a Class
;; (make-enrollment s c) represents the assertion that
;; student s is enrolled in class c.

;; Template:
;; enrollment-fn : Enrollment -> ??
;; (define (enrollment-fn e)
;;   (... (enrollment-student e)
;;        (enrollment-class e)))

;; A SetOfEnrollment is a ListOfEnrollment without duplication.
;; A ListOfEnrollment (LOE) is either
;; -- empty
;; -- (cons Enrollment LOE)

;; Template:
;; (define (loe-fn loe)
;;   (cond
;;     [(empty? loe) ...]
;;     [else(...
;;            (enrollment-fn (first loe))
;;            (loe-fn (rest loe)))]))

(define-struct roster(classname students))

;; Interpretation:
;; A ClassRoster is a
;; (make-roster Class SetOfStudent)
;; classname represents a Class
;; students is the SetOfStudent with no duplicates enrolled in a class
;; (make-roster c ss) represents that the students in
;; class c are exactly the students in set ss.

;; Template:
;; roster-fn : ClassRoster -> ??
;; (define (roster-fn roster)
;;   (... (roster-classname roster)
;;        (roster-students roster)))

;; A SetOfClassRoster is a ListOfClassRoster without duplication.
;; A ListOfClassRoster (LOR) is either
;; -- empty
;; -- (cons ClassRoster LOR)

;; Template:
;; (define (lor-fn lor)
;;   (cond
;;     [(empty? lor) ...]
;;     [else(...
;;            (roster-fn (first lor))
;;            (lor-fn (rest lor)))]))

;; Student is unspecified, so assuming Student as,
(define-struct student (name))

;; Interpretation:
;; A Student is a (make-student String)
;; name is the name of the student

;; Template:
;; student-fn : Student -> ??
;; (define (student-fn student)
;;    (... (student-name student)))

;; A SetOfStudent is a ListOfStudent without duplication.
;; A ListOfStudent (LOS) is either
;; -- empty
;; -- (cons Student LOS)

;; Template:
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else(...
;;            (student-fn (first los))
;;            (los-fn (rest los)))]))

;; Class is unspecified, so assuming Class as,
(define-struct class (name))

;; Interpretation:
;; A Class is a (make-class String)
;; name is the name of the class

;; Template:
;; class-fn : Class -> ??
;; (define (class-fn class)
;;    (... (class-name class)))

;; A SetOfClass is a ListOfClass without duplication.
;; A ListOfClass (LOC) is either
;; -- empty
;; -- (cons Class LOC)

;; Template:
;; (define (loc-fn loc)
;;   (cond
;;     [(empty? loc) ...]
;;     [else(...
;;            (class-fn (first loc))
;;            (loc-fn (rest loc)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; roster=?                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; roster=? : ClassRoster ClassRoster -> Boolean
;; GIVEN: two classrosters
;; RETURNS: true iff the two arguments represent the same roster
;; STRATEGY: combine simpler functions
(define (roster=? roster1 roster2)
  (and (equal? (roster-classname roster1) (roster-classname roster2))
       (equal-sets-of-students? (roster-students roster1)
                                (roster-students roster2))))
;; TEST:
(begin-for-test
  (check-true (roster=? (make-roster "PDP" (list "vish" "swati" "arjun"))
                        (make-roster "PDP" (list "arjun" "swati" "vish"))))
  (check-false (roster=? (make-roster "PDP" (list "vish" "swati" "arjun"))
                         (make-roster "Networks" (list "arjun" "swati" "vish"))))
  (check-false (roster=? (make-roster "PDP" (list (make-student "vish")
                                                  (make-student "swati")
                                                  (make-student "arjun")))
                         (make-roster "Networks" (list (make-student "arjun")
                                                       (make-student "swati")
                                                       (make-student "vish"))))))

;; equal-sets-of-students? : SetOfStudent SetOfStudent -> Boolean
;; GIVEN: two SetOfStudent's
;; RETURNS: true iff the given sets are equal
;; STRATEGY: Call a more generic function
(define (equal-sets-of-students? set1 set2)
  (equal-sets? set1 set2 subset?))
#|
(define (equal-sets-of-students? set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))|#

;; TEST:
(begin-for-test
  (check-true (equal-sets-of-students? (list "vish" "swati") (list "swati" "vish")))
  (check-false (equal-sets-of-students? (list "vish" "swati") (list "vish")))
  (check-false (equal-sets-of-students? (list "vish") (list "vish" "swati")))
  (check-false (equal-sets-of-students? (list "swati") (list "soni")))
  (check-true (equal-sets-of-students? (list (make-student "vish")
                                             (make-student "swati"))
                                       (list (make-student "swati")
                                             (make-student "vish")))))

;; my-member? : Student SetOfStudent -> Boolean
;; GIVEN: a student and a set of students
;; RETURNS: true iff the given student is
;;          a member of given set
;; STRATEGY: Call a more generic function
(define (my-member? student set)
  (is-member? student set equal?))
#|
(define (my-member? student set)
  (ormap
   ;; Student -> Boolean
   (lambda (member) (equal? student member))
   set))|#
;; TEST:
(begin-for-test
  (check-true (my-member? "vish" (list "vish" "soni")))
  (check-true (my-member? (make-student "vish")
                          (list (make-student "vish") (make-student "swati")))))

;; subset? : SetOfStudent SetOfStudent -> Boolean
;; GIVEN: two SetOfStudents's
;; RETUNS: true iff the first set is the subset of second set
;; STRATEGY: Call a more generic function
(define (subset? set1 set2)
  (is-subset? set1 set2 my-member?))
#|
(define (subset? set1 set2)
  (andmap
   ;; Student -> Boolean
   (lambda (x) (my-member? x set2))
   set1))
|#
;; TEST:
(begin-for-test
  (check-true (subset? (list "vish" "soni") (list "vish" "soni" "swati"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rosterset=?                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rosterset=? : SetOfClassRoster SetOfClassRoster -> Boolean
;; GIVEN: two sets of ClassRoster's
;; RETURNS: true iff the two arguments represent the same set of rosters
;; STRATEGY: Call a more generic function
(define (rosterset=? set1 set2)
  (equal-sets? set1 set2 roster-subset?))
#|
(define (rosterset=? set1 set2)
  (and
   (roster-subset? set1 set2)
   (roster-subset? set2 set1)))|#
;; TEST:
(begin-for-test
  (check-true (rosterset=? (list (make-roster "PDP" (list "vish"))
                                 (make-roster "PDP" (list "swati")))
                           (list (make-roster "PDP" (list "swati"))
                                 (make-roster "PDP" (list "vish")))))
  (check-false (rosterset=? (list (make-roster "PDP" (list "vish"))
                                  (make-roster "PDP" (list "swati")))
                            (list (make-roster "PDP" (list "vish"))
                                  (make-roster "Networks" (list "soni"))))))

;; roster-subset? : SetOfClassRoster SetOfClassRoster -> Boolean
;; GIVEN: two SetOfClassRoster's
;; RETUNS: true iff the first set is the subset of second set
;; STRATEGY: Call a more generic function
(define (roster-subset? set1 set2)
  (is-subset? set1 set2 is-roster-member?))
#|
(define (roster-subset? set1 set2)
  (andmap
   ;; ClassRoster -> Boolean
   (lambda (roster) (is-roster-member? roster set2))
   set1))|#
;; TEST:
(begin-for-test
  (check-true (roster-subset? (list (make-roster "PDP" (list "vish"))
                                    (make-roster "PDP" (list "swati")))
                              (list (make-roster "PDP" (list "vish"))
                                    (make-roster "PDP" (list "swati"))
                                    (make-roster "Networks" (list "arjun"))))))

;; is-roster-member? : ClassRoster SetOfClassRoster -> Boolean
;; GIVEN: a classroster and a set of class roster
;; RETURNS: true iff the given classroster is an
;;          element of the given set
;; STRATEGY: Call a more generic function
(define (is-roster-member? roster set)
  (is-member? roster set roster=?))
#|
(define (is-roster-member? roster set)
  (ormap
   ;; ClassRoster -> Boolean
   (lambda (member) (roster=? roster member))
   set))|#
;; TEST:
(begin-for-test
  (check-true (is-roster-member? (make-roster "PDP" (list "vish"))
                                 (list (make-roster "PDP" (list "vish"))
                                       (make-roster "PDP" (list "swati")))))
  (check-false (is-roster-member? (make-roster "Networks" (list "vish"))
                                  (list (make-roster "PDP" (list "vish"))
                                        (make-roster "PDP" (list "swati"))))))

;; equal-sets? : SetOfX SetOfX (SetOfX SetOfX -> Boolean) -> Boolean
;; GIVEN: two sets of X's and a function
;; RETURNS: true iff the two arguments represent the same set of X
;; STRATEGY: combine simpler functions
(define (equal-sets? set1 set2 func)
  (and
   (func set1 set2)
   (func set2 set1)))

;; is-subset? : SetOfX SetOfX (X SetOfX -> Boolean) -> Boolean
;; GIVEN: two SetOfX's and a function
;; RETUNS: true iff the first set is the subset of second set
;; STRATEGY: Use HOF andmap on set1
(define (is-subset? set1 set2 func)
  (andmap
   ;; X -> Boolean
   (lambda (x) (func x set2))
   set1))

;; is-member? : X SetOfX (X X -> Boolean) -> Boolean
;; GIVEN: a X and a set of X and a function
;; RETURNS: true iff the given x is an
;;          element of the given set
;; STRATEGY: Use HOF ormap on set
(define (is-member? x set func)
  (ormap
   ;; X -> Boolean
   (lambda (member) (func x member))
   set))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enrollments-to-rosters:                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enrollments-to-rosters: SetOfEnrollment -> SetOfClassRoster
;; GIVEN: a set of enrollments
;; RETURNS: the set of class rosters for the given enrollments
;; STRATEGY: combine simpler functions
(define (enrollments-to-rosters set)
  (get-rosters (unique-class set) set))
;; TEST:
(begin-for-test
  (check-random (enrollments-to-rosters
                 (list (make-enrollment "John" "PDP")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks")))
                (list
                 (make-roster "PDP" (list "John" "Feng" "Amy"))
                 (make-roster "Networks" (list "Kathryn" "Amy"))))
  (check-random (enrollments-to-rosters
                 empty)
                empty)
  (check-random (enrollments-to-rosters
                 (list (make-enrollment (make-student "vish")
                                        (make-class "PDP"))
                       (make-enrollment (make-student "soni")
                                        (make-class "PDP"))))
                (list (make-roster (make-class "PDP")
                                   (list (make-student "vish")
                                         (make-student "soni"))))))

;; get-rosters : SetOfClass SetOfEnrollment -> SetOfClassRoster
;; GIVEN: a set of classes and a set of enrollments
;; RETURNS: a set of class rosters for the given enrollments
;; STRATEGY: Use HOF map on unique-classes
(define (get-rosters unique-classes set)
  (map
   ;; Class -> ClassRoster
   (lambda (x) (make-roster x
                            (get-students-for-class set x)))
   unique-classes))
;; TEST:
(begin-for-test
  (check-random (get-rosters (list "PDP" "Networks")
                             (list (make-enrollment "swati" "PDP")
                                   (make-enrollment "vish" "Networks")
                                   (make-enrollment "soni" "PDP")
                                   (make-enrollment "like" "PDP")
                                   (make-enrollment "kill" "Networks")))
                (list (make-roster "PDP" (list "swati" "soni" "like"))
                      (make-roster "Networks" (list "vish" "kill")))))

;; get-students-for-class : SetOfEnrollment Class -> ListOfStudent
;; GIVEN: a set of enrollments and a class
;; RETURNS: the list of students enrolled in given class
;; STRATEGY: Use HOF foldr on set
(define (get-students-for-class set class)
  (foldr
   ;; Enrollment ListOfStudent -> ListOfStudent
   (lambda (x y) (if (equal? (enrollment-class x) class)
                     (cons (enrollment-student x) y)
                     y))
   empty
   set))
;; TEST:
(begin-for-test
  (check-random (get-students-for-class (list (make-enrollment "John" "PDP")
                                              (make-enrollment "Kat" "Network")
                                              (make-enrollment "Feng" "PDP")
                                              (make-enrollment "Imy" "PDP")
                                              (make-enrollment "Am" "Network"))
                                        "PDP")
                (list "John" "Feng" "Imy"))
  (check-random (get-students-for-class empty empty)
                empty))

;; unique-class : SetOfEnrollment -> SetOfClass
;; GIVEN: a set of enrollments
;; RETURNS: a set of classes for the given set of enrollments
;; STRATEGY: Use HOF foldr on set
(define (unique-class set)
  (foldr
   ;; Enrollment SetOfClass -> SetOfClass
   (lambda (x y)  (add-unique-class (enrollment-class x) y))
   empty
   set))
;; TEST:
(begin-for-test
  (check-random (unique-class (list (make-enrollment "John" "PDP")
                                    (make-enrollment "Kathryn" "Networks")
                                    (make-enrollment "Feng" "PDP")
                                    (make-enrollment "Amy" "PDP")
                                    (make-enrollment "Amy" "Networks")))
                (list "PDP" "Networks")))

;; add-unique-class : Class SetOfClass -> SetOfClass
;; GIVEN: a class and a set of classes
;; RETURNS: a set of unique classes after adding new
;;          class only iff it is not duplicate
;; STRATEGY: combine simpler functions
(define (add-unique-class class set)
  (if (is-duplicate-class? class set)
      set
      (cons class set)))
;; TEST:
(begin-for-test
  (check-random (add-unique-class "PDP" (list "Networks"))
                (list "PDP" "Networks"))
  (check-random (add-unique-class "Networks" (list "Networks"))
                (list "Networks")))

;; is-duplicate-class? : Class SetOfClass -> Boolean
;; GIVEN: a class and a set of classes
;; RETURNS: true iff the class is already
;;          present in the given set
;; STRATEGY: combine simpler functions
(define (is-duplicate-class? class set)
  (if (empty? set)
      false
      (is-duplicate-class-in-non-empty-set? class set)))
;; TEST:
(begin-for-test
  (check-false (is-duplicate-class? "PDP" empty))
  (check-true (is-duplicate-class? "PDP" (list "PDP"))))

;; is-duplicate-class-in-non-empty-set? : Class SetOfClass -> Boolean
;; GIVEN: a class and a non-empty set of classes
;; RETURNS: true iff the class is already present
;;          in given non-empty set
;; STRATEGY: Use HOF ormap on set
(define (is-duplicate-class-in-non-empty-set? class set)
  (ormap
   ;; Class -> Boolean
   (lambda (member) (equal? class member))
   set))
;; TEST:
(begin-for-test
  (check-true (is-duplicate-class-in-non-empty-set? "PDP" (list "PDP")))
  (check-false (is-duplicate-class-in-non-empty-set? (make-class "Networks")
                                                      (list (make-class "PDP")))))
