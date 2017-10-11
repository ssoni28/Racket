;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (outlines.rkt).
;; GOALS:
;; Write a data definition for FlatRep that defines exactly
;; the legal flat representations.
;; Included whatever invariants are applicable.
;; Provided: section numbers must be in order,
;; and you are not allowed to skip any section numbers.

;; An Example for an outline:
;; 1 The first section
;; 1.1 A subsection with no subsections
;; 1.2 Another subsection
;; 1.2.1 This is a subsection of 1.2
;; 1.2.2 This is another subsection of 1.2
;; 1.3 The last subsection of 1
;; 2 Another section
;; 2.1 More stuff
;; 2.2 Still more stuff

(require rackunit)
(require "extras.rkt")
(check-location "07" "outlines.rkt")

(provide
 legal-flat-rep?
 tree-rep-to-flat-rep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS:                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TREE REPRESENTATION:
;-------------------------------------------------------------------------------

;; An Outline is a ListOfSection

;; ListOfSection
;; A ListOfSection (LOS) is either
;; -- empty
;; -- (cons Section LOS)

;; Template:
;; los-fn : LOS -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (...
;;             (section-fn (first los))
;;             (los-fn (rest los)))]))

(define-struct section (str secs))

;; Interpretation:
;; A Section is a (make-section String ListOfSection)
;; where
;; str is the header text of the section
;; secs is the list of subsections of the section

;; Template:
;; section-fn : Section -> ??
;; (define (section-fn s)
;;   (... (section-str s)
;;        (los-fn (section-secs s))))

;; FLAT REPRESENTATION:
;-------------------------------------------------------------------------------

;; An FlatRep is a ListOfLine

;; ListOfLine
;; A ListOfLine (LOL) is either
;; -- empty
;; -- (cons Line LOL)

;; Template:
;; lol-fn : LOL -> ??
;; (define (lol-fn lol)
;;   (cond
;;     [(empty? lol) ...]
;;     [else (...
;;             (line-fn (first lol))
;;             (lol-fn (rest lol)))]))

(define-struct line (sec-numbers title))

;; Interpretation:
;; A Line is a (make-line ListOfPosInt String)
;; where
;; sec-numbers is the list of positve integers
;; representing the order of section.
;; title is is the header text of the section

;; Template:
;; line-fn : Line -> ??
;; (define (line-fn l)
;;   (... (lop-fn (line-sec-numbers l))
;;        (line-title l)))

;; ListOfPosInt
;; A ListOfPosInt (LOP) is either
;; -- empty
;; -- (cons PosInt LOP)

;; Template:
;; lop-fn : LOP -> ??
;; (define (lop-fn lop)
;;   (cond
;;     [(empty? lop) ...]
;;     [else (...
;;             (... (first lop))
;;             (lop-fn (rest lop)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF DATA DEFINITIONS                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Examples for tree representation:
(define outline-1
  (list 
   (make-section "The first section"
                 (list
                  (make-section "A subsection with no subsections" empty)
                  (make-section "Another subsection"
                                (list
                                 (make-section "This is a subsection of 1.2" empty)
                                 (make-section "This is another subsection of 1.2" empty)))
                  (make-section "The last subsection of 1" empty)))
   (make-section "Another section"
                 (list
                  (make-section "More stuff" empty)
                  (make-section "Still more stuff" empty)))))

;; Examples for Sections:
(define section2 
  (make-section "Another section"
                (list
                 (make-section "More stuff" empty)
                 (make-section "Still more stuff" empty))))

;; Examples for Lines:
(define line1 (make-line (list 1) "The first section"))
(define line1-1 (make-line (list 1 1) "A subsection with no subsections"))
(define line1-2 (make-line (list 1 2) "Another subsection"))
(define line1-2-1 (make-line (list 1 2 1) "This is a subsection of 1.2"))
(define line1-2-2 (make-line (list 1 2 2) "This is another subsection of 1.2"))
(define line1-3 (make-line (list 1 3) "The last subsection of 1"))
(define line2 (make-line (list 2) "Another section"))
(define line2-1 (make-line (list 2 1) "More stuff"))
(define line2-2 (make-line (list 2 2) "Still more stuff"))

;; Examples for flat representation:
(define flat-rep-1
  (list
   (make-line (list 1) "The first section")
   (make-line (list 1 1) "A subsection with no subsections")
   (make-line (list 1 2) "Another subsection")
   (make-line (list 1 2 1) "This is a subsection of 1.2")
   (make-line (list 1 2 2) "This is another subsection of 1.2")
   (make-line (list 1 3) "The last subsection of 1")
   (make-line (list 2) "Another section")
   (make-line (list 2 1) "More stuff")
   (make-line (list 2 2) "Still more stuff")))

(define lol1 (list line1-1 line1-2 line1-2-1 line1-2-2
                   line1-3 line2 line2-1 line2-2))

(define lol2 (list line2 line1-1 line1-2 line1-2-1 line1-2-2
                   line1-3 line2 line2-1 line2-2))

(define flat-rep-2 (list line1 line2 line1-1 line1-2 line1-2-1 line1-2-2
                         line1-3 line2 line2-1 line2-2))

(define flat-rep-3 (list line1 line1-2 line1-2-1 line1-2-2
                         line1-3 line2 line2-1 line2-2))

;; Examples for ListOfPosInt:
(define lop1 (list 1))
(define lop1-1 (list 1 1))
(define lop1-2 (list 1 2))
(define lop1-2-copy (list 1 2))
(define lop1-3 (list 1 3))
(define lop1-3-4 (list 1 3 4))
(define lop1-3-5 (list 1 3 5))
(define lop1-2-5 (list 1 2 5))
(define lop2 (list 2))
(define lop3 (list 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF EXAMPLES                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; legal-flat-rep?                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; legal-flat-rep? : ListOfLine -> Boolean
;; GIVEN: a list of lines
;; RETURNS: true iff it is a legal flat representation of an outline.
;; EXAMPLES: (legal-flat-rep? flat-rep-1) -> true
;; STRATEGY: Use Template for ListOfLine on lol
(define (legal-flat-rep? lol)
  (cond
    [(empty? lol) true]
    [else
     (and
      (all-legal-titles? lol)
      (list=? (line-sec-numbers (first lol)) (list 1))
      (all-legal-sec-numbers? (rest lol) (list 1)))]))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (legal-flat-rep? flat-rep-1)
   true
   " flat-rep-1 is a legal flat rep")
  (check-equal?
   (legal-flat-rep? flat-rep-2)
   false
   " flat-rep-2 is an illegal flat rep")
  (check-equal?
   (legal-flat-rep? flat-rep-3)
   false
   " flat-rep-3 is an illegal flat rep")
  (check-equal?
   (legal-flat-rep? empty)
   true
   " empty is an illegal flat rep"))
;===============================================================================

;; all-legal-titles? : ListOfLine -> Boolean
;; GIVEN: a list of lines
;; RETURNS: true iff all the lines have legal titles.
;; EXAMPLES: (all-legal-titles? flat-rep-1-1) -> false
;; STRATEGY: Use HOF andmap on lol
(define (all-legal-titles? lol)
  (andmap
   ;; Line -> Boolean
   ;; GIVEN: a line
   ;; RETURN: true iff the line's title is not an empty string
   (lambda (l) (not (string=? "" (line-title l))))
   lol))

;===============================================================================
;; TEST:
;; example for test:
(define lol1-1
  (list
   (make-line (list 1) "")))

(begin-for-test
  (check-equal?
   (all-legal-titles? lol1-1)
   false
   " empty title is illegal"))
;===============================================================================

;; all-legal-sec-numbers? : ListOfLine ListOfPosInt -> Boolean
;; GIVEN: a list of lines and a list of section numbers
;; WHERE: the given section number is a legal section number so far
;; RETURNS: true iff all the lines have legal section numbers(i.e. in order).
;; EXAMPLES: (all-legal-sec-numbers? flat-rep-1 (list 1)) -> true          
;; STRATEGY: Use Template for ListOfLines on lol
(define (all-legal-sec-numbers? lol legal-lop-so-far)
  (cond
    [(empty? lol) true]
    [else (and
           (line-sec-number-legal?
            (line-sec-numbers (first lol)) legal-lop-so-far)
           (all-legal-sec-numbers?
            (rest lol) (line-sec-numbers (first lol))))]))

;; line-sec-number-legal? : ListOfPosInt ListOfPosInt -> Boolean
;; GIVEN: two list of section numbers
;; WHERE: the second given section number is a legal section number so far
;; RETURNS: true iff the first list's section number are all legal.
;; EXAMPLES: (line-sec-number-legal? lop1-1 (list 1))
;;           -> true
;; STRATEGY: Combine Simpler Functions
(define (line-sec-number-legal? lop legal-lop-so-far)
  (or
   (next-line? lop legal-lop-so-far)
   (sub-section? lop legal-lop-so-far)
   (legal-new-sections? lop legal-lop-so-far)))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (all-legal-sec-numbers? lol1 (list 1))
   true
   "lol1 is a legal-sec-numbers list of line")
  (check-equal?
   (all-legal-sec-numbers? lol2 (list 1))
   false
   "lol2 is an illegal-sec-numbers list of line"))
;===============================================================================

;; next-line? : ListOfPosInt ListOfPosInt -> Boolean
;; GIVEN: two list of section numbers
;; WHERE: the second list of section numbers is the legal section number so far
;; RETURNS: true iff the first list represent the next legal
;;          line of the second list
;; EXAMPLES: (next-line? lop2 lop1) -> true
;; STRATEGY: Combine Simpler Functions
(define (next-line? lop legal-lop-so-far)
  (list=? lop (add1-to-last-lop legal-lop-so-far)))

;; sub-section? : ListOfPosInt ListOfPosInt -> Boolean
;; GIVEN: two list of section numbers
;; WHERE: the second list of section numbers is the legal section number so far
;; RETURNS: true iff the first list represent the next legal subsection
;;          of the second list
;; EXAMPLES: (sub-section? lop1-1 lop1) -> true
;; STRATEGY: Combine Simpler Functions
(define (sub-section? lop legal-lop-so-far)
  (list=? lop (add-sub-sec-to-lop legal-lop-so-far)))

;===============================================================================
;; TEST for next-line? & sub-section? :
(begin-for-test
  (check-equal?
   (next-line? lop2 lop1)
   true
   " next-line? : lop1's next line should be lop2 ")
  (check-equal?
   (next-line? lop3 lop1)
   false
   " next-line? : lop1's next line should not be lop3 ")
  (check-equal?
   (sub-section? lop1-1 lop1)
   true
   " sub-section? : lop1's subsection shoule be lop1-1 ")
  (check-equal?
   (sub-section? lop1-2-5 lop1-2)
   false
   " sub-section? : lop1-2's subsection shoule not be lop1-2-5 "))
;===============================================================================

;; legal-new-sections? : ListOfPosInt ListOfPosInt -> Boolean
;; GIVEN: two list of section numbers
;; WHERE: the second list of section numbers is the legal section number so far
;; RETURNS: true iff the first list is one of the new sections
;;          of the second list
;; EXAMPLES: (legal-new-sections? lop1-3 lop1-2-5) -> true
;;           (legal-new-sections? lop2 lop1-2-5) -> true
;; STRATEGY: Divide into cases on (empty? (rest legal-lop-so-far))
(define (legal-new-sections? lop legal-lop-so-far)
  (if (empty? (rest legal-lop-so-far))
      false
      (or (new-section? lop legal-lop-so-far)
          (legal-new-sections?
           lop (remove-last-elt legal-lop-so-far)))))

;; new-section? : ListOfPosInt ListOfPosInt -> Boolean
;; GIVEN: two list of section numbers
;; WHERE: the second list of section numbers is the legal section number so far
;; RETURNS: true iff the first list represent the new legal section which is of 
;;          the nearest order to the second list
;; EXAMPLES: (new-section? lop1-3 lop1-2-5) -> true
;;           (new-section? lop2 lop1-2-5) -> false
;; STRATEGY: Combine Simpler Functions
(define (new-section? lop legal-lop-so-far)
  (list=? lop
          (add1-to-last-lop (remove-last-elt legal-lop-so-far))))

;===============================================================================
;; TEST for legal-new-sections? & new-section? :
(begin-for-test
  (check-true
   (new-section? lop1-3 lop1-2-5))
  (check-false
   (new-section? lop2 lop1-2-5))
  (check-true
   (legal-new-sections? lop2 lop1-2-5))
  (check-false
   (legal-new-sections? lop3 lop1-2-5)))
;===============================================================================

;; HELPER FUNCTIONS:

;; list=? : ListOfPosInt ListOfPosInt -> Boolean
;; GIVEN: two list of section numbers
;; RETURNS: true iff the two lists are equal 
;; EXAMPLES: (list=? lop1-3 lop1-3-4) -> false
;;           (list=? lop1-3 lop1-3) -> true
;; STRATEGY: Use Template for ListOfPosInt on lop1 and lop2
(define (list=? lop1 lop2)
  (cond
    [(empty? lop1) (empty? lop2)]
    [(empty? lop2) false]
    [else (and
           (equal? (first lop1) (first lop2))
           (list=? (rest lop1) (rest lop2)))]))

;; remove-last-elt=? : ListOfPosInt -> ListOfPosInt
;; GIVEN: a list of section numbers
;; RETURNS: a list of section numbers with its last element removed
;; EXAMPLES: (remove-last-elt lop1-3) -> lop1
;; STRATEGY: Use Template for ListOfPosInt on lop
(define (remove-last-elt lop)
  (if (empty? (rest lop))
      empty
      (cons (first lop) (remove-last-elt (rest lop)))))

;===============================================================================
;; TEST for list=? and remove-last-elt :
(begin-for-test
  (check-equal?
   (list=? lop1-2 lop1-2-copy)
   true
   " list=? lop1-2 lop1-2-copy : lop1-2 should = lop1-2-copy")
  (check-equal?
   (list=? lop1-3 lop1-3-4)
   false
   " list=? lop1-3 lop1-3-4 : lop1-3 should not = lop1-3-4")
  (check-equal?
   (list=? lop1-3-4 lop1-3)
   false
   " list=? lop1-3-4 lop1-3 : lop1-3-4 should not = lop1-3")
  (check-equal?
   (list=? lop1-3-4 lop1-3-5)
   false
   " list=? lop1-3-4 lop1-3-5 : lop1-3-4 should not = lop1-3-5")
  (check-equal?
   (remove-last-elt lop1-3)
   lop1
   " remove-last-elt=? : lop1-3 -> lop1"))

;===============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-rep-to-flat-rep                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-rep-to-flat-rep : Outline -> FlatRep                       
;; GIVEN: the representation of an outline as a list of Sections
;; RETURNS: the flat representation of the outline
;; WHERE: the section number of an outline started with (list 1)
;; EXAMPLES: (tree-rep-to-flat-rep outline-1)
;;           -> flat-rep-1
;; STRATEGY: Call a more general function
(define (tree-rep-to-flat-rep los)
  (treeRep-to-flatRep los (list 1)))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (tree-rep-to-flat-rep outline-1)
   flat-rep-1
   "outline-1 -> flat-rep-1"))
;===============================================================================

;; treeRep-to-flatRep : Outline ListOfPosInt -> FlatRep
;; GIVEN: a list of sections and a list of section numbers for the first section
;; RETURNS: the flat representation of the outline
;; EXAMPLES: (tree-rep-to-flat-rep outline-1 (list 1))
;;           -> flat-rep-1
;; STRATEGY: Use Template for ListOfSection on los
(define (treeRep-to-flatRep los lop)
  (cond
    [(empty? los) empty]
    [else (append
           (section-to-line (first los) lop)
           (treeRep-to-flatRep (rest los) (add1-to-last-lop lop)))]))

;; section-to-line : Section ListOfPosInt -> Line
;; GIVEN: a section and a list of section numbers lop
;; WHERE: lop is the given section's section numbers
;; RETURNS: a list of lines representing the given section and its sub-sections
;; EXAMPLES: (section-to-line section2 (list 2))
;;           -> (list line2 line2-1 line2-2)
;; STRATEGY: Use Template for Section on s
(define (section-to-line s lop)
  (cons
   (make-line lop (section-str s))
   (treeRep-to-flatRep (section-secs s) (add-sub-sec-to-lop lop))))

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (section-to-line section2 (list 2))
   (list line2 line2-1 line2-2)
   "section2 -> (list line2 line2-1 line2-2)"))
;===============================================================================

;; add-sub-sec-to-lop : ListOfPosInt -> ListOfPosInt
;; GIVEN: a list of section numbers, lop
;; WHERE: lop is the root section's section numbers
;; RETURNS: a new list of lop with a new section number 1 added
;;          to the end of lop
;; EXAMPLES: (add-sub-sec-to-lop (list 1))
;;           -> (list 1 1)
;; STRATEGY: Use HOF foldr on lop
(define (add-sub-sec-to-lop lop)
  (foldr
   cons
   (list 1)
   lop))

;; add1-to-last-lop : ListOfPosInt -> ListOfPosInt
;; GIVEN: a list of section numbers lop
;; WHERE: lop is the root section's section numbers
;; RETURNS: a new lop with the last element added 1
;; EXAMPLES: (add1-to-last-lop (list 1))
;;           -> (list 2)
;; STRATEGY: Use Template for ListOfPosInt on lop
(define (add1-to-last-lop lop)
  (if (empty? (rest lop))
      (list (+ 1 (first lop)))
      (cons (first lop) (add1-to-last-lop (rest lop)))))











