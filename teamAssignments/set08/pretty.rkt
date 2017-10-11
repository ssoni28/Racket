;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;(pretty.rkt).
;; GOALS:
;; Your task is to write a program that contains a pretty-printer for Exprs.
;; The rules for rendering the expression as a list of lines are as follows:
;; The expression should be rendered on a single line if it fits
;; within the specified width.
;; Otherwise, render the subexpressions in a stacked fashion, like
;; 	(+ expr1
;; 	   expr2
;; 	   ...
;; 	   exprN)
;; All subexpressions must fit within the space allotted minus
;; the space for surrounding parentheses, if any.
;;
;; The algorithm may determine that the given expression cannot fit
;; within the allotted space.
;; In this case, the algorithm should raise an appropriate error,
;; using the function error.

(require "extras.rkt")
(require rackunit)
(check-location "08" "pretty.rkt")

(provide
 expr-to-strings
 make-sum-exp
 sum-exp-exprs
 make-diff-exp
 diff-exp-exprs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS:                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct sum-exp (exprs))
;; Interpretation:
;; A Sum-exp is a (make-sum-exp NELOExpr)
;; A sum-exp represents a sum
;; where exprs is a list of non-empty expressions

;; Template:
;; sum-exp-fn : Sum-exp -> ??
;; (define (sum-exp-fn e)
;;   (... (neloe-fn (sum-exp-exprs e))))

(define-struct diff-exp (exprs))
;; Interpretation:
;; A Diff-exp is a (make-diff-exp NELOExpr)
;; A diff-exp represents a difference calculation. 
;; where exprs is a list of non-empty expressions

;; Template:
;; diff-exp-fn : Diff-exp -> ??
;; (define (diff-exp-fn e)
;;   (... (neloe-fn (diff-exp-exprs e))))

;; An Expr is one of
;; -- Integer
;; -- (make-sum-exp NELOExpr)
;; -- (make-diff-exp NELOExpr)

;; A NELOExpr is a non-empty LOExpr.
;; A NELOExpr is one of: 
;; – (cons Expr '())
;; – (cons Expr NELOExpr)

;; Template:
;; neloe-fn : NELOExpr -> ??
;; (define (neloe-fn neloe)
;;   (cond
;;     [(empty? neloe) ...]
;;     [else (...
;;             (... (first neloe))
;;             (neloe-fn (rest neloe)))]))

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

(define-struct line (sec-numbers str))

;; Interpretation:
;; A Line is a (make-line ListOfPosInt String)
;; where
;; sec-numbers is the list of positve integers represent the order of sections
;; str is is the header text of the section

;; Template:
;; line-fn : Line -> ??
;; (define (line-fn l)
;;   (... (lop-fn (line-sec-numbers l))
;;        (line-str l)))

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

;; ListOfSecNumber
;; A ListOfSecNumber (LOSN) is either
;; -- empty
;; -- (cons LOP ListOfSecNumber)

;; Template:
;; losn-fn : LOSN -> ??
;; (define (losn-fn losn)
;;   (cond
;;     [(empty? losn) ...]
;;     [else (...
;;             (lop-fn (first losn))
;;             (losn-fn (rest losn)))]))

;; ListOfString
;; A ListOfString (LOStr) is either
;; -- empty
;; -- (cons PosInt LOStr)

;; Template:
;; lostr-fn : LOStr -> ??
;; (define (lostr-fn lostr)
;;   (cond
;;     [(empty? lostr) ...]
;;     [else (...
;;             (... (first lostr))
;;             (lostr-fn (rest lostr)))]))

;; General-Recursion Template for fib:
#|
;; General Recursion Template for decode:
(define (solution the-problem)
  (cond
    [(trivial1? the-problem) (trivial-solution1 the-problem)]
    [(trivial2? the-problem) (trivial-solution2 the-problem)]
    [(difficult? the-problem)
     (local
       ((define new-problem1 (simpler-instance1 the-problem))
        (define new-problem2 (simpler-instance2 the-problem)))
       (combine-solutions
        (solve new-problem1)
        (solve new-problem2)))]))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF DATA DEFINITIONS                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define hw-example-1
  (make-sum-exp (list 22 333 44)))

(define hw-example-2
  (make-sum-exp
   (list
    (make-diff-exp (list 22 3333 44))
    (make-diff-exp
     (list
      (make-sum-exp (list 66 67 68))
      (make-diff-exp (list 42 43))))
    (make-diff-exp (list 77 88
                         (make-diff-exp (list 42 43)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF EXAMPLES                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expr-to-strings                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expr-to-strings : Expr NonNegInt -> ListOfString
;; GIVEN: An expression and a width
;; RETURNS: A representation of the expression as a sequence of lines,
;;          with each line represented
;;          as a string of length not greater than the width.
;; EXAMPLE: (expr-to-strings hw-example-1 15)
;;       -> (list "(+ 22 333 44)")
;; STRATEGY: Call more general functions
(define (expr-to-strings expr width)
 (lines-to-strs
  (lines-to-width (list 1)
                (new-lines
                 (section-to-lines
                  (expr-to-section expr)))
                width 1 empty)))

;===============================================================================

;; expr-to-section : Expr -> Section
;; GIVEN: An expression
;; RETURNS: A Section representing the given expression
;; EXAMPLE: (expr-to-section (make-sum-exp (list 22 333 44)))
;;          -> sec-example-1
;; STRATEGY: Divide into cases on expr
(define (expr-to-section expr)
 (cond
    [(integer? expr)
     (make-section (number->string expr) empty)]
    [(sum-exp? expr)
     (make-section "(+" (exprs-to-secs (sum-exp-exprs expr)))]
    [(diff-exp? expr)
     (make-section "(-" (exprs-to-secs (diff-exp-exprs expr)))]))

;; exprs-to-secs : ListOfExprs -> ListOfSections
;; GIVEN: A list of Expressions 
;; RETURNS: A list of Sections representing the given list of expressions
;; EXAMPLE: (exprs-to-secs (list 22 333 44))
;;          -> (list
;;              (make-section "22" empty)
;;              (make-section "333" empty)
;;              (make-section "44" empty)
;;              (make-section ")" empty))
;; STRATEGY: Use Template for ListOfExpr on loe                    
(define (exprs-to-secs loe)
  (cond
     [(empty? loe) (list (make-section ")" empty))]
     [else (cons (expr-to-section (first loe))
                 (exprs-to-secs (rest loe)))]))

;===============================================================================
;; TEST:
(define sec-example-1
  (make-section "(+" (list
                      (make-section "22" empty)
                      (make-section "333" empty)
                      (make-section "44" empty)
                      (make-section ")" empty))))
(begin-for-test
  (check-equal?
   (expr-to-section hw-example-1)
   sec-example-1
   "(expr-to-section hw-example-1)"))
;===============================================================================

;; section-to-lines : Section -> ListOfLines
;; GIVEN: A section 
;; RETURNS: A list of Lines representing the given Section
;; WHERE: the section number started with (list 1)
;; EXAMPLES: See test below
;; STRATEGY: Call a more general function
(define (section-to-lines s)
  (section-to-line s (list 1)))

;; section-to-line : Section ListOfPosInt -> Line
;; GIVEN: a section and a list of section numbers lop
;; WHERE: lop is the given section's section numbers
;; RETURNS: a list of lines representing the given section and its sub-sections
;; STRATEGY: Use Template for Section on s
(define (section-to-line s lop)
  (cons
   (make-line lop (section-str s))
   (secs-to-lines (section-secs s) (add-sub-sec-to-lop lop))))

;; secs-to-lines : ListOfSections ListOfPosInt -> ListOfLines
;; GIVEN: a list of sections and a list of section numbers for the first section
;; RETURNS: A list of Lines representing the given list of sections
;; STRATEGY: Use Template for ListOfSection on los
(define (secs-to-lines los lop)                                           
  (cond
     [(empty? los) empty]
     [else (append
             (section-to-line (first los) lop)
             (secs-to-lines (rest los) (add1-to-last-lop lop)))]))

;; 2 HELPER FUNCTIONS:

;; add-sub-sec-to-lop : ListOfPosInt -> ListOfPosInt
;; GIVEN: a list of section numbers lop
;; WHERE: lop is the root section's section numbers
;; RETURNS: a new list of lop with a new section number 1 added
;;          to the end of lop
;; EXAMPLES: (add-sub-sec-to-lop (list 1))
;;           -> (list 1 1)
;; STRATEGY: Use Template for ListOfPosInt on lop
(define (add-sub-sec-to-lop lop)
  (foldr
   ;; PosInt List -> List
   ;; GIVEN: an positive interger and a list
   ;; RETURNS: a list with all the integers in the list
   (lambda (p lst) (cons p lst))
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

;===============================================================================
;; TEST:
(begin-for-test
  (check-equal?
   (section-to-lines sec-example-1)
   (list
    (make-line (list 1) "(+")
    (make-line (list 1 1) "22")
    (make-line (list 1 2) "333")
    (make-line (list 1 3) "44")
    (make-line (list 1 4) ")"))
   "(section-to-lines hw-example-1)"))
;===============================================================================

;; new-lines : ListOfLines -> ListOfLines
;; GIVEN: A list of lines 
;; RETURNS: A new list of lines representing the given list of lines
;;          excepte all the right parenthesis append to its closest expression
;;          and all the "(+" and "(-" has an expression appended to the right
;; STRATEGY: Divide into cases on lol
(define (new-lines lol)
  (local
    ((define number-of-p
       (count-right-parenthesis lol 0)))
  (cond
    [(empty? lol) empty]
    [(< 0 number-of-p)
     (append-parenthesis lol number-of-p)]
    [(integer? (string->number (line-str (first lol))))
     (cons (first lol) (new-lines (rest lol)))]
    [else (append-line (first lol) (rest lol))])))

;; append-parenthesis : ListOfLines NonNegInteger -> ListOfLines
;; GIVEN: A list of lines and the number of
;;        the right parentheses it should append
;; RETURNS: A new list of lines with all
;;          the right parentheses append to its closest expression
;; STRATEGY: Use Template for ListOfLines on lol
(define (append-parenthesis lol number-of-p)
  (cons
   (make-line
    (line-sec-numbers (first lol))
    (string-append (line-str (first lol))
                   (make-string number-of-p #\))))
   (new-lines (nth-rest-lines (rest lol) number-of-p))))

;; append-line : Line ListOfLines -> ListOfLines
;; GIVEN: a line and a list of lines
;; RETURNS: a list of lines with every "(+" and "(-" has an expression appended
;;          if (first lol) is an integer, append the interger to the given line
;;          and then start a new line
;;          else it is a "(+" or "(-" still need other expression to append
;; STRATEGY: Divide into cases on (first lol)
(define (append-line l lol)
  (cond
    [(integer? (string->number (line-str (first lol))))
     (cons (append-l-to-l l (first lol))
           (new-lines (rest lol)))]
    [else (append-line (append-l-to-l l (first lol))
                       (rest lol))]))

;; append-l-to-l : Line Line -> Line
;; GIVEN: two lines
;; RETURNS: a new line combining the two given lines
;; STRATEGY: Use Template for Line on first-l and second-l
(define (append-l-to-l first-l second-l)
   (make-line
    (line-sec-numbers first-l)
    (string-append (line-str first-l) " " (line-str second-l))))

;; HELPER FUNCIONTS:

;; count-right-parenthesis : ListOfLines NonNegInt -> 
;; GIVEN: a list of lines and the number of the right parentheses
;; WHERE: n represent the number of the right parentheses
;;        found in the list sofar
;; RETURNS: the number of the right parentheses in the list
;; STRATEGY: Use Template for ListOfLines on lol
(define (count-right-parenthesis lol n)
  (cond
    [(empty? lol) n]
    [(empty? (rest lol)) n]
    [else (if (string=? ")" (line-str (second lol)))
              (count-right-parenthesis (rest lol) (+ 1 n))
              n)]))

;; nth-rest-lines : ListOfLines NonNegInt -> ListOfLines
;; GIVEN: a list of lines 
;; RETURNS: a list with the first n elements being removed
;; STRATEGY: divide into cases on n
(define (nth-rest-lines lol n)
  (if (> n 0)
      (nth-rest-lines (rest lol) (- n 1))
      lol))

;;==============================================================================

;; lines-to-width : ListOfPosInt ListOfLine NonNegInt PosInt ListOfSecNumber
;;                -> ListOfLine
;; GIVEN: a line's section number where the line is wilder than the given width
;;        and a list of lines, the maximum width,
;;        the depth of the list that is going to render to shorter
;;        a list of section numbers where these lines should not be rendered
;; RETURNS: a list of lines with all line fit into the given width
;;          send an error if it fail to render to the given width
;; STRATEGY: Recur on max-d, subsecs, and shorter-lol
;;           then use render-wilder-l
;;           Halt when d > max-d
(define (lines-to-width sec-n lol width d losn)
  (local
    ((define max-d (max-depth lol))
     (define subsecs (remove-subsecs (sub-secs sec-n lol) losn))
     (define shorter-lol (render-to-shorter-lines subsecs d)))
  (cond
    [(> d max-d) (error "not enough room")]
    [(<= (max-width shorter-lol) width)
     shorter-lol]
    [else (render-wilder-l shorter-lol lol width d)])))

;; render-wilder-l : ListOfLine ListOfLine NonNegInt PosInt -> ListOfLine
;; GIVEN: a list of lines which is shinked 
;;        and a list of lines which is not modified
;;        the given width
;;        the depth of the list that is going to be dealed with
;; RETURNS: a list of lines with all lines wilder than the given width
;;          been filtered and shinked to fit into the width
;;          and all other lines not modified
;; STRATEGY: Use Template for ListOfLines on lol
(define (render-wilder-l shorter-lol lol width d)
  (cond
    [(empty? shorter-lol) empty]
    [else (if (> (line-width (first shorter-lol)) width)
               (append (lines-to-width
                        (line-sec-numbers (first shorter-lol))
                        lol width (+ d 1)
                        (list-of-sec-numbers (rest shorter-lol)))
                       (render-wilder-l (rest shorter-lol) lol width d))
               (cons (first shorter-lol)
                     (render-wilder-l (rest shorter-lol) lol width d)))]))

;; render-to-shorter-lines : ListOfLine PosInt -> ListOfLine
;; GIVEN: a list of lines and a depth
;;        that lines in that depth should be dealed with
;; RETURNS: a list of lines with all lines in the given depth
;;          render to shorter lines
;; STRATEGY: Use Template for ListOfLines on lol
(define (render-to-shorter-lines lol d)
  (cond
    [(empty? (rest lol)) (list (first lol))]
    [(< d (length (line-sec-numbers (second lol))))
     (render-to-shorter-lines
      (cons (append-l-to-l (first lol) (second lol))
            (rest (rest lol))) d)]
    [else (cons (first lol) (render-to-shorter-lines (rest lol) d))]))

;===============================================================================

;; lines-to-strs : ListOfLine -> ListOfString
;; GIVEN: a list of lines 
;; RETURNS: a list of strings representing the list of lines
;; STRATEGY: Use HOF map on lol
(define (lines-to-strs lol)
  (map
   ;; Line -> String
   ;; GIVEN: a line
   ;; RETURNS: a string representing the given line
   (lambda (l) (line-to-str l))
   lol))

;; line-to-str : Line -> String
;; GIVEN: a line
;; RETURNS: a string representing the given line
;; EXAMPLE: (line-to-str (make-line (list 1 3 2) "22"))
;;          -> "      22"
;; STRATEGY: Combine Simpler Functions
(define (line-to-str l)
  (local
    ((define number-of-space (* (- (length (line-sec-numbers l)) 1) 3)))
  (string-append (replicate number-of-space " ") (line-str l))))

;===============================================================================

;; HELPER FUNCTIONS:

;; max-depth : ListOfLines -> PosInt         
;; GIVEN: a list of lines
;; RETURNS: the max depth of the given line
;; EXAMPLE: (max-depth (list (make-line (list 1 3 2) "22"))) -> 3
;; STRATEGY: Use HOF foldr on lol
(define (max-depth lol)
  (foldr
   ;;Line PosInt -> PosInt
   ;; GIVEN: a line and the max depth so far
   ;; RETURNS: a max depth right now
   (lambda (l max-d) (max max-d (length (line-sec-numbers l))))
   1
   lol))

;; max-width : ListOfLines -> NonNegInt
;; GIVEN: a list of lines 
;; RETURNS: the max string-length among the given list of lines
;; STRATEGY: Use HOF foldr on lol
(define (max-width lol)
  (foldr
   ;; Line NonNegInt -> NonNegInt
   ;; GIVEN: a line and a max width of the given list of lines sofar 
   ;; RETURNS: a max width right now
   (lambda (l max-w) (max max-w (line-width l)))
   0
   lol))

;; line-width : Line -> PosInt
;; GIVEN: a line
;; RETURNS: the string length of the given line
;; EXAMPLE: (line-width (make-line (list 1 3 2) "22")) -> 8
;; STRATEGY: Combine Simpler Function
(define (line-width l)
  (+ (string-length (line-str l))
     (* (- (length (line-sec-numbers l)) 1) 3)))

;; list-of-sec-numbers : ListOfLines -> ListOfSecNumber                      
;; GIVEN: a list of lines
;; RETURNS: a list of every line's section numbers 
;; EXAMPLE: (list-of-sec-numbers (list (make-line (list 1 3 2) "22")))
;;          -> (list (list 1 3 2))
;; STRATEGY: Use HOF foldr on lol
(define (list-of-sec-numbers lol)
  (foldr
   ;; Line ListOfSecNumbers -> ListOfSecNumbers
   ;; GIVEN: a line and a list of section numbers sofar
   ;; RETURNS: a list of section numbers 
   (lambda (l los) (cons (line-sec-numbers l) los))
   empty
   lol))

;; sub-secs : ListOfPosInt ListOfLines -> ListOfLines
;; GIVEN: a section number and a list of lines
;; RETURNS: all the lines which is the subsection of the given section number
;; EXAMPLE: 
;; STRATEGY: Use HOF filter on lol
(define (sub-secs sec-n lol)
  (filter
   ;; Line -> Boolean
   ;; GIVEN: a line
   ;; RETURNS: true iff the given line's sec-number is a sub section of sec-n
   (lambda (l) (sub-sec? sec-n (line-sec-numbers l)))
   lol))

;; one-of-subsec? : ListOfSecNumber ListOfPosInt -> Boolean                          
;; GIVEN: a list of section numbers and a section number lop       
;; RETURNS: true iff lop is a sub-section of
;;          one of sec-numbers in the given list
;; STRATEGY: Use HOF ormap on losn
(define (one-of-subsec? losn lop)
  (ormap
   ;; ListOfPosInt -> Boolean
   ;; GIVEN: a section number
   ;; RETURNS: true iff lop is a sub-section of the given sec-number
   (lambda (sn) (sub-sec? sn lop))
   losn))

;; sub-sec? : ListOfPosInt ListOfPosInt -> Boolean
;; GIVEN: two sec-numbers
;; RETURNS: true iff lop2 is a sub-section of lop1
;; EXAMPLE: (sub-sec? (list 1) (list 1 2)) -> true
;; STRATEGY: Use Template for ListOfPosInt on lop1 and lop2
(define (sub-sec? lop1 lop2)
  (cond
    [(empty? lop1) true]
    [(empty? lop2) false]
    [else (and
           (= (first lop1) (first lop2))
           (sub-sec? (rest lop1) (rest lop2)))]))

;; remove-subsecs : ListOfSecNumber ListOfSecNumber -> ListOfSecNumber      
;; GIVEN: two list of section numbers losn1 and losn2
;; RETURNS: a list of section numbers losn2 with all the member is not
;;          one of a subsection in list losn1
;; STRATEGY: Use HOF ormap on losn1                                                   
(define (remove-subsecs losn1 losn2)
  (filter
   ;; ListOfPosInt -> Boolean
   ;; GIVEN: a section number
   ;; RETURNS:true iff the sec-number if not one of subsection in losn2
   (lambda (sn) (not (one-of-subsec? losn2 (line-sec-numbers sn))))
   losn1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS:                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (check-equal?
   (expr-to-strings hw-example-2 65)
   (list
    "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88 (- 42 43)))")
   "(expr-to-strings hw-example-2 65)")
  
  (check-equal?
   (expr-to-strings hw-example-2 29)
   (list
    "(+ (- 22 3333 44)"
    "   (- (+ 66 67 68) (- 42 43))"
    "   (- 77 88 (- 42 43)))")
   "(expr-to-strings hw-example-2 29)")
  
  (check-equal?
   (expr-to-strings hw-example-2 23)
   (list
    "(+ (- 22 3333 44)"
    "   (- (+ 66 67 68)"
    "      (- 42 43))"
    "   (- 77 88 (- 42 43)))")
   "(expr-to-strings hw-example-2 23)")
  
  (check-equal?
   (expr-to-strings hw-example-2 22)
   (list
    "(+ (- 22 3333 44)"
    "   (- (+ 66 67 68)"
    "      (- 42 43))"
    "   (- 77"
    "      88"
    "      (- 42 43)))")
   "(expr-to-strings hw-example-2 22)")
  
  (check-equal?
   (expr-to-strings hw-example-2 17)
   (list
    "(+ (- 22 3333 44)"
    "   (- (+ 66"
    "         67"
    "         68)"
    "      (- 42 43))"
    "   (- 77"
    "      88"
    "      (- 42 43)))")
   "(expr-to-strings hw-example-2 17)")
  
  (check-equal?
   (expr-to-strings hw-example-2 16)
   (list
    "(+ (- 22"
    "      3333"
    "      44)"
    "   (- (+ 66"
    "         67"
    "         68)"
    "      (- 42 43))"
    "   (- 77"
    "      88"
    "      (- 42"
    "         43)))")
   "(expr-to-strings hw-example-2 16)")
  
  (check-equal?
   (expr-to-strings hw-example-2 14)
   (list
    "(+ (- 22"
    "      3333"
    "      44)"
    "   (- (+ 66"
    "         67"
    "         68)"
    "      (- 42"
    "         43))"
    "   (- 77"
    "      88"
    "      (- 42"
    "         43)))")
   "(expr-to-strings hw-example-2 14)")
  (check-error
   (expr-to-strings hw-example-2 13)
   "not enough room"))
;===============================================================================
