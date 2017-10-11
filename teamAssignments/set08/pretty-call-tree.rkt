;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty-call-tree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
An Expression:
(make-sum-exp
 (list
  (make-diff-exp (list 22 3333 44))
  (make-diff-exp
   (list
    (make-sum-exp (list 66 67 68))
    (make-diff-exp (list 42 43))))
  (make-diff-exp (list 77 88
                       (make-diff-exp (list 42 43))))))

-> (expr-to-section expr)-> A Section
(make-section
 "(+"
 (list
  (make-section
   "(-"
   (list
    (make-section "22" '())
    (make-section "3333" '())
    (make-section "44" '())
    (make-section ")" '())))
  (make-section
   "(-"
   (list
    (make-section
     "(+"
     (list
      (make-section "66" '())
      (make-section "67" '())
      (make-section "68" '())
      (make-section ")" '())))
    (make-section
     "(-"
     (list (make-section "42" '())
           (make-section "43" '())
           (make-section ")" '())))
    (make-section ")" '())))
  (make-section
   "(-"
   (list
    (make-section "77" '())
    (make-section "88" '())
    (make-section
     "(-"
     (list (make-section "42" '())
           (make-section "43" '())
           (make-section ")" '())))
    (make-section ")" '())))
  (make-section ")" '())))

-> (section-to-lines s) -> A ListOfLine
(list
 (make-line (list 1) "(+")
 (make-line (list 1 1) "(-")
 (make-line (list 1 1 1) "22")
 (make-line (list 1 1 2) "3333")
 (make-line (list 1 1 3) "44")
 (make-line (list 1 1 4) ")")
 (make-line (list 1 2) "(-")
 (make-line (list 1 2 1) "(+")
 (make-line (list 1 2 1 1) "66")
 (make-line (list 1 2 1 2) "67")
 (make-line (list 1 2 1 3) "68")
 (make-line (list 1 2 1 4) ")")
 (make-line (list 1 2 2) "(-")
 (make-line (list 1 2 2 1) "42")
 (make-line (list 1 2 2 2) "43")
 (make-line (list 1 2 2 3) ")")
 (make-line (list 1 2 3) ")")
 (make-line (list 1 3) "(-")
 (make-line (list 1 3 1) "77")
 (make-line (list 1 3 2) "88")
 (make-line (list 1 3 3) "(-")
 (make-line (list 1 3 3 1) "42")
 (make-line (list 1 3 3 2) "43")
 (make-line (list 1 3 3 3) ")")
 (make-line (list 1 3 4) ")")
 (make-line (list 1 4) ")"))

-> (new-line lol) -> A Legal ListOfLine
which deal with ")"
and append an expressions to the right of a "(+" or "(-"

(list
 (make-line (list 1) "(+ (- 22")
 (make-line (list 1 1 2) "3333")
 (make-line (list 1 1 3) "44)")
 (make-line (list 1 2) "(- (+ 66")
 (make-line (list 1 2 1 2) "67")
 (make-line (list 1 2 1 3) "68)")
 (make-line (list 1 2 2) "(- 42")
 (make-line (list 1 2 2 2) "43))")
 (make-line (list 1 3) "(- 77")
 (make-line (list 1 3 2) "88")
 (make-line (list 1 3 3) "(- 42")
 (make-line (list 1 3 3 2) "43)))"))

-> (lines-to-strs lol) -> ListOfString
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

ListOfLine -> (lol-to-width lol width) -> Well Indented ListOfLine 
(lines-to-strs (lol-to-width lol 65))->
(list "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88 (- 42 43)))")

(lines-to-strs (lol-to-width lol 29))->
(list "(+ (- 22 3333 44)"
      "   (- (+ 66 67 68) (- 42 43))"
      "   (- 77 88 (- 42 43)))")

(lines-to-strs (lol-to-width lol 23))->
(list
 "(+ (- 22 3333 44)"
 "   (- (+ 66 67 68)"
 "      (- 42 43))"
 "   (- 77 88 (- 42 43)))")

(lines-to-strs (lol-to-width lol 22))->
(list
 "(+ (- 22 3333 44)"
 "   (- (+ 66 67 68)"
 "      (- 42 43))"
 "   (- 77"
 "      88"
 "      (- 42 43)))")

(lines-to-strs (lol-to-width lol 17))->
(list
 "(+ (- 22 3333 44)"
 "   (- (+ 66"
 "         67"
 "         68)"
 "      (- 42 43))"
 "   (- 77"
 "      88"
 "      (- 42 43)))")

(lines-to-strs (lol-to-width lol 16))->
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

(lines-to-strs (lol-to-width lol 14))->
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

(lines-to-strs (lol-to-width lol 13))->
not enough room