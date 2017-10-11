;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Implementing tiny text editor
;; editor.rkt

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide
 make-editor
 editor-pre
 editor-post
 editor?
 edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS :
(define FIRST-INDEX 0)
(define SECOND-INDEX 1)
(define ONE 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

(define-struct editor(pre post))

;; An Editor is a
;;    (make-editor String String).
;; It represents a text editor.
;; INTERPRETATION:
;;    pre field of the editor denotes text to the left of the cursor 
;;    post field of the editor denotes text to the right of the cursor

;; TEMPLATE :
#|(define (editor-fn ed)
  (...
   (editor-pre ed)
   (editor-post ed)
   (editor? ed)))|#

;; KeyEvent is defined in the 2htdp/universe module. A KeyEvent
;; represents key board events. Every KeyEvent is a string, but
;; not all strings are key events. The predicate for comparing
;; two KeyEvent is key=? .

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; edit : Editor KeyEvent -> Editor
;; PURPOSE :
;; GIVEN : an editor ed and a key event ke.
;; RETURNS : an editor after performing the
;;           key event action to the pre field
;;           of ed.
;;           Ignoring the tab key ("\t") and
;;           the return key ("\r").
;;           Ignoring key events longer than
;;           one letter except "left" and "right".
;;
;; EXAMPLES :
;; (edit (make-editor "foo" "bar") "left")  -> (make-editor "fo" "obar")
;; (edit (make-editor "foo" "bar") "right") -> (make-editor "foob" "ar")
;; (edit (make-editor "foo" "bar") "\t")    -> (make-editor "foo" "bar")
;; (edit (make-editor "foo" "bar") "\r")    -> (make-editor "foo" "bar")
;; (edit (make-editor "foo" "bar") "\b")    -> (make-editor "fo" "bar")
;; (edit (make-editor "foo" "bar") "abc")   -> (make-editor "foo" "bar")
;; (edit (make-editor "foo" "bar") "a")     -> (make-editor "fooa" "bar")
;; (edit (make-editor "foo" "bar") "up")    -> (make-editor "foo" "bar")
;; (edit (make-editor "foo" "bar") " ")     -> (make-editor "foo " "bar")
;;
;; DESIGN STRATEGY : Cases on KeyEvent
(define (edit ed ke)
  (cond
    [(key=? ke "left") (editor-after-left ed)]
    [(key=? ke "right") (editor-after-right ed)]
    [(key=? ke "\t") ed]
    [(key=? ke "\r") ed]
    [(key=? ke "\b") (editor-after-backspace ed)]
    [(= (string-length ke) 1) (editor-after-add ed ke)]
    [else ed])) 

;; TEST :
(begin-for-test
  (check-equal?
   (edit (make-editor "foo" "bar") "left")
   (make-editor "fo" "obar")
   "The editor after moving the cursor one character to the left of pre
    field (foo) in editor ed should have pre field (fo) and post field
    (obar)")
  (check-equal?
   (edit (make-editor "foo" "bar") "right")
   (make-editor "foob" "ar")
   "The editor after moving the cursor one character to the right of pre
    field (foo) in editor ed should have pre field (foob) and post field
    (ar)")
  (check-equal?
   (edit (make-editor "foo" "bar") "\t")
   (make-editor "foo" "bar")
   "The editor after key event tab should be same as editor  ed  because
    tab is ignored and should have the same  pre field  (foo) and post
    field  (bar)")
  (check-equal?
   (edit (make-editor "foo" "bar") "\r")
   (make-editor "foo" "bar")
   "The editor after key event return should be same as editor  ed  because
    return is ignored and should have the same  pre field  (foo) and post
    field  (bar)")
  (check-equal?
   (edit (make-editor "foo" "bar") "\b")
   (make-editor "fo" "bar")
   "The editor after key event backspace should delete the character
    immediately to the left of the cursor and should have pre field (fo)
    and post field (bar)")
  (check-equal?
   (edit (make-editor "foo" "bar") "a")
   (make-editor "fooa" "bar")
   "The editor after key event (a) should add the character
    next to the pre field of editor ed and should have pre field (fooa)
    and post field (bar)")
  (check-equal?
   (edit (make-editor "foo" "bar") "up")
   (make-editor "foo" "bar")
   "The editor after key event up should be same as editor  ed  because
    up is ignored and should have the same  pre field  (foo) and post
    field  (bar)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editor-after-left : Editor -> Editor
;; GIVEN : an editor ed
;; RETURNS : another editor with the cursor
;;           moved one character to the left
;;           in editor ed.
;; EXAMPLES :
;; (editor-after-left (make-editor "abc" "def")) -> (make-editor "ab" "cdef")
;; (editor-after-left (make-editor "abc" ""))    -> (make-editor "ab" "c")
;; (editor-after-left (make-editor "" "abc"))    -> (make-editor "" "abc")
;; (editor-after-left (make-editor "a c" "d"))   -> (make-editor "a " "cd")
;; (editor-after-left (make-editor "a " "b"))    -> (make-editor "a" " b")

;; DESIGN STRATEGY : Use template for Editor on ed
(define (editor-after-left ed)
  (make-editor (string-remove-last (editor-pre ed))
               (string-append (string-last(editor-pre ed)) (editor-post ed))))

;; TEST :
(begin-for-test
  (check-equal?
   (editor-after-left (make-editor "abc" "def"))
   (make-editor "ab" "cdef")
   "The editor after shifting cursor to one character left of the pre
    field abc should have pre field ab and post field cdef")
  (check-equal?
   (editor-after-left (make-editor "abc" ""))
   (make-editor "ab" "c")
   "The editor after shifting cursor to one character left of the pre
    field abc should have pre field ab and post field c")
  (check-equal?
   (editor-after-left (make-editor "a " "b"))
   (make-editor "a" " b")
   "The editor after shifting cursor to one character left of the pre
    field (a ) should have pre field a and post field ( b)"))                           


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editor-after-right : Editor -> Editor
;; GIVEN : an editor ed
;; RETURNS : another editor with the cursor
;;           moved one character to the right
;;           in editor ed.
;; EXAMPLES :
;; (editor-after-right (make-editor "abc" "def")) -> (make-editor "abcd" "ef")
;; (editor-after-right (make-editor "abc" ""))    -> (make-editor "abc" "")
;; (editor-after-right (make-editor "" "a"))      -> (make-editor "a" "")

;; DESIGN STRATEGY : Use template for Editor on ed
(define (editor-after-right ed)
  (make-editor (string-append (editor-pre ed) (string-first(editor-post ed)))
               (string-remove-first (editor-post ed))))
;; TEST :
(begin-for-test
  (check-equal?
   (editor-after-right (make-editor "abc" "def"))
   (make-editor "abcd" "ef")
   "The editor after shifting cursor to one character right of the pre
    field abc should have pre field abcd and post field ef")
  (check-equal?
   (editor-after-right (make-editor "abc" ""))
   (make-editor "abc" "")
   "The editor after shifting cursor to one character right of the pre
    field abc should have pre field abc and post field empty")
  (check-equal?
   (editor-after-right (make-editor "" "a"))
   (make-editor "a" "")
   "The editor after shifting cursor to one character right of the empty
    pre field and post field (a) should have pre field (a) and post field
    empty"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editor-after-backspace : Editor -> Editor
;; GIVEN : an editor ed
;; RETURNS : another editor after deleting the
;;           character immediately to the left
;;           of the cursor(if there are any)
;;           in editor ed.
;; EXAMPLES :
;; (editor-after-backspace (make-editor "abc" ""))   -> (make-editor "ab" "")
;; (ediotr-efter-backspace (make-editor "abc" "def"))-> (make-editor "ab" "def")
;; (editor-after-backspace (make-editor "" "def"))   -> (make-editor "" "def")

;; DESIGN STRATEGY : Use template for Editor on ed
(define (editor-after-backspace ed)
  (make-editor (string-remove-last (editor-pre ed)) (editor-post ed)))

;; TEST :
(begin-for-test
  (check-equal?
   (editor-after-backspace (make-editor "abc" ""))
   (make-editor "ab" "")
   "The editor, having pre field abc and post field empty, after deleting
    the character c immediately to the left of the cursor should have pre
    field ab and post field empty")
  (check-equal?
   (editor-after-backspace (make-editor "abc" "def"))
   (make-editor "ab" "def")
   "The editor, having pre field abc and post field def, after deleting
    the character c immediately to the left of the cursor should have pre
    field ab and post field def")
  (check-equal?
   (editor-after-backspace (make-editor "" "def"))
   (make-editor "" "def")
   "The editor, having pre field empty and post field def, does not have
    any character immediately to the left of the cursor and after deleting
    the character immediately to the left of the cursor should have pre
    field empty and post field def"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editor-after-add : Editor KeyEvent -> Editor
;; GIVEN : an editor and a keyevent ke.
;; RETRUNS : another editor after adding a
;;           single-character KeyEvent ke
;;           to the end of the pre field
;;           of editor ed.
;; EXAMPLES :
;; (editor-after-add (make-editor "ab" "def") "c") -> (make-editor "abc" "def")
;; (editor-after-add (make-editor "" "ello") "h")  -> (make-editor "h" "ello")
;; (editor-after-add (make-editor "ye" "") "s")    -> (make-editor "yes" "")
;; (editor-after-add (make-editor "foo" "bar") " ")-> (make-editor "foo " "bar")

;; DESIGN STARTEGY : Use template for Editor on ed
(define (editor-after-add ed ke)
  (make-editor (string-append (editor-pre ed) ke) (editor-post ed)))

;; TEST :
(begin-for-test
  (check-equal?
   (editor-after-add (make-editor "ab" "def") "c")
   (make-editor "abc" "def")
   "The editor after adding the keyevent c to the end of the pre field ab
    of editor ed should have pre field abc")
  
  (check-equal?
   (editor-after-add (make-editor "" "ello") "h")
   (make-editor "h" "ello")
   "The editor after adding the keyevent h to the end of the empty pre
    field of editor ed should have pre field h")
  
  (check-equal?
   (editor-after-add (make-editor "ye" "") "s")
   (make-editor "yes" "")
   "The editor after adding the keyevent s to the end of the pre field ye
    of editor ed should have pre field yes")
  
  (check-equal?
   (editor-after-add (make-editor "foo" "bar") " ")
   (make-editor "foo " "bar")
   "The editor after adding the keyevent spacebar to the end of
    the pre field (foo) of editor ed should have pre field (foo )"))

;;
;; Wish-list
;;
;; string-remove-last : String -> String
;; GIVEN : a string
;; RETURNS : a string without the string at last position of input string
;; EXAMPLES :
;; (string-remove-last "hello") ->  "hell"
;; (string-remove-last "")      ->  ""
;; DESIGN STARTEGY : combine simpler functions
(define (string-remove-last str)
  (if (check-length? str)
      (substring str FIRST-INDEX (- (string-length str) ONE))
      str))
;; TEST:
(begin-for-test
  (check-equal?
   (string-remove-last "hello")
   "hell"
   "The result after remove last string from input hello should be hell")
  (check-equal?
   (string-remove-last "")
   ""
   "The result after remove last string from an empty string should be an
    empty string"))


;; string-remove-first : String -> String
;; GIVEN : a string
;; RETURNS : a string without the string
;;           at first position of input
;;           string.
;; EXAMPLES :
;; (string-remove-first "hello") ->  "ello"
;; (string-remove-frist "")      ->  ""
;; (string-remove-first "h")     ->  ""
;; DESIGN STARTEGY : combine simpler functions
(define (string-remove-first str)
  (if (check-length? str)
      (substring str SECOND-INDEX (string-length str))
      str))

;; TEST :
(begin-for-test
  (check-equal?
   (string-remove-first "hello")
   "ello"
   "The string after removing string at first position of hello should be
    ello")
  (check-equal?
   (string-remove-first "")
   ""
   "The string after removing string at first position of an empty string
    should be an empty string")
  (check-equal?
   (string-remove-first "h")
   ""
   "The string after removing string at first position of h should be an
    empty string"))

;; string-last : String -> String
;; GIVEN : a string
;; RETURNS : the string at last position of input string 
;; EXAMPLES :
;; (string-last "hello") -> "o"
;; (string-last "")      -> ""
;; (string-last "h")     -> "h"
;; DESIGN STRATEGY : combine simpler functions
(define (string-last str)
  (if (check-length? str)
      (substring str (- (string-length str) ONE))
      str))
;; TEST :
(begin-for-test
  (check-equal?
   (string-last "hello")
   "o"
   "The string at last position of hello should be o")
  (check-equal?
   (string-last "")
   ""
   "The string at last position of empty string should
    be an empty string")
  (check-equal?
   (string-last "h")
   "h"
   "The string at last position of 1string h should be
    h"))


;; string-first : String -> String
;; GIVEN : a string
;; RETURNS : the string at first position of input string 
;; EXAMPLES :
;; (string-first "hello") -> "h"
;; (string-first "")      -> ""
;; (string-first "h")     -> "h"
;; DESIGN STRATEGY : combine simpler functions
(define (string-first str)
  (if (check-length? str)
      (substring str FIRST-INDEX SECOND-INDEX)
      str))
;; TEST :
(begin-for-test
  (check-equal?
   (string-first "hello")
   "h"
   "The string at first position of input string hello should be h")
  (check-equal?
   (string-first "")
   ""
   "The string at first position of an empty string should be an empty
    string")
  (check-equal?
   (string-first "h")
   "h"
   "The string at first position of input string h should be h"))


;; check-length? : String -> Boolean
;; GIVEN : a string
;; RETURNS : true iff the length of the string greater than 0,
;;           otherwise false 
;; EXAMPLES :
;; (check-length? "hello") -> true
;; (check-length? "")      -> false
;; (check-length? "a")     -> true
;; DESIGN STARTEGY : cases on length of string
(define (check-length? str)
  (if (> (string-length str) 0)
      true
      false))
;; TEST :
(begin-for-test
  (check-equal?
   (check-length? "hello")
   true
   "The length of input string hello is greater than 0 so the result
    should be true")
  (check-equal?
   (check-length? "")
   false
   "The length of input string is 0 so the result should be false")
  (check-equal?
   (check-length? "a")
   true
   "The length of input string a is greater than 0 so the result
    should be true"))

