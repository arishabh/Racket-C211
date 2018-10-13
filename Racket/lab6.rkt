;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lab6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

; a NaturalNumber is one of:
; - 0
; - (n + 1)

; process-nn : NaturalNumber -> ...
; ...
(define (process-nn n)
  (cond
    [(= n 0) ... ]
    [else (... (process-nn (sub1 n)) ...)]))

; a ListOfStrings is one of:
; - empty
; - (cons String ListOfStrings)

(define los1 empty)
(define los2 (list "Hello" "World"))
(define los3 (list "Hello," "how" "are" "you?"))
(define file-names (list "thefly.txt" "thegerm.txt" "theoctopus.txt"
                         "theostrich.txt" "thetermite.txt"))

(define thefly(read-words "thefly.txt"))
(define thegerm(read-words "thegerm.txt"))
(define theoctopus(read-words "theoctopus.txt"))
(define theostrich(read-words "theostrich.txt"))
(define thetermite(read-words "thetermite.txt"))

; process-los : ListOfString -> ...
; ...
(define (process-los los)
  (cond
    [(empty? los) ...]
    [else ( ... (first los) ...
            (process-los (rest los)...))]))    

; has-word? : ListOfString String -> Boolean
; Checks if the list has that word given
(define (has-word? los word)
  (cond
    [(empty? los) false]
    [else (or (string=? word (first los))
              (has-word? (rest los) word))]))

(check-expect (has-word? los1 "Hello")false)
(check-expect (has-word? los2 "Hello")true)
(check-expect (has-word? los3 "Hello,")true)
(check-expect (has-word? los3 "World")false)


; file-has-word?: String String -> Boolean
; Checks if a word is there is a file
(define (file-has-word? file word)
  (cond
    [(empty? (read-words file)) false]
    [else (or (string=? word (first (read-words file)))
              (has-word? (rest (read-words file)) word))]))

(check-expect (file-has-word? "thetermite.txt" "termite")true)
(check-expect (file-has-word? "thegerm.txt" "the")true)
(check-expect (file-has-word? "theoctopus.txt" "The")true)
(check-expect (file-has-word? "thefly.txt" "the")true)
(check-expect (file-has-word? "theostrich.txt" "ostrich")true)
(check-expect (file-has-word? "thetermite.txt" "iwhdiwhdhw")false)
(check-expect (file-has-word? "thegerm.txt" "iwhdiwhdhw")false)
(check-expect (file-has-word? "theoctopus.txt" "iwhdiwhdhw")false)
(check-expect (file-has-word? "thefly.txt" "iwhdiwhdhw")false)
(check-expect (file-has-word? "theostrich.txt" "iwhdiwhdhw")false)

; search-files : ListOfStrings String -> ListOfStrings
; Takes in a list of files and searches the word in it, and gives out the files that have the word
(define (search-files lof word)
  (cond
    [(empty? lof)empty]
    [else (cond
            [(file-has-word? (first lof) word)
             (cons (first lof) (search-files (rest lof) word))]
            [else (search-files (rest lof) word)])]))

;Exercise 6 : (list "theostrich.txt" "thetermite.txt")

(define background (rectangle 300 300 "solid" "black"))
(define (make-ripple x)
  (circle x "outline" "white"))

; a RippleWorld is a (make-rippleworld Number Number Number)
(define-struct rippleworld(radius x y))
(define rw1 (make-rippleworld 5
                               50 50))
(define rw2 (make-rippleworld 10
                              10 10))
(define rw3 (make-rippleworld 2
                               100 10))

;draw-ripple : Rippleworld -> Image
(define (draw-ripple rw)
  (place-image (circle (rippleworld-radius rw) "outline" "white")
               (rippleworld-x rw)
               (rippleworld-y rw)
               background))


;make-circle : RippleWorld x y MouseEvent -> RippleWorld
(define (make-circle rw x y me)
  (cond
    [(string=? me "button-down")
     (make-rippleworld 1
                       x
                       y)]
    [else rw]))

;increase-rad : RippleWorld -> RippleWorld
(define (increase-rad rw)
  (make-rippleworld (add1 (rippleworld-radius rw))
                    (rippleworld-x rw)
                    (rippleworld-y rw)))
; a World is a rippleworld
(big-bang (make-rippleworld 0 0 0)
  (to-draw draw-ripple)
  (on-mouse make-circle)
  (on-tick increase-rad))



 