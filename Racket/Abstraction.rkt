;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 20181010abstraction_rose) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; a ListOfNumbers is [ListOf Number]

; a ListOfStrings is [ListOf String]

; a [ListOf X] is one of:
; - empty
; - (cons X [ListOf X])

; filter-20-50 : ListOfNumbers -> ListOfNumbers
; filter out numbers not in the range 20-50
(define (filter-20-50 lon)
  (filter.v2 between-20-and-50 lon)
  #;(cond
    [(empty? lon) empty]
    [(between-20-and-50 (first lon))
     (cons (first lon) (filter-20-50 (rest lon)))]
    [else (filter-20-50 (rest lon))]))

(check-expect (filter-20-50 empty) empty)
(check-expect (filter-20-50 (list 10 20 30 40 50 60))
              (list 20 30 40 50))

; between-20-and-50 : Number -> Boolean
(define (between-20-and-50 n)
  (<= 20 n 50))

; filter-lorem : ListOfStrings -> ListOfStrings
; filters out all strings not containing "lorem"
(define (filter-lorem los)
  (filter.v2 lorem? los)
  
  #;(cond
    [(empty? los) empty]
    [(lorem? (first los))
     (cons (first los) (filter-lorem (rest los)))]
    [else (filter-lorem (rest los))]))

; lorem? : String -> Boolean
(define (lorem? s)
  (string-contains? "lorem" s))

(check-expect (filter-lorem empty) empty)
(check-expect (filter-lorem (list "" "c211" "clorem211" "lorem"))
              (list "clorem211" "lorem"))

; filter.v2 : [X -> Boolean] [ListOf X] -> [ListOf X]
; removes elements from input list that don't satisfy the predicate
(define (filter.v2 pred lox)
  (cond
    [(empty? lox) empty]
    [(pred (first lox))
     (cons (first lox) (filter.v2 pred (rest lox)))]
    [else (filter.v2 pred (rest lox))]))

; a MaybePosn is a [Maybe Posn]

; a MaybeNumber is a [Maybe Number]

; a [Maybe X] is one of:
; - (make-none)
; - X

; draw-lop : Image [ListOf Posn] -> Image
; depicts each posn with the given image
(define (draw-lop img lop)
  (local [; place : Posn Image -> Image
          (define (place p background)
            (place-image img
                         (posn-x p) (posn-y p)
                         background))]
    (fold.v2 place (empty-scene 300 300) lop)
    #;(cond
      [(empty? lop) (empty-scene 300 300)]
      [else (place (first lop) (draw-lop img (rest lop)))]))) 
  
  
; mult-all : [ListOf Number] -> Number
; multiplies all of the elements of the list together
(define (mult-all lon)
  (fold.v2 mult 1 lon)
  #;(cond
    [(empty? lon) 1]
    [else (mult (first lon) (mult-all (rest lon)))]))

; mult : Number Number -> Number
(define (mult m n)
  (* m n))

(check-expect (mult-all empty) 1)
(check-expect (mult-all (list 1 2 3)) 6)


(check-expect (draw-lop (circle 5 "solid" "blue") empty) (empty-scene 300 300))
(check-expect (draw-lop (star 5 "solid" "blue") (list (make-posn 100 100) (make-posn 20 50)))
              (place-image (star 5 "solid" "blue")
                           100 100
                           (place-image (star 5 "solid" "blue")
                                        20 50
                                        (empty-scene 300 300))))

; fold.v2 : [X Y -> Y] Y [ListOf X] -> Y
(define (fold.v2 op y lox)
  (cond
    [(empty? lox) y]
    [else (op (first lox) (fold.v2 op y (rest lox)))]))

