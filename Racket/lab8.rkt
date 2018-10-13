;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; a ListOfNumbers is one of:
; - empty
; - (cons Number ListOfNumbers)

;Examples
(define lon1 (list 1 2 3))
(define lon2 (list 100 50 20))
(define lon3 (list 30 16 50))

; triple: ListOfNumbers -> ListOfNumbers
; triples every element in the list
(define (triple lon)
  (abs1 * 3 lon))

(check-expect (triple empty) empty)
(check-expect (triple lon1)
              (list 3 6 9))
(check-expect (triple lon2)
              (list 300 150 60))

; halve: ListOfNumbers -> ListOfNumbers
; halves every element in the list
(define (halve lon)
  (abs1 / 2 lon))

(check-expect (halve empty) empty)
(check-expect (halve lon2)
              (list 50 25 10))
(check-expect (halve lon3)
              (list 15 8 25))

; abs1: op Number ListOfNumbers -> ListOfNumbers
(define (abs1 op n lo)
  (cond
    [(empty? lo) empty]
    [else (cons (op (first lo) n)
                (abs1 op n (rest lo)))]))

(define init-speed 1)
(define init-angle 45)

; abs2 [Number -> Number] -> Number
(define (abs2 op)
  (* init-speed (op init-angle)))

(define init-x-vel (abs2 cos))
(define init-y-vel (abs2 sin))

; longer: String String -> String
; returns the longer string out of the 2
(define (longer s1 s2)
  (abs3 string-length s1 s2))

(check-expect (longer "hi" "hello")
              "hello")
(check-expect (longer "hi" "to")
              "hi")
(check-expect (longer "hello" "hell")
              "hello")

; dist : Posn -> Number
; computes the distance of the POsn from origin
(define (dist p1)
  (sqrt (+ (sqr (posn-x p1))
           (sqr (posn-y p1)))))
(check-expect (dist (make-posn 30 40)) 50)
(check-expect (dist (make-posn 5 12)) 13)

; closer? : Posn Posn -> Posn
; Gives out the posn that is closer to the origin
(define (closer? p1 p2)
  (abs3 dist p1 p2))

(check-expect (closer? (make-posn 0 0)
                       (make-posn 1 2))
              (make-posn 1 2))
(check-expect (closer? (make-posn 3 4)
                       (make-posn 4 7))
              (make-posn 4 7))

; a ListOfImages is one of:
; - empty
; - (cons Image ListOfImages)

; Examples
(define loi1 (list (empty-scene 20 20)))
(define loi2 (list (empty-scene 10 20)
                   (empty-scene 20 10)
                   (empty-scene 20 20)))
(define loi3 (list (empty-scene 10 20)))

; longer-list? : ListOfImages ListOfImages -> ListOfImages
; gives out the longer list
(define (longer-list? loi1 loi2)
  (abs3 length loi1 loi2))

(check-expect (longer-list? loi1 loi2) loi2)
(check-expect (longer-list? loi1 loi3) loi1)
(check-expect (longer-list? loi2 loi1) loi2)

; A Address is (make-address String String String Number)
(define-struct address(street apartment city zip))

(define my-address
  (make-address "1900 E 10th Street" "Eigenmenn Hall 745" "Bloomington" 47408))
(define your-address
  (make-address "700 N Woodlawn Ave" "Room 0121" "Bloomington" 47408))
(define his-address
  (make-address "1600 Pennsylvania Ave" "The White House" "Washington" 20500))

; smaller-zip: Address Address -> Address
; Takes in 2 addresses and sees which one has a smaller zip and prints that address
(define (smaller-zip a1 a2)
  (abs3 address-zip a1 a2))

(check-expect (smaller-zip my-address your-address) my-address)
(check-expect (smaller-zip my-address his-address) my-address)
(check-expect (smaller-zip his-address your-address) your-address)

; abs3 : op x x -> x
(define (abs3 op x y)
  (cond
    [(>= (op x) (op y)) x]
    [else y]))

;-------------------------------------------------------------------------------------------------

; A Mobile is one of:
; - (make-leaf Number)
; - (make-rod Mobile Number Number Mobile)
(define-struct leaf [weight])
(define-struct rod [lm ld rd rm])

;Eamples
(define m1 (make-leaf 3))
(define m2 (make-rod (make-leaf 10) 10 20 (make-leaf 5)))
(define m3 (make-rod (make-leaf 5) 2 20 (make-leaf 15)))

; weight : Mobile -> Number
; tells the weight of the mobile
(define (weight m)
  (cond
    [(leaf? m)(leaf-weight m)]
    [(rod? m)(+ (weight (rod-lm m)) (weight (rod-rm m)))]))

(check-expect (weight m1) 3)
(check-expect (weight m2) 15)
(check-expect (weight m3) 20)

; lighten : Mobile -> Mobile
; Halfs all the weights of the leafs
(define (lighten m)
  (cond
    [(leaf? m)(make-leaf (/ (leaf-weight m) 2))]
    [(rod? m) (make-rod (lighten (rod-lm m))
                        (rod-ld m)
                        (rod-rd m)
                        (lighten (rod-rm m)))]))

(check-expect (lighten m1)(make-leaf (/ (leaf-weight m1) 2)))
(check-expect (lighten m2)(make-rod (make-leaf 5)
                                    (rod-ld m2)
                                    (rod-rd m2)
                                    (make-leaf 2.5)))
(check-expect (lighten m3)(make-rod (make-leaf 2.5)
                                    (rod-ld m3)
                                    (rod-rd m3)
                                    (make-leaf 7.5)))


; count : ListOfNumbers -> Number
; gives the number of element in the list
(define (count lon)
  (abs4 lon))

(check-expect (count (list 1 2 3 4)) 4)
(check-expect (count empty) 0)
(check-expect (count (list 3 40 2)) 3)

; count-s : ListOfString -> Number
; gives the number of element in the list
(define (count-s los)
  (abs4 los))

(check-expect (count-s (list "hi" "Hello")) 2)
(check-expect (count-s empty) 0)
(check-expect (count-s (list "hi")) 1)

; abs4: [ListOfX] -> Number
(define (abs4 lox)
  (cond
    [(empty? lox) 0]
    [else (+ 1 (count (rest lox)))]))

;

