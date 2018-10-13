;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Abstract and fold|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; a ListOfNumbers is one of:
; - empty
; - (cons Number ListOfNUmbers)

; Examples:
(define lon1 (list 1))
(define lon2 (list 4 9 2))

; process-lon : ListOfNumbers -> ...
(define (process-lon lon)
  (cond
    [(empty? lon) ...]
    [else (... (first lon) ...
               (process-lon (rest lon)) ...)]))


; add3 : ListOfNumbers -> ListOfNumbers
; add 3 to each number in the list
(define (add3 lon)
  (map.v2 plus3 lon)
  #;(cond
    [(empty? lon) empty]
    [else (cons (+ 3 (first lon))
                 (add3 (rest lon)))]))

(check-expect (add3 empty)empty)
(check-expect (add3 (list 1 2 3))
              (list 4 5 6))
(check-expect (add3 (list 3))
              (list 6))

; sub5 : ListOfNumbers -> ListOfNUmbers
; add 5 to each number
(define (sub5 lon)
  (map.v2 minus5 lon)
  #;(cond
    [(empty? lon) empty]
    [else (cons (- (first lon)5)
                 (sub5 (rest lon)))]))

(check-expect (sub5 empty) empty)
(check-expect (sub5 lon1) (list -4))
(check-expect (sub5 lon2) (list -1 4 -3))

; map.v2 : ...
; ...
(define (map.v2 op lon)
  (cond
    [(empty? lon) empty]
    [else (cons (op (first lon))
                 (map.v2 op (rest lon)))]))

; plus3 : Number -> Number
; adds 3 to input
(define (plus3 n)
  (+ n 3))

; minus5 : Number -> Number
; subtract 5 from input
(define (minus5 n)
  (- n 5))

; Design recipe for Abstarct
; 1. Design at least two similar functions
; 2. Identitfy inessential differences and revise fundtions to remobe them
; 3. Identify essential differences and pick inpit names for each of them (op)
; 4. Design the abstraction by replacing differences with input names
; 5. Redefine the original funtions using the abstractions

; add-all : ListOfNumbers -> Number
; adds all the numbers in the provided list
(define (add-all lon)
  (op-all + 0 lon)

  #;(cond
    [(empty? lon) 0]
    [else (+ (first lon)
             (add-all (rest lon)))]))

(check-expect (add-all empty) 0)
(check-expect (add-all lon1) 1)
(check-expect (add-all lon2) 15)

; multiply-all : ListOfNumbers -> Number
; Multiplies all the numbers in the list
(define (multiply-all lon)
  (op-all * 1 lon)
  #;(cond
    [(empty? lon) 1]
    [else (* (first lon)
             (multiply-all (rest lon)))]))

(check-expect (multiply-all empty) 1)
(check-expect (multiply-all lon1) 1)
(check-expect (multiply-all lon2) 72)

; op-all : ...
; ...
(define (op-all op base lon)
  (cond
    [(empty? lon) base]
    [else (op (first lon)
              (op-all op base (rest lon)))]))

; filter-numbers : ListOfNumbers -> ListOfNumbers
; Returns a ListOfNumbers between 20 and 50
(define (filter-numbers lon)
  (cond
    [(empty? lon) empty]
    [else
     (if (and (<= (first lon) 50) (>= (first lon) 20))
         (cons (first lon) (filter-numbers (rest lon)))
         (filter-numbers (rest lon)))]))

(check-expect (filter-numbers (list 1 11 25 46 52)) (list 25 46))
(check-expect (filter-numbers (list 1 2 51 20 50)) (list 20 50))
(check-expect (filter-numbers empty) empty)

  