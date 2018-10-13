;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Unions and recursion|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Wagon is one of:
;  - (make-passenger-wagon Company)
;  - (make-freight-wagon String Number)
 
; A Company is one of:
;  - "Alstom"
;  - "Bombardier"
 
(define-struct passenger-wagon (model))
(define-struct freight-wagon (destination axles))

(define wagon1 (make-passenger-wagon "Alstom"))
(define wagon2 (make-passenger-wagon "Bombardier"))
(define wagon3 (make-freight-wagon "Bloomington" 6))
(define wagon4 (make-freight-wagon "LA" 4))

; make-passenger-wagon: Company -> passenger-wagon
; passenger-wagon-model : passenger-wagon -> company
; passenger-wagon? : Anything -> Boolean

; make-freight-wagon: String Number -> freight-wagon
; freight-wagon-destination: freight-wagon -> string
; freight-wagon-axles: freight-wagon -> number
; freight-wagon? : Anything -> Boolean

; process-wagon : Wagon -> ...
; ...
(define (process-wagon w)
  (cond [(passenger-wagon? w)
         (process-company (passenger-wagon-model w))]
        [(freight-wagon? w)
         (...(freight-wagon-destination w) ... (freight-wagon-axles w) ...)]))

; process-company: Company -> ...
; ...
;(define (process-company c)
;  (cond [(string=? "Alstom" c)
;         ...]
;        [(string=? "Bombardier" c)
;         ...]))

; wagon-weight : Wagon -> Weight
; Gives out the weight of the wagon entered
(define (wagon-weight w)
  (cond [(passenger-wagon? w)
         (process-company (passenger-wagon-model w))]
        [(freight-wagon? w)
         (* 6 (freight-wagon-axles w))]))

; process-company: Company -> weight
; Checks which company of passenger wagon model it is and gives out the weight of it according
(define (process-company c)
  (cond [(string=? "Alstom" c)
         45]
        [(string=? "Bombardier" c)
         60]))

(check-expect (wagon-weight wagon1) 45)
(check-expect (wagon-weight wagon2) 60)
(check-expect (wagon-weight wagon3) 36)
(check-expect (wagon-weight wagon4) 24)

; A Shuttle is one of:
; - (make-shuttle1)
; - (make-shuttle2 Wagon)
; - (make-shuttle3 Wagon Wagon)

(define-struct no-wagon [])
(define-struct one-wagon [wagon11])
(define-struct two-wagon [wagon21 wagon22])

(define shuttle1 (make-no-wagon))
(define shuttle2 (make-one-wagon wagon3))
(define shuttle3 (make-two-wagon wagon2 wagon4))

; make-no-wagon : nothing -> no-wagon
; no-wagon? : anything -> boolean

; make-one-wagon : Wagon -> one-wagon
; one-wagon-wagon11 : one-wagon -> wagon
; one-wagon? : anything -> boolean

; make-two-wagon : Wagon Wagon -> two-wagon
; two-wagon-wagon21 : two-wagon -> Wagon
; two-wagon-wagon22 : two-wagon -> Wagon
; two-wagon? : anything -> Boolean

; Process-shuttle : shuttle -> ...
; ...
;(define (process-shuttle s)
;  (cond [(no-wagon? s)
;         (make-no-wagon)]
;        [(one-wagon? s)
;         (process-wagon (one-wagon-wagon11 s))]
;        [(two-wagon? s)
;         (process-wagon (two-wagon-wagon21 s) process-wagon(two-wagon-wagon21 s) ...)]))

; A TrainOfWagons is one of:
; - (make-no-tow)
; - (make-tow Wagon TrainOfWagons)

(define-struct no-tow[])
(define-struct one-tow [wagon prev-tow])

(define tow1 (make-no-tow))
(define tow2 (make-one-tow wagon2 (make-no-tow)))
(define tow3 (make-one-tow wagon3 (make-one-tow wagon1 (make-one-tow wagon4 (make-no-tow)))))

; make-no-tow: nothing -> no-tow
; no-tow? : anything -> boolean

; make-one-tow: wagon prev-tow -> one-tow
; one-tow-wagon: one-tow -> Wagon
; one-tow-prev-tow : one-tow -> TrainOfWagons
; one-tow? : anything -> boolean

; process-train-of-wagons : Trainofwagons -> ...
; ...
;(define (process-train-of-wagons t)
;  (cond [(no-tow? t)
;         (make-no-tow)]
;        [(one-tow? t)
;         (process-wagon (one-tow-wagon t)) ...
;          (process-train-of-wagons t) ... ]))

; train-weight : Trainofwagons -> String
; Takes in a train of wagons and gives out the weight of the entire train
(define (train-weight t)
  (cond [(no-tow? t)
         130]
        [(one-tow? t)
        (+ 130 (wagon-weight(one-tow-wagon t))
           (train-weight (one-tow-prev-tow t)))]))

(check-expect (train-weight tow1) 130)
(check-expect (train-weight tow2) 190)
(check-expect (train-weight tow3) 235)





