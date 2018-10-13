;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab 3 structure definiation|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; A Department is one of:
; - "biology"
; - "business"
; - "computer science"
; - "English"

(define alices-department "computer science")
(define bobs-department "business")
(define charlies-department "English")
(define dans-department "geology")

; draw-department : Department -> Image(header because it doesnt give any process)
; displays the department as image text
(define (draw-department d) ...)

; process-department : Department -> ...
; ...
(define (process-department d)
  (cond [(string=? d "biology") ...]
        [(string=? d "business") ...]
        [(string=? d "computer science") ...]
        [(string=? d "English") ...]))

; (check-expect (process-department "biology") ...)
; (check-expect (process-department "business") ...)
; (check-expect (process-department "computer science") ...)
; (check-expect (process-department "English") ...)

; salary : Department -> Number
; returns the salary for each department.
(define (salary department)
  (cond [(string=? department "biology") 100000]
        [(string=? department "business") 110000]
        [(string=? department "computer science") 120000]
        [(string=? department "English") 130000]))
(check-expect (salary "biology") 100000)
(check-expect (salary "business") 110000)
(check-expect (salary "computer science") 120000)
(check-expect (salary "English") 130000)




; A suit is one of:
; - "Diomands"
; - "Spades"
; - "Clubs"
; - "Hearts"

(define suit1 "Diomands")
(define suit2 "Spades")
(define suit3 "Clubs")

; process-suits: Suits -> ...
; ...
(define (process-suits s)
  (cond
    [(string=? s "Clubs") ...]
    [(string=? s "Hearts") ...]
    [(string=? s "Spades") ...]
    [(string=? s "Diomands") ...]))

;(check-expect (process-suits "Diomands")...)
;(check-expect (process-suits "Clubs")...)
;(check-expect (process-suits "Hearts")...)
;(check-expect (process-suits "Spades")...)

; suits-points: String -> Number
; Each club is worth certain points, so each string will return the coressponding points

(define (suits-points s)
  (cond
    [(string=? s "Clubs") 1]
    [(string=? s "Hearts") 3]
    [(string=? s "Spades") 4]
    [(string=? s "Diomands") 2]))

(check-expect (suits-points "Diomands") 2)
(check-expect (suits-points "Clubs") 1)
(check-expect (suits-points "Hearts") 3)
(check-expect (suits-points "Spades") 4)

; A RainbowColor is one of:
; - "Hot Pick"
; - "Red"
; - "Orange"
; - "Yellow"
; - "Green"
; - "Turquoise"
; - "Indigo"
; - "Violet"

(define color1 "Yellow")
(define color2 "Green")
(define color3 "Red")
(define color4 "purple")

; process-color: Color -> ...
; ...
(define (process-color c)
  (cond
    [(string=? c "Hot Pink") ...]
    [(string=? c "Orange") ...]
    [(string=? c "Red") ...]
    [(string=? c "Yellow") ...]
    [(string=? c "Green") ...]
    [(string=? c "Indigo") ...]
    [(string=? c "Turquoise") ...]
    [(string=? c "Violet") ...]))

;(check-expect (process-color "Indigo")...)
;(check-expect (process-color "Orange")...)
;(check-expect (process-color "Red")...)
;(check-expect (process-color "Green")...)
;(check-expect (process-color "Yellow")...)
;(check-expect (process-color "Violet")...)
;(check-expect (process-color "Turqouise")...)
;(check-expect (process-color "Hot Pink")...)

; next-color: color -> color
; Takes in a color in terms of string and returns the next color

(define (next-color c)
  (cond
    [(string=? c "Hot Pink") "Red"]
    [(string=? c "Red") "Orange"]
    [(string=? c "Orange") "Yellow"]
    [(string=? c "Yellow") "Green"]
    [(string=? c "Green") "Indigo"]
    [(string=? c "Indigo") "Turquoise"]
    [(string=? c "Turquoise") "Violet"]
    [(string=? c "Violet") "Hot Pink"]))

(check-expect (next-color "Indigo")"Turquoise")
(check-expect (next-color "Orange")"Yellow")
(check-expect (next-color "Red")"Orange")
(check-expect (next-color "Green")"Indigo")
(check-expect (next-color "Yellow")"Green")
(check-expect (next-color "Violet")"Hot Pink")
(check-expect (next-color "Turquoise")"Violet")
(check-expect (next-color "Hot Pink")"Red")


; A Shape is one of:
; - "triangle"
; - "rectangle"
; - "pentagon"
; - "hexagon"

(define shape1 "triangle")
(define shape2 "hexagon")
(define shape3 "rectangle")
(define shape4 "circle")

; process-shape: Shape -> ...
; ...
(define (process-shape s)
  [cond
    [(string=? s "triangle") ...]
    [(string=? s "hexagon") ...]
    [(string=? s "reactangle") ...]
    [(string=? s "pentagon") ...]])

;(check-expect (process-shape "triangle")...)
;(check-expect (process-shape "hexagon")...)
;(check-expect (process-shape "rectangle")...)
;(check-expect (process-shape "pentagon")...)

; sides-shape: Shape -> Number
; Takes in a shape and tells the number of sides of the shape
(define (sides-shape s)
  [cond
    [(string=? s "triangle") 3]
    [(string=? s "hexagon") 6]
    [(string=? s "rectangle") 4]
    [(string=? s "pentagon") 5]])

(check-expect (sides-shape "triangle")3)
(check-expect (sides-shape "hexagon")6)
(check-expect (sides-shape "rectangle")4)
(check-expect (sides-shape "pentagon")5)


; A Vehicle is (make-vehicle String String Number) (Data Definition)
(define-struct vehicle (company model year)) ;names of each field
(define brians-vehicle (make-vehicle "Toyota" "Camry" 2005))
(define marks-vehicle (make-vehicle "Ford" "F-150 Flareside" 1998))
(define rachaels-vehicle (make-vehicle "Chrysler" "PT Cruiser" 2005))
;(define kens-vehicle (make-vehicle "Bicycle" 1982)) (wont work, expected 3 arguments but found only 2
;(define elons-vehicle (make-vehicle 2018 "SpaceX" "Falcon-9 Heavy")) (will work but company will become 2018 and the oters respectively)

; 1. make-vehicle : String String Number -> Vehicle (constructor)
; 2. vehicle-company : Vehicle -> String (selector)
; 3. vehicle-model : Vehicle -> String (selector)
; 4. vehicle-year : Vehicle -> Number (selector)
; 5. vehicle? : Anything -> Boolean (predicate)

; process-vehicle : Vehicle -> ...
; ...
(define (process-vehicle v)
  (... (vehicle-company v) ...
       (vehicle-model v) ...
       (vehicle-year v) ...))
; (check-expect (process-vehicle brians-vehicle) ...)

; praise-vehicle : Vehicle -> String
; Returns a sentence that praises the given vehicle.
(define (praise-vehicle v)
  (string-append (vehicle-company v)
                 " "
                 (vehicle-model v)
                 ", a car you can trust."))
(check-expect (praise-vehicle brians-vehicle) "Toyota Camry, a car you can trust.")





; Without the simicolon it shows an error because it takes them as functions

; A book is (make-book string string number)
(define-struct book (title author nof))
(define jacks-book (make-book "Hamlet" "Shakespeare" 500))
(define toms-book (make-book "Twilight" "Stephenie Meyer" 350))
(define tims-book (make-book "Holes" "Louis Sachar" 150))

; 1. make-book: String String Number -> book (constuctor)
; 2. book-title: book -> string (selector)
; 3. book-author: book -> string (selector)
; 4. book-nof: book -> number (selector)
; 5. book? : anything -> boolean

; process-book : Book -> ...
; ...
(define (process-book b)
  (... (book-title b) ...
       (book-author b) ...
       (book-nof b) ...))
; (check-expect (process-book tims-book) ...)

; tome? : Book -> boolean
; tells if book has more than 300 pages
(define (tome? b)
  (cond
    [(> (book-nof b) 300) (boolean? #t)]
    [else (boolean? 'f)]))

(check-expect (tome? tims-book) (boolean? 'f))
(check-expect (tome? toms-book) (boolean? #t))
(check-expect (tome? jacks-book) (boolean? #t))



; A instructor is (make -instructor string string number)
(define-struct instructor(name department salary))
(define jacks-instructor (make-instructor "Kevin" "Computer Science" 2000))
(define toms-instructor (make-instructor "Raghav" "Biology" 3000))
(define tims-instructor (make-instructor "Petey" "Chemistry" 400))

; 1. make-instructor: String String Number -> intructor
; 2. intructor-name: intructor -> string (selector)
; 3. intructor-department: intructor -> string (selector)
; 4. intructor-salary: intructor -> string (selector)
; 5. instructor? : anything -> boolean

; process-instructor : Instructor -> ...
; ...
(define (process-instructor i)
  (... (instructor-name i) ...
       (instructor-department i) ...
       (instructor-salary i) ...))
; (check-expect (process-instructor tims-instructor) ...)

; A pet is (make-pet string string number)
(define-struct pet (name species age))
(define jacks-pet (make-pet "Max" "Pug" 2))
(define toms-pet (make-pet "Charlie" "Beagle" 3))
(define tims-pet (make-pet  "Teddy" "Bulldog" 1))

; 1. make-pet: String String Number -> pet
; 2. pet-name: pet -> string (selector)
; 3. pet-species: pet -> string (selector)
; 4. pet-age: pet -> string (selector)
; 5. pet? : anything -> boolean

; A pet is (make-pet string string number)
(define-struct pet1 (name species age))
(define jacks-pet1 (make-pet1 "Max" "Pug" 3))
(define toms-pet1 (make-pet1 "Charlie" "Beagle" 4))
(define tims-pet1 (make-pet1 "Teddy" "Bulldog" 2))

; process-pet : pet -> ...
; ...
(define (process-pet p)
  (... (pet-name p) ...
       (pet-species p) ...
       (pet-age p) ...))
; (check-expect (process-pet tims-pet) ...)

; birthday : pet -> number
; Increases the age of the pet by one and eturns the name and species and then ew age of pet
(define (birthday p)
  (string-append (pet-name p) " " (pet-species p) " " (integer-string(add1 (pet-age p))))

(check-expect (birthday tims-pet)(string-append (pet-name tims-pet) " " (pet-species tims-pet) " " (integer-string(add1 (pet-age tims-pet)))))
(check-expect (birthday toms-pet)(string-append (pet-name toms-pet) " " (pet-species toms-pet) " " (integer-string(add1 (pet-age toms-pet)))))
(check-expect (birthday jacks-pet)(string-append (pet-name jacks-pet) " " (pet-species jacks-pet) " " (integer-string(add1 (pet-age jacks-pet)))))






  





