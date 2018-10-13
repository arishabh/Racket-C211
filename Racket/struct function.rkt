;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |struct function|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A Point is a (make-point Number Number)
(define-struct point [x y])

; Example
(define p1 (make-point 3 4))
(make-point 4 5)
(point-x (make-point 4 5))
(point? p1)
(point? (point-x p1))

; Courtesy Function
; make-person : Number Number -> Point
; point-x : Point -> Number
; point-y : Point -> Number
; point? : Anything -> Boolean

; process-point: Point -> ...
; ...
(define (process point p)
  (... (point-x p) ...
       (point-y p) ...))


; dist-from-origon: Point -> Number
; It takes a point and calculates the distance from p to (make-point 0 0)
(define (dist-from-origin p)
  (sqrt(+ (sqr(point-x p)) (sqr(point-y p)))))

(check-expect (dist-from-origin (make-point 3 4)) 5)
(check-expect (dist-from-origin (make-point 5 12)) 13)

; dist : Point Point ->
; Calculates the distance from point p and q
(define (dist p q)
   (sqrt(+ (sqr(- (point-x p) (point-x q))) (sqr(- (point-y p) (point-y q))))))

(check-expect (dist (make-point 3 4) (make-point 0 0)) 5)
(check-expect (dist (make-point 205 212) (make-point 200 200)) 13)

; A Person is a (make-person String Number)
(define-struct person [name age])

(define me(make-person "Alice" 22))

; teanager? -> Person -> Boolean
; decides if the person's age is between 13 and 19
(define (teenager? p)
  (and (>= (person-age p) 13) (<= (person-age p) 19)))

(check-expect (teenager? me) false)
(check-expect (teenager? (make-person "Max" 15)) true)


(define width 400)
(define height 300)
(define origin (make-point (/ width 2) (/ height 2)))



; draw-circle : POint -> Image
; renders a circle with radius determined by p
(define (draw-circle p)
  (place-image (circle (dist p origin) "outline" "blue")
               (point-x origin)
               (point-y origin)
               (empty-scene width height)))

(check-expect (draw-circle (make-point 230 190))  (place-image (circle 50 "outline" "blue")
                                                               (point-x origin)
                                                               (point-y origin)
                                                               (empty-scene width height)))
              
(check-expect (draw-circle origin)  (place-image (circle 0 "outline" "blue")
                                                  (point-x origin)
                                                  (point-y origin)
                                                  (empty-scene width height)))



                               




  
