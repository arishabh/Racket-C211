;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define rocket (above (triangle 20 "solid" "blue")
                      (rectangle 20 60 "solid" "white")))
(define resting-y-pos (- 300 (/ (image-height rocket) 2)))
(define init-speed 1)
(define init-angle 45)
(define init-x-vel (* init-speed (cos init-angle)))
(define init-y-vel (* init-speed (sin init-angle)))
(define screen (empty-scene 500 200))
(define t 0)


; a Time is one of:
; - an integer at least 0 and not more than 99
; - an integer at least 100

; Interpretation: The rocket is either preparing to launch or has launched

; Examples
; 0
; 99
; 100
; 301

; an Image is an image (in general, you don't have to mention this)
; init-speed is a non 0 positive number
; init-angle is a an angle (90, 180) less than 360 and more than 0

; draw-rocket : Time -> Image
; Render a rocket with positions x-pos and y-po
; (define (draw-rocket x-pos y-pos)...)
; 
(check-expect (launch 0)
              (place-image rocket 100 resting-y-pos screen))
(check-expect (launch 100)
              (place-image rocket (* init-x-vel 100) (- (* init-y-vel 100) (* 0.5 0.002 100 100) screen))
(check-expect (launch 400) screen)

; y-pos: Time -> Position in y axis
(define (y-pos t)
  (- (* init-y-vel t) (* 0.5 0.002 t t)))

; y: y-pos -> coordinate on y axis in racket
(define y (- (image-height screen) y-pos))

; x-pos: Time -> Position in x axis
(define (x-pos t)
  (* init-x-vel t))

; draw-sprite: y position and x position -> Image
(define (draw-sprite x-pos y)
  (place-image rocket x-pos y screen))

(define (launch t)
  (cond
    [(and (> x-pos 0) (> y 0)) (draw-sprite x-pos y)]))
  
(animate launch)

