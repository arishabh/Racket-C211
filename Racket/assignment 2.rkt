;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |assignment 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define background (rectangle 200 300 "solid" "lightblue"))
(define rocket (above (triangle 20 "solid" "blue")
                      (rectangle 20 60 "solid" "white")))
(define resting-y-pos (- 300 (/ (image-height rocket) 2)))


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

; draw-rocket : Time -> Image
; Render a rocket either in prelaunch mode or launch mode at time t
; (define (draw-rocket t)...)
; 

(check-expect (draw-rocket 0)
              (place-image rocket 100 resting-y-pos background))
(check-expect (draw-rocket 99)
              (place-image rocket 100 resting-y-pos background))
(check-expect (draw-rocket 100)
              (place-image rocket 100 (- resting-y-pos 1) background))
(check-expect (draw-rocket 400) background)

(define (draw-rocket t)
  (cond
    [(<= t 99) (place-image rocket 100 resting-y-pos background)]
    [(and (> t 99) (< t 450)) (place-image rocket 100 (- resting-y-pos (- t 99)) background)]
    [(and (>= t 450) (< t 751)) (place-image rocket 100 (- t (+ 450 (/ (image-height rocket) 2))) background)]
    [(>= t 751) (place-image rocket 100 resting-y-pos background)])) 

(animate draw-rocket)

