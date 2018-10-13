;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Moving rocket|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define background (rectangle 200 300 "solid" "lightblue"))
(define rocket (above (triangle 20 "solid" "blue")
                      (rectangle 20 60 "solid" "white")))
(define resting-y-pos (- 300 (/ (image-height rocket) 2)))


;Step 1: Data Definiation

; a Time is a non-negative number

; Exampes
; 0
; 100
; 301

; Non- Examples
; -3
; "c211"

; an Image is an image (in generak, you don't have to mention this)

; Step 2: Signature, Purpose Statement and header
; draw-rocket : Time -> Image
; Render a launched rocket at time t
; (define (draw-rocket t)...)

;Step 3: Functional Examples
; (draw-rocket 0) => (place-image rocket 100 resting-y-pos background)
; (draw-rocket 100) => (place-image rocket 100 (- resting-y-pos 100) background)
; (draw-rocket 301) => Only Background

;Step 4: Function Template
;(define (draw-rocket t) (... t ...))

; Step 5: Function Definition
(define (draw-rocket t)
  (place-image rocket 100 (- resting-y-pos t) background))

; Step 6: Testing
(check-expect (draw-rocket 0)
              (place-image rocket 100 resting-y-pos background))
(check-expect (draw-rocket 100)
              (place-image rocket 100 (- resting-y-pos 100) background))
(check-expect (draw-rocket 301)
              (place-image rocket 100 (- resting-y-pos 301) background))

(animate draw-rocket)

;---------------------------------------------------------------------------------------;

; Data Definition
; a Time is one of:
; - an integer at least 0 but no more than 99
; - an integer at least 100

;Interpretation: a rocket is either preparing to launch or have been launched

; Examples
; 0
; 99
; 100
; 301

; draw-rocket-2 : Time -> Image
; render an image in either prelaunch mode or launch mode
(define (draw-rocket-2 t) ... )
