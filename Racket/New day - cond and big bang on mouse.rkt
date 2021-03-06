;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |New day - cond and big bang on mouse|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define tl-background (rectangle 80 240 "solid" "black"))
(define green-tl (circle 40 "solid" "green"))
(define yellow-tl (circle 40 "solid" "yellow"))
(define red-tl (circle 40 "solid" "red"))

; a TrafficLight is one of
; "green"
; "yellow"
; "red"

; next-tl: traffic light -> traffic light(next)
(define (next-tl tl)
  (cond
    [(string=? tl "green") "yellow"]
    [(string=? tl "yellow") "red"]
    [(string=? tl "red") "green"]))

; Tests
(check-expect (next-tl "green") "yellow")
(check-expect (next-tl "yellow") "red")
(check-expect (next-tl "red") "green")

; draw-tl: traffic light -> image
; renders tl as a typical traffic light
(define (draw-tl tl)
  (cond
    [(string=? "green" tl) (place-image green-tl 40 200 tl-background)]
    [(string=? "yellow" tl) (place-image yellow-tl 40 120 tl-background)]
    [(string=? "red" tl) (place-image red-tl 40 40 tl-background)]))

(check-expect (draw-tl "green")(place-image green-tl 40 200 tl-background))
(check-expect (draw-tl "yellow")(place-image yellow-tl 40 120 tl-background))
(check-expect (draw-tl "red")(place-image red-tl 40 40 tl-background))

; a MouseEvent is one of
; - "button-down"
; - "button-up"
; - "drag"
; - "move"
; - "enter"
; - "leave"

;  process-me: MouseEvent -> ...
(define (process-me me)
  (cond
    [(string=? me "button-down") (... me ...)]
    [(string=? me "button-up") (... me ...)]
    [(string=? me "drage") (... me ...)]
    [(string=? me "move") (... me ...)]
    [(string=? me "enter") (... me ...)]
    [(string=? me "leave") (... me ...)]))

; mouse : TrafficLIght Number Number MouseEvent -> TrafficLight
; returns next traffic light in sequence on button-down
(define (mouse tl x y me)
  (cond
    [(string=? me "button-down") (next-tl tl)]
    [else tl]))

(check-expect (mouse "green" 0 0 "button-down") "yellow")
(check-expect (mouse "yellow" 0 0 "button-down") "red")
(check-expect (mouse "red" 0 0 "button-down") "green")
(check-expect (mouse "green" 0 0 "button-up") "green")
(check-expect (mouse "green" 0 0 "enter") "green")
(check-expect (mouse "green" 0 0 "move") "green")
(check-expect (mouse "green" 0 0 "leave") "green")
(check-expect (mouse "green" 0 0 "drag") "green")

; keys : TrafficLight KeyEvent -> TrafficLight
;change TrafficLight to red when r is pressed
;change TrafficLight to yellow when y is pressed
;change TrafficLight to green when g is pressed
(define (keys tl ke)
  (cond
    [(string=? "g" ke)"green"]
    [(string=? "y" ke)"yellow"]
    [(string=? "r" ke)"red"]))

(check-expect (keys "green" "r")"red")
(check-expect (keys "red" "r")"red")
(check-expect (keys "yellow" "r")"red")
(check-expect (keys "green" "g")"green")
(check-expect (keys "yellow" "y")"yellow")
(check-expect (keys "red" "g")"green")

; world is a traffic light
(big-bang "green"
  [to-draw draw-tl]
;  [on-mouse mouse]
  [on-key keys])
  





    
