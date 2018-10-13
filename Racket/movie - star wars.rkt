;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |movie - star wars|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define background (rectangle 956 400 "solid" "black"))
(define epigraph1 (text "A long time ago in a galaxy far," 40 "blue"))
(define epigraph2 (text "far away...." 40 "blue"))
(define story-text
  (above (text "It is a period of civil war." 40 "yellow")
         (text "Rebel spaceships, striking"   40 "yellow")
         (text "lorem ipsum dolor sit"        40 "yellow")))
(define epigraph-text (above epigraph1 epigraph2))
(define stars "STAR")
(define wars "WARS")

(define shot1 (overlay epigraph-text  background))
(define shot3a (place-image (above (text stars 200 "yellow") (text wars 200 "yellow"))478 200 background))
(define shot3b
  (overlay (above (text stars 2 "yellow") (text wars 2 "yellow")) background))

; star-wars : Number -> Image
; returns the image at the given frame number
; in our Star Wars opening crawl
(check-expect (star-wars   0) shot1)
(check-expect (star-wars 100) shot1)
(check-expect (star-wars 149) shot1)
(check-expect (star-wars 150) background)
(check-expect (star-wars 199) background)
(check-expect (star-wars 200) shot3a)
(check-expect (star-wars 299) shot3b)
(check-expect (star-wars 300) (place-image story-text (/ 956 2) 470 background))
(check-expect (star-wars 350) (place-image story-text (/ 956 2) 420 background))
(check-expect (star-wars 400) (place-image story-text (/ 956 2) 370 background))

(define (star-wars t)
  (cond
    [(< t 150) shot1]
    [(< t 200) background]
    [(< t 300) (place-image (above (text stars (- 200 (*(- t 200)2)) "yellow") (text wars (- 200 (*(- t 200)2)) "yellow"))478 200 background)]
    [else (place-image story-text (/ 956 2) (- 770 t) background)]))

(animate star-wars)
