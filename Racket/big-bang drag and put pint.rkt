;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |big-bang drag and put pint|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;Unions



; process-nos : NumberOrString -> ...
;(define (process-nos nos)
 ; (cond
  ;  [(number? nos) (...nos...)]
   ; [(string? nos) (...nos...)]))

; to-str : NumberOrString -> String
; converts number to string
(define (to-str nos)
  (cond
    [(number? nos) (number->string nos)]
    [(string? nos) nos]))

(check-expect (to-str "I am string") "I am string")
(check-expect (to-str 3434) "3434")

;===================================================

(define mark (circle 5 "solid" "blue"))
(define canvas (empty-scene 300 300))

; a Point is a (make-point Number Number)
(define-struct point [x y])

(define p0 (make-point 0 0))
(define p1 (make-point 140 200))

; draw-point : Point -> Image
; renders p as a mark positioned in DrRacket coordinates
(define (draw-point p)
  (place-image mark (point-x p) (point-y p) canvas))

(check-expect (draw-point p0) (place-image mark (point-x p0) (point-y p0) canvas))
(check-expect (draw-point p1) (place-image mark (point-x p1) (point-y p1) canvas))

; update-world: Point Number Number MouseEvent -> Point
; returns a point at the mouse location of "button-down" or "drag' events
(define (update-world p x y me)
  (cond
    [(or (mouse=? "drag" me)(mouse=? "button-down" me)) (make-point x y)]
    [else p]))

(check-expect (update-world p0 100 100 "button-down") (make-point 100 100))
(check-expect (update-world p0 110 150 "drag") (make-point 110 150))
(check-expect (update-world p0 100 100 "move") p0)

; remove-point : MaybePoint KeyEvent -> MaybePoint
; remove the point from the canvas

(define (remove-point mp ke)
  (make-nothing))

(check-expect (remove-point p1 "z") (make-nothing))

; a MaybePoint is one of:
; - (make-nothing)
; - (make-point Number Number)
(define-struct nothing [])

; Examples
(define mp0 [make-point 100 100])
(define mp1 (make-nothing))

; process-maybepoint : MaybePoint -> ...
;(define (process-maybepoint mp)
 ; (cond
  ;  [(point? mp) ...]
   ; [(nothing? mp) ...] (... (process-point mp) ...)))

; draw-maybepoint : MaybePoint -> Image
; place the marker at the given point, if any
(define (draw-maybepoint mp)
  (cond
    [(point? mp)(place-image mark (point-x mp) (point-y mp) canvas)]
    [(nothing? mp)canvas]))

(check-expect (draw-maybepoint (make-nothing)) canvas)
(check-expect (draw-maybepoint (make-point 70 200))
              (place-image mark 70 200 canvas))

; A world is a MaybPoint
(big-bang (make-nothing)
  [to-draw draw-maybepoint]
  [on-mouse update-world]
  [on-key remove-point])