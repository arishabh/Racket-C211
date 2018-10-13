;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Recursive struct|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; a Point is a (make-point Number Number)
(define-struct point[x y])

(define p0 (make-point 0 0))
(define p1 (make-point 30 40))

; process-point : Point -> ...
; ...
(define (process-point p)
  (.... (point-x p) ... (point-y p) ...))

; a BunchOfPoints is one of:
; - (make-none)
; - (make-some Point BunchOfPoints)

(define-struct none [])
(define-struct some [first rest])

(define p11 (make-none))
(define p12 (make-some p1 (make-none)))
(define p13 (make-some p0 (make-some p1 (make-none))))
(define p14 (make-some (make-point 70 40) (make-some (make-point 30 50) (make-none))))

; process-bunchofpoints : BunchofPOints -> ...
; ...
;(define (process-bunchofpoints bp)
;  (cond
;    [(none? bp) ...]
;    [(some? bp) (process-point(some-first bp))
;                (process-bunchofpoints(some-rest bp))]))


(define mark (circle 10 "solid" "blue"))
(define background (empty-scene 300 300))

;draw-bop : BunchOfPOints -> Image
; renders a bop as marks on the background
(define (draw-bop bp)
  (cond
    [(none? bp) background]
    [(some? bp) (place-image mark
                             (point-x(some-first bp))
                             (point-y(some-first bp))
                             (draw-bop(some-rest bp)))]))

(check-expect (draw-bop (make-none)) background)
(check-expect (draw-bop (make-some (make-point 0 0) (make-none)))
              (place-image mark 0 0 background))
(check-expect (draw-bop (make-some (make-point 0 0)
                                   (make-some (make-point 30 40)
                                              (make-none))))
              (place-image mark
                           0 0
                           (place-image mark
                                        30 40
                                        background)))

; paint : BunchOfPOints Number Number MouseEvent -> BunchofPOints
; adds point from "button-down" or "drag" events to bop
(define (paint bp x y me)
  (cond
    [(mouse=? "button-down" me) (make-some (make-point x y) bp)]
    [(mouse=? "drag" me) (make-some (make-point x y) bp)]
    [else bp]))

(check-expect (paint p11 0 0 "button-down")(make-some (make-point 0 0) p11))
(check-expect (paint p12 30 40 "drag")(make-some (make-point 30 40) p12))

; raise : BunchOfPoints -> BunchOfPoints
; raises the bunch of points by 1 x and y coordinate
(define (raise bp)
  (cond
    [(none? bp) background]
    [(some? bp) (place-image mark
                             (- (point-x(some-first bp)) 1)
                             (- (point-y(some-first bp)) 1)
                             (raise (some-rest bp)))]))


(big-bang (make-none)
  (on-mouse paint)
  ;(on-tick raise)
  (to-draw draw-bop))
