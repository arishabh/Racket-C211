;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |recurive - owl and tools|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; a Tool is one of:
; - (make-solid-tool String)
; - (make-hollow-tool String Tool)
(define-struct solid-tool [kind])
(define-struct hollow-tool [kind inner])

; Examples:
(define tool1 (make-solid-tool "phillips"))
(define tool2 (make-hollow-tool "flathead" tool1))
(define tool3 (make-hollow-tool "phillips" tool2))
(define tool4 (make-hollow-tool "flathead" tool3))
(define tool5 (make-hollow-tool "hammer" tool4))

; process-tool : Tool -> ...
; ...
(define (process-tool t)
  (cond
    [(solid-tool? t) (... (solid-tool-kind t) ...) ]
    [(hollow-tool? t) (... (hollow-tool-kind tool) ...
                       ... (process-tool
                            (hollow-tool-inner t)) ...)]))

; a Nextedowl os ne of:
; - (make-solid-tool)
; - (make-hollow NestedOwl)
(define-struct solid [])
(define-struct hollow [inner])

;Examples
(define o1 (make-solid))
(define o2 (make-hollow (make-solid)))
(define o6 (make-hollow (make-hollow
                         (make-hollow
                          (make-hollow
                           (make-hollow
                            (make-solid)))))))

; process-owl : NextedOwl -> ...
; ...
(define (process-owl no)
  (cond
    [(solid? no) ...]
    [(hollow? no) (... (process-owl
                        (hollow-inner no)) ...)]))

; count : NestedOwl -> Number
; Gives out the number of Nested Owls
(define (count no)
  (cond
    [(solid? no) 1]
    [(hollow? no) (add1 (process-owl
                         (hollow-inner no)))]))

(check-expect (count (make-solid)) 1)
(check-expect (count o2) 2)
(check-expect (count o6) 6)

(define background (empty-scene 300 300))
(define sharpee (make-pen "brown" 4 "solid" "round" "round"))

; draw-owl : NestedOwl -> Images
; renders o as concentric circles
(define (draw-owl o)
  (cond
    [(solid? o) (overlay (circle 10 "solid" "brown")
                                     background)]
    [(hollow? o) (overlay (circle (+ 14 (* 6 (- (count o) 2)))
                                  "outline" sharpee)
                          (draw-owl (hollow-inner 0)))]))

(check-expect (draw-owl o1) (overlay (circle 10 "solid" "brown")
                                     background))
(check-expect (draw-owl o2) (overlay (circle 14 "outline" sharpee)
                                     (draw-owl o1)))

; key : NestedOwl KeyEvent -> NestedOwl
; on "a" we will add hollow owl
; on "s" we subtract a hollow owl unless it is solid
; otherwise nothing
(define (key o ke)
  (cond
    [(key=? "a" ke)(make-hollow o)]
    [(key=? "s" ke) (cond
                      [(solid? o) o]
                      [(hollow? o) (hollow-inner o)])]
    [else o]))

(check-expect (key (make-solid) "a") (make-hollow (make-solid)))
(check-expect (key (make-solid) "s") (make-solid))
(check-expect (key (make-solid) " ") (make-solid))
(check-expect (key (make-hollow (make-solid)) "s") (make-solid))

; a world is a NestedOwl
(big-bang o6
  (to-draw draw-owl))