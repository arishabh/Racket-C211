;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A SolarSystem is one of:
; - (make-sun)
; - (make-planet Distance inner-ss)

(define-struct sun[])
(define-struct planet [dist inner-ss])

(define ss1 (make-sun))
(define ss2 (make-planet 300 (make-sun)))
(define ss3 (make-planet 200 (make-planet 100 (make-sun))))

; process-solar-system : solar system -> ...
;(define (process-solar-system s)
;  (cond
;    [(sun? s) ...]
;    [(planet? s) ... (planet-dist s) ...
;                 ... (process-solar-system (planet-inner-ss s))]))

; dist-of-solar-system: SolarSystem -> Number
; returns the distance between its outermost object and its center
(define (distance-of-solar-system s)
  (cond
    [(sun? s) 0]
    [(planet? s) (+ (planet-dist s) 
                    (distance-of-solar-system
                     (planet-inner-ss s)))]))

(check-expect (distance-of-solar-system ss1) 0)
(check-expect (distance-of-solar-system ss2) 300)
(check-expect (distance-of-solar-system ss3) 300)

; add-to-solar-system : SolarSystem Number -> SolarSystem
; adds another planet to the solar system with enetered number
; the distance from the next inner planet.
(define (add-to-solar-system s num)
  (cond
    [(sun? s) (make-planet num (make-sun))]
    [(planet? s) (make-planet num s)]))

(check-expect (add-to-solar-system ss1 300)(make-planet 300 (make-sun)))
(check-expect (add-to-solar-system ss2 700)(make-planet
                                            700
                                            (make-planet
                                             300(make-sun))))
(check-expect (add-to-solar-system ss3 100)(make-planet
                                            100
                                            (make-planet
                                             200
                                             (make-planet
                                              100 (make-sun)))))
(define sun-img (circle 20 "solid" "yellow"))
(define background (empty-scene 500 500))
; draw-solar-system : SolarSystem -> image
; renders an image of the solar system on the background
(define (draw-solar-system s)
  (cond
    [(sun? s) (place-image sun-img 250 250 background)]
    [(planet? s) (place-image
                  (circle (planet-dist s) "outline" "red")
                  250
                  250
                  (draw-solar-system (planet-inner-ss s)))]))

(check-expect (draw-solar-system ss1)
              (place-image sun-img 250 250 background))

(check-expect (draw-solar-system ss2)
              (place-image (circle 300 "outline" "red")
                            250
                            250
                            (place-image
                             sun-img
                             250
                             250
                             background)))

; A Pair is a: (make-pair Number Number)
(define-struct pair[x y])

(define p1 (make-pair 10 20))
(define p2 (make-pair 0 0))
(define p3 (make-pair 30 10))

; a PackOfPairs is one of:
; - (make-no-pairs)
; - (make-some-pairs Number PackOfPairs)

(define-struct no-pairs[])
(define-struct some-pairs [pair prev-pop])

(define pp1 (make-no-pairs))
(define pp2 (make-some-pairs p2 (make-no-pairs)))
(define pp3 (make-some-pairs p3 (make-some-pairs
                                 p1
                                 (make-no-pairs))))

; process-pair : Pair -> ...
; ...
(define (process-pair p)
  (... (pair-x p) ... (pair-y p) ...))

; process-pairs : PackOfPairs -> ...
; ...
(define (process-pairs pop)
  (cond
    [(no-pairs? pop)...]
    [(some-pairs? pop) (...(process-pair (some-pairs-pair pop))
                       (process-pairs (some-pairs-prev-pop pop)))]))

(define img (circle 10 "solid" "black"))
;(define background (empty-scene 200 300))

; draw-pairs : PackOfPairs -> Image
; renders an image of a circle on the empty screen
(define (draw-pairs pop)
  (cond
    [(no-pairs? pop) background]
    [(some-pairs? pop) (place-image img
                             (pair-x(some-pairs-pair pop))
                             (pair-y(some-pairs-pair pop))
                             (draw-pairs(some-pairs-prev-pop pop)))]))

(check-expect (draw-pairs (make-no-pairs)) background)
(check-expect (draw-pairs (make-some-pairs (make-pair 10 10)
                                           (make-no-pairs)))
              (place-image img 10 10 background))


; any-paint : PackOfPairs Number Number MouseEvent -> PackOfPairs
; adds circle from any event
(define (any-paint pop x y me)
  (cond
    [(mouse-event? me)(make-some-pairs
                       (make-pair x y)
                       pop)]))

(check-expect (any-paint pp1 0 0 "drag")(make-some-pairs
                                        (make-pair 0 0)
                                        pp1))
(check-expect (any-paint pp2 100 200 "button-down")(make-some-pairs
                                                   (make-pair 100 200)
                                                   pp2))
(check-expect (any-paint pp3 0 0 "move")(make-some-pairs
                                        (make-pair 0 0)
                                        pp3))
(check-expect (any-paint pp3 0 0 "enter")(make-some-pairs
                                         (make-pair 0 0)
                                         pp3))
              
; any-undo : PackOfPairs KeyboardEvent -> PackOfPairs
; Removes the last circle printed
(define (any-undo pop ke)
  (cond
    [(key-event? ke) (cond
                       [(no-pairs? pop) pop]
                       [(some-pairs? pop) (some-pairs-prev-pop pop)])]))

(check-expect (any-undo (make-some-pairs p1 pp1) "z")pp1)
(check-expect (any-undo (make-some-pairs p2 pp2) "a")pp2)
(check-expect (any-undo (make-some-pairs p3 pp3) " ")pp3)

; paint: PackOfPairs Number Number MouseEvent -> PackOfPairs
; adds a circle at te coordinates if there is a "button-down"
; or a "drag" mouse event
(define (paint pop x y me)
  (cond
    [(or (mouse=? me "drag") (mouse=? me "button-down"))(make-some-pairs
                                                       (make-pair x y)
                                                       pop)]
    [else pop]))

(check-expect (paint pp1 0 0 "drag")(make-some-pairs
                                        (make-pair 0 0)
                                        pp1))
(check-expect (paint pp2 100 200 "button-down")(make-some-pairs
                                                   (make-pair 100 200)
                                                   pp2))
(check-expect (paint pp3 0 0 "move") pp3)
(check-expect (paint pp3 0 0 "enter") pp3)
              

; undo: PackOfPairs KeyEvent -> PackOfPairs
; undos the last circle if "z" pressed
(define (undo pop ke)
  (cond
    [(key=? "z" ke) (cond
                       [(no-pairs? pop) pop]
                       [(some-pairs? pop) (some-pairs-prev-pop pop)])]
    [else pop]))
(check-expect (undo (make-some-pairs p1 pp1) "z")pp1)
(check-expect (undo (make-some-pairs p2 pp2) "z")pp2)
(check-expect (undo (make-some-pairs p2 pp2) "a")
              (make-some-pairs p2 pp2))
(check-expect (undo (make-some-pairs p2 pp2) "\t")
              (make-some-pairs p2 pp2))
(check-expect (undo (make-some-pairs p3 pp3) " ")
              (make-some-pairs p3 pp3))

(big-bang (make-no-pairs)
  (on-mouse paint)
  (on-key undo)
  (to-draw draw-pairs))





