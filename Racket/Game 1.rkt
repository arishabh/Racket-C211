;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Game 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define background (rectangle 200 300 "solid" "black"))
(define alien (circle 10 "solid" "green"))
(define alien-speed 1/4)
(define bullet-speed 2)
(define player-speed 5)
(define defender (triangle 20 "solid" "white"))
(define bullet (rectangle 3 8 "solid" "white"))
(define defender-y 280)

; a Posn is a (make-posn Number Number)
(define p1 (make-posn 1 2))
(define p2 (make-posn 150 100))

; process-posn : Posn -> ...
(define (process-posn p)
  (... (posn-x p) ... (posn-y p) ...))

; place-posn : Posn Image Image -> Image
; place the posn on the given image
(define (place-posn p inv img)
  (place-image inv (posn-x p) (posn-y p) img))


(check-expect (place-posn p1 alien background)
              (place-image alien (posn-x p1) (posn-y p1) background))
(check-expect (place-posn p2 (triangle 10 "solid" "red") (empty-scene 200 300))
              (place-image (triangle 10 "solid" "red")
                           (posn-x p2) (posn-y p2) (empty-scene 200 300)))

; A ListOfPosns is one of:
; - empty
; - (cons Posn ListOfPosns)
(define ps0 empty)
(define ps1 (cons p1 empty))
;(define ps2 (cons p2 (cons p1 empty)))
(define ps2 (cons p2 ps1))

; process-posns : ListOfPosns -> ...
(define (process-posns ps)
  (cond
    [(empty? ps) ...]
    [else (... (process-posn (first ps)) ...
           ... (process-posns (rest ps)) ...)]))

; draw-aliens : ListOfPosns -> Image
; draw the given aliens
(define (draw-aliens ps)
  (cond
    [(empty? ps) background]
    [else (place-posn (first ps)
                      alien
                      (draw-aliens (rest ps)))]))
    
(check-expect (draw-aliens empty) background)
(check-expect (draw-aliens ps1) (place-image alien 1 2 background))
(check-expect (draw-aliens ps2) (place-image alien 150 100
                                             (place-image alien 1 2 background)))

; move-aliens : ListOfPosns -> ListOfPosns
; returns the given list of posns with each Y increased by 1
(define (move-aliens ps)
  (cond
    [(empty? ps) ps]
    [else (cons (move-posn (first ps))
                (move-aliens (rest ps)))]))

(check-expect (move-aliens empty) empty)
(check-expect (move-aliens (cons (make-posn 100 100) empty))
              (cons (make-posn 100 (+ alien-speed 100)) empty))

; move-posn : Posn -> Posn
; adds alien-speed to the y value of given posn
(define (move-posn p)
  (make-posn (posn-x p) (+ alien-speed (posn-y p))))

(check-expect (move-posn (make-posn 100 100))
              (make-posn 100 (+ alien-speed 100)))

; a NaturalNumber is one of:
; - 0
; - (add1 NaturalNumber)

; process-nat : NaturalNumber -> ...
(define (process-nat n)
  (cond
    [(zero? n) ...]
    [else (... (process-nat (sub1 n)) ...)]))

; random-aliens : NaturalNumber -> ListOfPosns
; returns the given number of posns at random x values at y = 15
(define (random-aliens n)
  (cond
    [(zero? n) empty]
    [else (cons (make-posn (random 200) 15) (random-aliens (sub1 n)))]))


(check-expect (random-aliens 0) empty)
(check-random (random-aliens 1) (cons (make-posn (random 200) 15) empty))



; a MaybePosn is one of:
; - (make-no-bullet)
; - Posn
(define-struct no-bullet [])

; process-mp : MaybePosn -> ...
(define (process-mp mp)
  (cond
    [(no-bullet? mp) ...]
    [(posn? mp) (... (process-posn mp) ...)]))

; a World is a (make-world ListOfPosns Number MaybePosn)
(define-struct world [aliens player bullet])

; process-world : World -> ...
(define (process-world w)
  (... (world-aliens w) ... (world-player w) ... (world-bullet w) ...))

; Examples
(define w1 (make-world (list (make-posn 20 20) (make-posn 50 20))
                       100
                       (make-no-bullet)))
(define w2 (make-world (list (make-posn 20 20) (make-posn 50 20))
                       100
                       (make-posn 100 100)))
(define w3 (make-world empty
                       100
                       (make-posn 100 100)))

; draw-world : World -> Image
; renders a world
(define (draw-world w)
  (draw-bullet (world-bullet w)
               (place-posn (make-posn (world-player w) defender-y)
                           defender
                           (draw-aliens (world-aliens w)))))

(check-expect (draw-world w1)
              (place-image defender
                           100 defender-y
                           (draw-aliens (list (make-posn 20 20) (make-posn 50 20)))))
(check-expect (draw-world w2)
              (place-image bullet
                           100 100
                           (place-image defender
                                        100 defender-y
                                        (draw-aliens (list (make-posn 20 20) (make-posn 50 20))))))
              

; draw-bullet : MaybePosn Img -> Img
; draws a bullet on img
(define (draw-bullet mp img)
  (cond
    [(no-bullet? mp) img]
    [else (place-posn mp bullet img)]))


(check-expect (draw-bullet (make-no-bullet) background) background)
(check-expect (draw-bullet (make-posn 100 100) background)
              (place-posn (make-posn 100 100) bullet background))


; tick : World -> World
; moves everything in world responsive to clock
(define (tick w)
  (make-world (move-aliens (remove-aliens (world-aliens w) (world-bullet w)))
              (world-player w)
              (move-bullet (remove-bullet (world-aliens w) (world-bullet w)))))

(check-expect (tick (make-world (list (make-posn 100 10) (make-posn 150 10))
                                100
                                (make-no-bullet)))
              (make-world (move-aliens (list (make-posn 100 10) (make-posn 150 10)))
                          100
                          (move-bullet (make-no-bullet))))

(check-expect (tick (make-world (list (make-posn 100 10) (make-posn 150 10))
                                100
                                (make-posn 100 100)))
              (make-world (move-aliens (list (make-posn 100 10) (make-posn 150 10)))
                          100
                          (move-bullet (make-posn 100 100))))

(check-expect (tick (make-world (list (make-posn 100 10) (make-posn 150 10))
                                100
                                (make-posn 100 10)))
              (make-world (move-aliens (list (make-posn 150 10)))
                          100
                          (make-no-bullet)))


; move-bullet : MaybePosn -> MaybePosn
; moves bullet (if any) up and removes if off screen
(define (move-bullet mp)
  (cond
    [(no-bullet? mp) (make-no-bullet)]
    [(posn? mp)
     (cond
       [(<= (posn-y mp) (- 0 (/ (image-height bullet) 2))) (make-no-bullet)]
       [else (make-posn (posn-x mp) (- (posn-y mp) bullet-speed))])]))


(check-expect (move-bullet (make-no-bullet)) (make-no-bullet))
(check-expect (move-bullet (make-posn 50 50)) (make-posn 50 (- 50 bullet-speed)))
(check-expect (move-bullet (make-posn 50 (- 0 (/ (image-height bullet) 2)))) (make-no-bullet))

; player : World KeyEvent -> World
; move player left and right, shoot bullet
(define (player w ke)
  (cond
    [(key=? "left" ke)
     (make-world (world-aliens w)
                 (- (world-player w) player-speed)
                 (world-bullet w))]
    [(key=? "right" ke)
     (make-world (world-aliens w)
                 (+ (world-player w) player-speed)
                 (world-bullet w))]
    [(key=? " " ke)
     (cond
       [(no-bullet? (world-bullet w))
        (make-world (world-aliens w)
                    (world-player w)
                    (make-posn (world-player w) defender-y))]
       [else w])]
    [else w]))

(check-expect (player w1 "left")
              (make-world (list (make-posn 20 20) (make-posn 50 20))
                          (- 100 player-speed)
                          (make-no-bullet)))
(check-expect (player w1 "right")
              (make-world (list (make-posn 20 20) (make-posn 50 20))
                          (+ 100 player-speed)
                          (make-no-bullet)))
(check-expect (player w1 " ")
              (make-world (list (make-posn 20 20) (make-posn 50 20))
                          100
                          (make-posn 100 defender-y)))
(check-expect (player w2 " ") w2)
(check-expect (player w1 "y") w1)

; hit? : MaybePosn Posn -> Boolean
; check whether the given bullet hit the given alien
(define (hit? mp p)
  (cond
    [(no-bullet? mp) false]
    [else (and (<= (abs (- (posn-x mp) (posn-x p))) 10)
               (<= (abs (- (posn-y mp) (posn-y p))) 10))]))

(check-expect (hit? (make-no-bullet) (make-posn 100 100)) false)
(check-expect (hit? (make-posn 50 50) (make-posn 20 20)) false)
(check-expect (hit? (make-posn 100 100) (make-posn 100 100)) true)
(check-expect (hit? (make-posn 100 100) (make-posn 90 90)) true)
(check-expect (hit? (make-posn 100 100) (make-posn 105 105)) true)

; remove-aliens : ListOfPosns MaybePosn -> ListOfPosns
; remove hit aliens (if any)
(define (remove-aliens lop mp)
  (cond
    [(empty? lop) empty]
    [else
     (cond
       [(hit? mp (first lop)) (remove-aliens (rest lop) mp)]
       [else (cons (first lop) (remove-aliens (rest lop) mp))])]))

(check-expect (remove-aliens empty (make-no-bullet)) empty)
(check-expect (remove-aliens (list (make-posn 50 50)) (make-posn 50 50)) empty)
(check-expect (remove-aliens (list (make-posn 50 50)) (make-posn 20 50))
              (list (make-posn 50 50)))


; remove-bullet : ListOfPosns MaybePosn -> MaybePosn
; remove the bullet if it hit an alien
(define (remove-bullet lop mp)
  (cond
    [(empty? lop) mp]
    [else
     (cond
       [(hit? mp (first lop)) (make-no-bullet)]
       [else (remove-bullet (rest lop) mp)])]))
    
(check-expect (remove-bullet empty (make-no-bullet)) (make-no-bullet))
(check-expect (remove-bullet empty (make-posn 50 100)) (make-posn 50 100))
(check-expect (remove-bullet (list (make-posn 50 100)) (make-posn 50 100))
              (make-no-bullet))
(check-expect (remove-bullet (list (make-posn 50 100) (make-posn 100 100)) (make-posn 100 100))
              (make-no-bullet))

; the-end -> World -> Boolean
; gives out true when there are no aliens left, else gives false
(define (the-end w)
  (cond
    [(empty? (world-aliens w)) true]
    [else false]))
  
(big-bang (make-world (random-aliens 2) 100 (make-no-bullet))
  [to-draw draw-world]
  [on-tick tick]
  [on-key player]
  [stop-when the-end])




