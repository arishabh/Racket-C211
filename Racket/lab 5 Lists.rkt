;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab 5 Lists|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A Posn is (make-posn Number Number)
; (define-struct posn [x y]) 
; The error if we define this is -> posn: this name was defined in the
; language or a required library and cannot be re-defined

; Courtosy functions
; make-posn : Number Number -> posn
; posn-x : posn -> Number
; posn-y : posn -> Number
; posn? : Anything -> Boolean

(define pos1 (make-posn 10 10))
(define pos2 (make-posn 1 0))
(define pos3 (make-posn 30 20))

; positive-posn : Posn -> boolean
; Takes in a POsn and tells if the x coordinate is positive or not
(define (positive-posn p)
  (positive? (posn-x p)))

(check-expect (positive-posn pos1) true)
(check-expect (positive-posn pos2) true)
(check-expect (positive-posn pos3) true)

; A BunchOfPosn is one of:
; - (make-none)
; - (make-some Pons BunchOfPons)
(define-struct none [])
(define-struct some [first rest])

(define bop1 (make-none))
(define bop2 (make-some pos1 (make-none)))
(define bop3 (make-some pos2 (make-some pos3 (make-none))))

; A ListOfColorPosns is one of:
; - empty
; - (cons Posn ListOfColorPosns)

(define lop1 empty)
(define lop2 (cons pos1 empty))
(define lop3 (cons pos2 (cons pos3 empty)))

; process-list-of-pons : LisOfPons -> ...
; ...
(define (process-list-of-pons lop)
  (cond
    [(empty? lop) ...]
    [else (... (process-posn (first lop)) ...
               (process-list-of-pons (rest lop)) ..)]))

; all-positive? : ListOfColorPosn -> Boolean
; tells if all the posn in the list have a positive x coordinate
(define (all-positive? lop)
  (cond
    [(empty? lop) true]
    [else (and (positive-posn (first lop)) 
               (all-positive? (rest lop)))]))

(check-expect (all-positive? lop1) true)
(check-expect (all-positive? lop2) true)
(check-expect (all-positive? lop3) true)

(define back (empty-scene 300 300))
(define img (circle 5 "solid" "gold"))

; draw : ListOfPosn -> image
; Puts a circle on back as per the posn
(define (draw lop)
  (cond
    [(empty? lop) back]
    [else (place-image img
                       (posn-x (first lop))
                       (posn-y (first lop))
                       (draw (rest lop)))]))

(check-expect (draw lop1) back)
(check-expect (draw lop2) (place-image img
                                       10
                                       10
                                       back))
              
(check-expect (draw lop3) (place-image img
                                       1
                                       0
                                       (place-image img
                                                    30
                                                    20
                                                    back)))

; move : ListOfPosn -> ListOfPosn
; Increases the y coordinate of the posn by 1
(define (move lop)
  (cond
    [(empty? lop) empty]
    [else (cons (make-posn
                 (posn-x (first lop))
                 (add1 (posn-y (first lop)))) 
                (move (rest lop)))]))

(check-expect (move lop1) empty)
(check-expect (move lop2) (cons
                           (make-posn 10 11)
                           empty))
(check-expect (move lop3) (cons
                           (make-posn 1 1)
                           (cons
                            (make-posn 30 21)
                            empty)))

; add : ListofPosn KeyEvent -> ListOfPOsn
; adds a random posn in the list
(define (add lop ke)
  (cond
    [(key-event? ke)
        (cond
          [(empty? lop) (cons (make-posn
                               (random 300)
                               (random 300))
                              empty)]
          [else (cons (make-posn
                       (random 300)
                       (random 300))
                      (cons
                       (first lop) 
                       (rest lop)))])]))
  
(check-random (add lop1 "a") (cons (make-posn
                                (random 300)
                                (random 300))
                               empty))
(check-random (add lop2 " ") (cons (make-posn
                                (random 300)
                                (random 300))
                               (cons pos1
                                     empty)))
(check-random (add lop3 "z") (cons (make-posn
                                (random 300)
                                (random 300))
                               (cons pos2
                                     (cons pos3
                                           empty))))

;; a World is Image of a posn on back
;(big-bang empty
;  (to-draw draw)
;  (on-key add)
;  (on-tick move))


;----------------------------------------------------------------------------------------

; a ColorPosn is a : (make-color-posn Number Number String)
(define-struct cp(x y color))

(define cp1 (make-cp
             (posn-x pos1)
              (posn-y pos1)
             (random (color))))
(define cp2 (make-cp
             (posn-x pos2)
             (posn-y pos2)
             (random (color))))
(define cp3 (make-cp
             (posn-x pos3)
             (posn-y pos3)
             (random (color))))

; A ListOfColorPosns is one of:
; - empty
; - (cons ColorPosn ListOfColorPosns)

(define locp1 empty)
(define locp2 (cons cp1 empty))
(define locp3 (cons cp2 (cons cp3 empty)))

; draw : ListOfColorPosn -> image
; Puts a circle on back as per the posn
(define (color-draw locp)
  (cond
    [(empty? locp) back]
    [else (place-image img
                       (cp-x (first locp))
                       (cp-y (first locp))
                       (draw (rest locp)))]))

(check-expect (color-draw locp1) back)
(check-expect (color-draw locp2) (place-image img
                                       10
                                       10
                                       back))
              
(check-expect (color-draw locp3) (place-image img
                                       1
                                       0
                                       (place-image img
                                                    30
                                                    20
                                                    back)))

; move : ListOfColorPosn -> ListOfColorPosn
; Increases the y coordinate of the posn by 1
(define (color-move locp)
  (cond
    [(empty? locp) empty]
    [else (cons (make-cp
                 (cp-x (first locp))
                 (add1 (cp-y (first locp)))
                 (cp-color (first locp))) 
                (move (rest locp)))]))

(check-expect (move locp1) empty)
(check-expect (move locp2) (cons
                           (make-cp 10 11
                                    (cp-color cp1))
                           empty))
(check-expect (move locp3) (cons
                           (make-cp 1 1
                                    (cp-color cp2))
                           (cons
                            (make-cp 30 21
                                     (cp-color cp3))
                            empty)))

; add : ListOfColorPosn KeyEvent -> ListOfColorPosn
; adds a random posn in the list
(define (color-add locp ke)
  (cond
    [(key-event? ke)
        (cond
          [(empty? locp) (cons (make-cp
                               (random 300)
                               (random 300)
                               (random (color)))
                              empty)]
          [else (cons (make-cp
                       (random 300)
                       (random 300)
                       (random (color)))
                      (cons
                       (first locp) 
                       (rest locp)))])]))
  
(check-random (color-add locp1 "a") (cons (make-cp
                                (random 300)
                                (random 300)
                                (random (color)))
                               empty))
(check-random (color-add locp2 " ") (cons (make-cp
                                (random 300)
                                (random 300)
                                (random (color)))
                               (cons cp1
                                     empty)))
(check-random (color-add locp3 "z") (cons (make-posn
                                (random 300)
                                (random 300)
                                (random (color)))
                               (cons cp2
                                     (cons cp3
                                           empty))))

; a World is Image of a posn on back
(big-bang empty
  (to-draw color-draw)
  (on-key color-add)
  (on-tick color-move))

