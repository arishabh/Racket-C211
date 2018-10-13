;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 0a, b, c
; I spoke to techPOintX
; I think the most needed skill in today's world is to be a problem solver and to creat effecient code
; that is as short as possible and doesn't take a lot of time to run. So effecient code design
; Along with that the attitude of learning more and being open to learning and the skill to grasp new
; things is very important.

; I asked them the different skills they see in a person for hiring them and also if there is any
; specific coding language they look for
; Because most of the companies were huge, they didnt require any specific coding language because
; they had systems in all knds of languages but they do require few famous ones such as
; C and python. But that is also recommended and not required. The skills I shared above. They are
; also looking for a alot of web designers or security people.


(require 2htdp/image)
(require 2htdp/universe)

(define background (rectangle 800 100 "solid" "black"))


; draw: string -> image
; it was take in a key from the keyboard and display it on a background

(define (draw str)
    (overlay (text str 40 "yellow") background))

(check-expect (draw " ") background)
(check-expect (draw "d") (overlay (text "d" 40 "yellow") background))

;procces: STRing KeyEvent -> string
;Takes in a key event and world and gives out the next string, and checks if its a " " so it can
; reset it

(define (process str ke)
  (cond
    [(string=? ke " ") " "]
    [else (string-append str ke)]))

(check-expect (process "a" "d")(string-append "a" "d"))
(check-expect (process "abc" "d")(string-append "abc" "d"))

  
(big-bang "a"
  [on-key process]
  [to-draw draw])


; Exercise 5

; A Address is (make-address String String String Number)
; Examples:
;  (make-address "1900 E 10th Street" "Eigenmenn Hall 745" "Bloomington" 47408)
;  (make-address"700 N Woodlawn Ave" "Room 0121" "Bloomington" 47408)
(define-struct address(street apartment city zip))

(define my-address
  (make-address "1900 E 10th Street" "Eigenmenn Hall 745" "Bloomington" 47408))
(define your-address
  (make-address "700 N Woodlawn Ave" "Room 0121" "Bloomington" 47408))
(define his-address
  (make-address "1600 Pennsylvania Ave" "The White House" "Washington" 20500))
; process-address: Address -> ...
; ...
(define (process-address a)
  (... ((address-street a) ...)
       ((adress-apartment a) ...)
       ((address-city a) ...)
       ((address-zip a) ...)))

; (check-expect (process-address my-address)..)

; indiana?: address -> boolean
; Takes in an address and tells if it the zipcode is in Indiana (46000 to 47999)
(define (indiana? a)
  (cond
    [(and (<= 46000 (address-zip a)) (> 48000 (address-zip a))) (boolean? #t)]
    [else (boolean? 't)]))

(check-expect (indiana? my-address)(boolean? #t))
(check-expect (indiana? your-address)(boolean? #t))
(check-expect (indiana? his-address)(boolean? 't))

; format-address: Address -> string
; takes in an address and returns a string with the entire address in order
(define (format-address a)
  (string-append (address-street a)", " (address-apartment a)", " (address-city a)
                 "-" (number->string(address-zip a))))

(check-expect
 (format-address my-address)  "1900 E 10th Street, Eigenmenn Hall 745, Bloomington-47408")
(check-expect
 (format-address your-address)  "700 N Woodlawn Ave, Room 0121, Bloomington-47408")
(check-expect
 (format-address his-address)  "1600 Pennsylvania Ave, The White House, Washington-20500")


; smaller-zip: Address Address -> Address
; Takes in 2 addresses and sees which one has a smaller zip and prints that address
(define (smaller-zip a1 a2)
  (cond
    [(>= (address-zip a1) (address-zip a2)) a1]
    [else a2]))

(check-expect (smaller-zip my-address your-address) my-address)
(check-expect (smaller-zip my-address his-address) my-address)
(check-expect (smaller-zip his-address your-address) your-address)


(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

(define background1 (empty-scene 200 20))
(define cursor (rectangle 1 20 "solid" "red"))

; render: Editor -> Image
; Displays the editor text and cursor on an empty scene
(define (render e)
  (overlay (beside (beside (text (editor-pre e) 11 "black")cursor)(text (editor-post e) 11 "black"))
           background1))

(check-expect (render (make-editor "Hello " "World")) (overlay (beside
                                                                (beside (text "Hello " 11 "black")
                                                                        cursor)
                                                                (text "World" 11 "black"))
           background1))

(check-expect (render (make-editor "Hey " "there")) (overlay (beside
                                                              (beside (text "Hey " 11 "black")
                                                                      cursor)
                                                              (text "there" 11 "black"))
           background1))


; edit: Editor KeyEvent -> Editor
; Takes in a keyevent from the keyboard and changes the editor as per the key pressed
(define (edit ed ke)
  (cond
    [(string=? ke "left") (cond
                            [(> (string-length (editor-pre ed)) 0)
                             (make-editor
                              (substring (editor-pre ed) 0 (- (string-length (editor-pre ed)) 1))
                              (string-append (substring (editor-pre ed)
                                                        (- (string-length (editor-pre ed)) 1))
                                             (editor-post ed)))]
                            [else (make-editor (editor-pre ed)(editor-post ed))])]                          
    [(string=? ke "right") (cond
                            [(> (string-length (editor-post ed)) 0)
                                (make-editor
                                 (string-append (editor-pre ed)
                                                (substring (editor-post ed) 0 1))
                                 (substring (editor-post ed) 1 (string-length (editor-post ed))))]
                            [else (make-editor (editor-pre ed)(editor-post ed))])]
    [(or (string=? ke " ") (and (string<=? "A" ke)(string>=? "z" ke)))(make-editor
                                                                   (string-append (editor-pre ed) ke)
                                                                   (editor-post ed))]
    [(string=? ke "\b")(make-editor
                        (substring (editor-pre ed) 0 (- (string-length (editor-pre ed)) 1))
                        (editor-post ed))]
    [else (make-editor (editor-pre ed)(editor-post ed))]))

(check-expect (edit (make-editor "Hello " "World") "left")(make-editor "Hello" " World"))
(check-expect (edit (make-editor "Hello " "World") "right")(make-editor "Hello W" "orld"))
(check-expect (edit (make-editor "Hello " "World") "a")(make-editor "Hello a" "World"))
(check-expect (edit (make-editor "Hello World" "") "b")(make-editor  "Hello Worldb" ""))
(check-expect (edit (make-editor "Hello " "World") "Z")(make-editor  "Hello Z" "World"))
(check-expect (edit (make-editor "Hello " "World") "\r")(make-editor "Hello " "World"))
(check-expect (edit (make-editor "Hello " "World") "\t")(make-editor "Hello " "World"))
(check-expect (edit (make-editor "Hello " "World") "\b")(make-editor "Hello" "World"))
(check-expect (edit (make-editor "Hello World" "") "\b")(make-editor "Hello Worl" ""))
(check-expect (edit (make-editor "Hel" "lo World") "a")(make-editor "Hela" "lo World"))
(check-expect (edit (make-editor "Hello" " World") "\b")(make-editor "Hell" " World"))

; run: Editor -> Image
; launches an interactive text editor window
(define (run ed)
  (big-bang ed
    [to-draw render]
    [on-key edit]))


                        
  