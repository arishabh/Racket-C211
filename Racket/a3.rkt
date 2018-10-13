;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
; A Year is a non-negative integer
; Examples:
;   0
;   1789
;   2018
; Non-examples:
;   -5000
;   "AD 2018"
 
; A Month is one of:
; - "January"
; - "February"
; - "March"
; - "April"
; - "May"
; - "June"
; - "July"
; - "August"
; - "September"
; - "October"
; - "November"
; - "December"
 
; A Day is an integer at least 1 but at most 31
; Examples:
;   1
;   10
;   31
; Non-examples:
;   32
;   "today"

; next-month : Month -> Month
; returns the month that comes after the given one
(define (next-month m)
  (cond
    [(string=? m "January") "Febuary"]
    [(string=? m "Febuary") "March"]
    [(string=? m "March") "April"]
    [(string=? m "April") "May"]
    [(string=? m "May") "June"]
    [(string=? m "June") "July"]
    [(string=? m "July") "August"]
    [(string=? m "August") "September"]
    [(string=? m "September") "October"]
    [(string=? m "October") "November"]
    [(string=? m "November") "December"]
    [(string=? m "December") "January"]))
 
(check-expect (next-month "September") "October")
(check-expect (next-month "December") "January")

; fall? : Month -> Boolean
; decides whether the given month is between September and November

(define (fall? m)
  (cond
    [(string=? m "January") (boolean? 'false)]
    [(string=? m "Febuary") (boolean? 'false)]
    [(string=? m "March") (boolean? 'false)]
    [(string=? m "April") (boolean? 'false)]
    [(string=? m "May") (boolean? 'false)]
    [(string=? m "June") (boolean? 'false)]
    [(string=? m "July") (boolean? 'false)]
    [(string=? m "August") (boolean? #t)]
    [(string=? m "September") (boolean? #t)]
    [(string=? m "October") (boolean? #t)]
    [(string=? m "November") (boolean? #t)]
    [(string=? m "December") (boolean? 'false)]))

(check-expect (fall? "September") true)
(check-expect (fall? "December") false)

; A MonthFormat is one of:
; - "long"
; - "short"
 
; A DateOrder is one of:
; - "MDY"
; - "DMY"

; format-month : Month MonthFormat -> String
; abbreviates Month to three letters or not
(define (format-month m f)
  (cond
    [(string=? f "short")(substring m 0 3)]
    [(string=? f "long")m]))  

(check-expect (format-month "November" "long") "November")
(check-expect (format-month "November" "short") "Nov")

; year-month-day->date : Year Month Day DateOrder MonthFormat -> String
; produces a date as a string
; given: "1936" "November" "12" "MDY" "long"   expect: "November 12, 1936"
; given: "1936" "November" "12" "MDY" "short"  expect: "Nov 12, 1936"
; given: "1936" "November" "12" "DMY" "long"   expect: "12 November 1936"
; given: "1936" "November" "12" "DMY" "short"  expect: "12 Nov 1936"
(define (year-month-day->date y m d o f)
  (cond
    [(string=? o "MDY") (string-append (format-month m f) " " d ", " y)]
    [(string=? o "DMY") (string-append d " " (format-month m f) " " y)]
    [else "Invalid Input!"]))

(check-expect (year-month-day->date "2018" "November" "23" "MDY" "short") "Nov 23, 2018")
(check-expect (year-month-day->date "1994" "October" "6" "DMY" "long") "6 October 1994")
(check-expect (year-month-day->date "1856" "January" "13" "DMY" "short") "13 Jan 1856")
(check-expect (year-month-day->date "1856" "January" "13" "YMD" "less") "Invalid Input!")

; month->days-in-year : Month -> Number
; returns the days elapsed in the year before the given month
; given: "January"    expect: 0
; given: "September"  expect: 243

(define(month->days-in-year m)
  (cond
    [(string=? m "January") 0]
    [(string=? m "Febuary") 31]
    [(string=? m "March") 59]
    [(string=? m "April") 90]
    [(string=? m "May") 120]
    [(string=? m "June") 151]
    [(string=? m "July") 181]
    [(string=? m "August") 212]
    [(string=? m "September") 243]
    [(string=? m "October") 273]
    [(string=? m "November") 304]
    [(string=? m "December") 334]))

(check-expect (month->days-in-year "January") 0)
(check-expect (month->days-in-year "September") 243)
(check-expect (month->days-in-year "December") 334)

; year-month-day->days: Year Month Day -> Number
; Takes in a date and tells the number of days passed from Jan 1, 0000
(define (year-month-day->days y m d)
  (+ (- d 1) (month->days-in-year m) (* 365 y)))

(check-expect (year-month-day->days 1 "January" 26) 390)
(check-expect (year-month-day->days 2000 "November" 26) 730329)

; days-between : Year Month Day Year Month Day -> Number
; Takes in 2 dates and tells the number of days between them
(define (days-between y1 m1 d1 y2 m2 d2)
  (cond
    [(> (year-month-day->days y1 m1 d1)(year-month-day->days y2 m2 d2))
     (- (year-month-day->days y1 m1 d1)(year-month-day->days y2 m2 d2))]
    [else (- (year-month-day->days y2 m2 d2)(year-month-day->days y1 m1 d1))]))

(check-expect (days-between 2018 "January" 5 2018 "January" 9) 4)
(check-expect (days-between 2008 "January" 5 2018 "January" 9) 3654)

; days->year : Number -> Year
; takes days since 1 Jan 0 and returns the year
; given: 364                                       expect: 0
; given: 365                                       expect: 1
; given: 736305                                    expect: 2017
; given: (year-month-day->days 1999 "December" 31) expect: 1999
(define(days->year d)
  (floor (/ d 365)))

(check-expect (days->year 364) 0)
(check-expect (days->year 365) 1)
(check-expect (days->year 736305) 2017)
(check-expect (days->year (year-month-day->days 1999 "December" 31)) 1999)

; DaysInYear is one of:
; - an integer at least   0 but less than  31
; - an integer at least  31 but less than  59
; - an integer at least  59 but less than  90
; - an integer at least  90 but less than 120
; - an integer at least 120 but less than 151
; - an integer at least 151 but less than 181
; - an integer at least 181 but less than 212
; - an integer at least 212 but less than 243
; - an integer at least 243 but less than 273
; - an integer at least 273 but less than 304
; - an integer at least 304 but less than 334
; - an integer at least 334 but less than 365
; *Interpretation*: the number of elapsed days
;                   since the first day of the year

(define (diy d)
  (- d (* 365 (floor (/ d 365)))))

; days->month : Number -> Month
; takes days since 1 Jan 0 and returns the month
; given: 59                                        expect: "March"
; given: 364                                       expect: "December"
; given: 736445                                    expect: "August"
; given: (year-month-day->days 1999 "December" 31) expect: "December"

(define (days->month d)
  (cond
    [(and (>= (diy d) 0) (<(diy d) 31))"January"]
    [(and (>= (diy d) 31) (< (diy d) 59))"Febuary"]
    [(and (>= (diy d) 59) (< (diy d) 90))"March"]
    [(and (>= (diy d) 90) (< (diy d) 120))"April"]
    [(and (>= (diy d) 120) (< (diy d) 151))"May"]
    [(and (>= (diy d) 151) (< (diy d) 181))"June"]
    [(and (>= (diy d) 181) (< (diy d) 212))"July"]
    [(and (>= (diy d) 212) (< (diy d) 243))"August"]
    [(and (>= (diy d) 243) (< (diy d) 273))"September"]
    [(and (>= (diy d) 273) (< (diy d) 304))"October"]
    [(and (>= (diy d) 304) (< (diy d) 334))"November"]
    [(and (>= (diy d) 334) (< (diy d) 365))"December"]))

(check-expect (days->month 364) "December")
(check-expect (days->month 59) "March")
(check-expect (days->month 736445) "August")
(check-expect (days->month (year-month-day->days 1999 "December" 31)) "December") 


; days-in-year->days-in-month : DaysInYear -> DaysInMonth
; takes days since the first of the year
; and returns days since the first of the month
; given: 0       expect: 0
; given: 59      expect: 0
; given: 364     expect: 30

(define (days-in-year->days-in-month diy)
  (cond
    [(and (>= diy 0) (< diy 31))diy]
    [(and (>= diy 31) (< diy 59))(- diy 31)]
    [(and (>= diy 59) (< diy 90))(- diy 59)]
    [(and (>= diy 90) (< diy 120))(- diy 90)]
    [(and (>= diy 120) (< diy 151))(- diy 120)]
    [(and (>= diy 151) (< diy 181))(- diy 151)]
    [(and (>= diy 181) (< diy 212))(- diy 181)]
    [(and (>= diy 212) (< diy 243))(- diy 212)]
    [(and (>= diy 243) (< diy 273))(- diy 243)]
    [(and (>= diy 273) (< diy 304))(- diy 273)]
    [(and (>= diy 304) (< diy 334))(- diy 304)]
    [(and (>= diy 334) (< diy 365))(- diy 334)]))

(check-expect (days-in-year->days-in-month 0) 0)
(check-expect (days-in-year->days-in-month 59) 0)
(check-expect (days-in-year->days-in-month 364) 30)

; days->day : Number -> Day
; takes days since 1 Jan 0 and returns the day of the month
; given: 0                                         expect: 1
; given: 59                                        expect: 1
; given: 736324                                    expect: 30
; given: (year-month-day->days 1999 "December" 31) expect: 31

(define (days->day n)
  (+ 1 (days-in-year->days-in-month (diy n))))

(check-expect (days->day 0) 1)
(check-expect (days->day 59) 1)
(check-expect (days->day 736324) 30)
(check-expect (days->day (year-month-day->days 1999 "December" 31)) 31)


(define init-year 2000)
(define init-month "January")
(define init-day 1)
 
; init-time : Number
; days since init-month init-day, init-year
; given:  init-month := "December"
;         init-year := "1999"
;         init-day := 31
; expect: 729999
(define init-time (year-month-day->days 2000 "January" 1))
 
; time-passing : Number -> Image
; takes days t since 1 Jan 0, advances t by init-time
; and returns a calendar image of the corresponding date
(define background (rectangle 1100 400 "solid" "lightblue"))

(define (time-passing t)
  (place-image (text (number->string(days->day (+ init-time t))) 50 "yellow") 200 200
               (place-image (text (days->month (+ init-time t)) 50 "yellow") 500 200
                            (place-image (text
                                          (number->string (days->year (+ init-time t)))
                                          50 "yellow")
                                         900 200 background))))

(animate time-passing)




  


