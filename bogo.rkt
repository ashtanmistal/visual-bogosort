;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname bogo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)
(require games/cards)
;==============================================================
;; DESIGNED BY: Ashtan Mistal
;; Tuesday, Feb 23, 2021

;; This project is intended to sort an arbitrary-sized list of numbers using the
;; Bogosort sorting algorithm. This sorting algorithm is not intended to be used
;; seriously, due to its almost amusing slowness especially in comparison to
;; other sorting algorithms. Below the sorting algorithm is a plot of the
;; algorithm, plotting the randomization until it is sorted using a slightly
;; modified algorithm allowing the plots. Call the sorting world with:
;;
;;                  (main (list ...))         i.e. (main LON1) or (main LON2)

;;, where (list ...) is any list of numbers less than 13 in length. If you want
;; to plot lists that are longer than 13 in length, decrease BAR-WIDTH to a
;; number that decreases it enough. For best results (aka a successful sort), I
;; would recommend choosing a list of 5 or less in length. Ensure the numbers in
;; the list are kept small to prevent too large of bars. Else, change Y-SCALE.
;;
;; Negative numbers are permitted; the magnitude will be the bar height.
;;
;; Special thanks to Dr. K during Feb 23 Office Hours for her initial help.
;; The bar chart code is derived from the additional practice problem Ref P1,
;; which is the Tuition bar graph chart function.
;;
;;
;; Potential improvements:
;; - Making it such that WIDTH and HEIGHT are fixed, by changing bar width to be
;;   (WIDTH / length of original list) and similarly with HEIGHT.
;; - Adding a counter of the current number of iterations displayed at the top.
;; - Making it such that when the program is complete, the bar color changes.
;; - Need to add check-expects for world function (This is bad code otherwise!!)
;==============================================================
(@htdd ListOfNumber)
;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; interp. a list of numbers

(@dd-template-rules one-of              ; 2 cases
                    atomic-distinct     ; empty
                    atomic-non-distinct ; Number
                    ref                 ; (first lon) is Number
                    self-ref)           ; (rest lon) is ListOfNumber

(define (fn-for-lon lon)
  (cond [(empty? lon)(...)]
        [else
         (... (fn-for-number (first lon))
              (fn-for-lon (rest lon)))]))

;==============================================================
;; Planning:
(define LON1 (list 1 6 7 4 3 2 9 8 5))
(define LON2 (list 75 42 58 8 43))
;(shuffle-list LON1 10)
; We need a function to check if an array is sorted or not:
; We need to iteravely go through the list and check if the one that is after
; it is larger; if it is not larger, return false
; (if (> (first (lon)) (first (rest lon)))
;     false
;     (is-sorted? (rest lon)))
;
; We will need to check the following:
; - An empty list is sorted
; - A list of one item is sorted
; empty
; one item
;(list 1 2 3 4 5 6 7 8 9)
;==============================================================
(@htdf bogo-sort)
(@signature ListOfNumber -> ListofNumber)

(check-expect (bogo-sort (list 3 4 2)) (list 2 3 4))
(check-expect (bogo-sort empty) empty)
(check-expect (bogo-sort (list 2)) (list 2))
(check-expect (bogo-sort (list 5 6 7 8)) (list 5 6 7 8))
(check-expect (bogo-sort (list 2 1)) (list 1 2))

(define (bogo-sort lon)
  (cond [(empty? lon) lon]
        [else (if (list-sorted? lon)
                  lon
                  (bogo-sort (shuffle-list lon 10)))]))

(@htdf list-sorted?)
(@signature ListOfNumber -> Boolean)
;; Checks if a list of arbitrary size
(check-expect (list-sorted? (list 1 2 3 4)) true)
(check-expect (list-sorted? (list 1)) true)
(check-expect (list-sorted? empty) true)
(check-expect (list-sorted? (list 4 3 2 1)) false)


(define (list-sorted? lon)
  (cond [(empty? lon) true]
        [else
         (if (first-is-min? (first lon) (rest lon))
             (list-sorted? (rest lon))
             false)]))

(@htdf first-is-min?)
(@signature Number ListOfNumber -> Boolean)
;; Returns true if n is the smallest number in a list

(check-expect (first-is-min? 3 (list 4 5 6 7)) true)
(check-expect (first-is-min? 6 (list 2 6 2 3 56 8)) false)
(check-expect (first-is-min? 7 empty) true)

(define (first-is-min? n lon)
  (cond [(empty? lon) true]
        [else
         (if (<= n (first lon))
             (first-is-min? n (rest lon))
             false)]))
;==============================================================
;; Plotting the Sorting
;; CONSTANTS:
(define FONT-SIZE 24)
(define FONT-COLOR "black")

(define WIDTH  605)
(define HEIGHT 535)
(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

(define Y-SCALE   8)  ; This could be changed as per max element of list   (?)
(define BAR-WIDTH 150) ; This could be changed as per number of list elements(?)
(define BAR-COLOR "Light Sky Blue")

;==============================================================

(@htdf main)
(@signature ListOfNumber -> ListOfNumber)
;; start the game, call with (main LON1) or any other list
;; <no tests for main functions>

(@template htdw-main)

(define (main lon)
  (big-bang lon
    (on-draw   render-numbers)     ;ListOfNumber -> Image
    (on-tick   next-iteration)     ;ListOfNumber -> ListOfNumber
    ))


(@htdf render-numbers)
(@signature ListOfNumber -> Image)

(@template ListOfNumber)

(define (render-numbers lon)
  (cond [(empty? lon) (square 0 "solid" "white")]
        [else
         (beside/align
          "bottom" 
          (overlay/align "center" "bottom"
                         (rotate 0 (text (number->string (first lon))
                                          FONT-SIZE FONT-COLOR))
                         (rectangle BAR-WIDTH (* (abs (first lon)) Y-SCALE)
                                    "outline" "black")
                         (rectangle BAR-WIDTH (* (abs (first lon)) Y-SCALE)
                                    "solid" BAR-COLOR))
          (render-numbers (rest lon)))]))

(@htdf next-iteration)
(@signature ListOfNumber -> ListOfNumber)
;; We want to shuffle the list if it is not sorted. We do not want to fully sort
;; on each tick, so we do not need to use the natural recursion on each step. 
(@template ListOfNumber)

(define (next-iteration lon)
  (cond [(empty? lon) lon]
        [else (if (list-sorted? lon)
                  lon
                  (shuffle-list lon 10))]))
