;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Queens puzzle solver
;;
;; A queens board is a 4x4 grid of SQUARES.
;; A queen attacks any other queen at the same row/column/diagonal.
;;
;; The idea of the game is to have 4 queens on a board with no of
;; them attacking each other.



;;=======================
;;Data Definition:

;; Board is (listof 0|1) that is 16 elements long
;; interp.
;;  A board is a falt list of 16 0s and/or 1s, 0 means the square
;;  is empty, 1 means there is a queen at that square

;; Pos is Natural[0, 15]
;; interp.
;;  the position of a square of the board, for a given p, then
;;  - the row    is (quotient p 4)
;;  - the column is (remainder p 4)

;;Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 4) c))

;; Unit is (listof Pos)
;; interp.
;;  The position of every square in a unit. There are 18 of these
;;  for the 4 rows, 4 columns and 10 diagonals(squares in four
;;  corners are not conisdered as diagonals)


;;===========================
;; Constants

(define BD0      
  (list 0 0 0 0
        0 0 0 0
        0 0 0 0
        0 0 0 0))

(define BD1       ;no solution??
  (list 1 0 0 0
        0 0 0 0
        0 0 0 0
        0 0 0 0))

(define BD2
  (list 0 1 0 0
        0 0 0 0
        0 0 0 0
        0 0 0 0))

(define BD3
  (list 0 0 1 0
        0 0 0 0
        0 0 0 0
        0 0 0 0))

(define BD2S
  (list 0 1 0 0
        0 0 0 1
        1 0 0 0
        0 0 1 0))

(define BD3S
  (list 0 0 1 0
        1 0 0 0
        0 0 0 1
        0 1 0 0))

;; Positions of all the rows, columns and diagonals:

(define ROWS
  (list (list  0  1  2  3)
        (list  4  5  6  7)
        (list  8  9 10 11)
        (list 12 13 14 15)))

(define COLS
  (list (list  0  4  8 12)
        (list  1  5  9 13)
        (list  2  6 10 14)
        (list  3  7 11 15)))

(define DIAS
  (list (list  1  4)
        (list  2  5  8)
        (list  3  6  9 12)
        (list  7 10 13)
        (list 11 14)
        (list  2  7)
        (list  1  6 11)
        (list  0  5 10 15)
        (list  4  9 14)
        (list  8 13)))

(define UNITS (append ROWS COLS DIAS))

;; number of queens to place on board [1, 4]
;; NOTE: change of SIZE will fill some tests, but solver
;;       still gives the corret solution.
(define SIZE 4) 





;;====================
;; Functions:
;; Board -> Board or false
;; produce a solution for bd; or false if bd is unsolvable
;; Assume:: bd is valid
(check-expect (solve BD1) false)
(check-expect (solve BD2) BD2S)
(check-expect (solve BD3) BD3S)

;(define (solve bd) false);stub

(define (solve bd)
  (local [(define (solve--bd bd)
            (if (solved? bd)
                bd
                (solve--lobd (next-boards bd))))
          (define (solve--lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try (solve--bd (first lobd)))]
                     (if (not (false? try))
                         try
                         (solve--lobd (rest lobd))))]))]
    (solve--bd bd)))



;; Board -> Boolean
;; produce true if board is solved
;; Assume: board is valid, so if a board sums to 4 then it's solved
(check-expect (solved? BD1) false)
(check-expect (solved? BD2) false)
(check-expect (solved? BD2S) true)

;(define (solved? bd) false) ;stub
 
(define (solved? bd)
  (local [(define sum (foldr + 0 bd))]
    (= sum SIZE)))



;; Board -> (listof Board)
;; produce list of valid next boards from given board
;; finds all 0 squares, fills each with 1, keeps only valid boards 
(check-expect (next-boards BD1)
              (list (place-queen BD1 (r-c->pos 1 2))
                    (place-queen BD1 (r-c->pos 1 3))
                    (place-queen BD1 (r-c->pos 2 1))
                    (place-queen BD1 (r-c->pos 2 3))
                    (place-queen BD1 (r-c->pos 3 1))
                    (place-queen BD1 (r-c->pos 3 2))))

;(define (next-boards bd) (list bd));stub

(define (next-boards bd)
  (local [(define all-boards (fill-board bd))]
    (filter valid-board? all-boards)))



;; Board -> (listof Board)
;; produce list of board with each 0 square filled with 1
(check-expect (fill-board BD1)
              (list (place-queen BD1 1)
                    (place-queen BD1 2)
                    (place-queen BD1 3)
                    (place-queen BD1 4)
                    (place-queen BD1 5)
                    (place-queen BD1 6)
                    (place-queen BD1 7)
                    (place-queen BD1 8)
                    (place-queen BD1 9)
                    (place-queen BD1 10)
                    (place-queen BD1 11)
                    (place-queen BD1 12)
                    (place-queen BD1 13)
                    (place-queen BD1 14)
                    (place-queen BD1 15)))

;(define (fill-board bd) (list bd));stub

(define (fill-board bd)
  (local [(define (fill-square n)
            (place-queen bd n))
          
          (define board-list
            (build-list 16 fill-square))
          
          (define (new-board? x)
            (not (equal? bd x)))]
    
    (filter new-board? board-list)))
          


;; Board -> Boolean
;; produce true if a board is vaild(each unit sums to less than 2);
;; false otherwise
(check-expect (valid-board? BD1) true)
(check-expect (valid-board? BD2) true)
(check-expect (valid-board? (place-queen BD1 1)) false)
(check-expect (valid-board? (place-queen BD1 4)) false)
(check-expect (valid-board? (place-queen BD1 5)) false)
(check-expect (valid-board? (place-queen BD1 6)) true)

;(define (valid-board? bd) false);stub

(define (valid-board? bd)
  (local [(define (valid-units? lou)
            (andmap valid-unit? lou))
          
          (define (valid-unit? u)
            (<= (foldr + 0 (map read-pos u)) 1))
          
          (define (read-pos n)
            (list-ref bd n))]
    
    (valid-units? UNITS)))
 


;; Board Pos -> Board
;; produce a board with a queen placed at the given position p
(check-expect (place-queen BD0 0) BD1)
(check-expect (place-queen BD0 1) BD2)
(check-expect (place-queen BD1 2) (cons 1 (rest BD3)))

;(define (place-queen bd p) bd);stub

(define (place-queen bd p)
  (if (zero? p)
      (cons 1 (rest bd))
      (cons (first bd) (place-queen (rest bd) (sub1 p)))))