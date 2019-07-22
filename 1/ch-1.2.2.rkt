#|
 - https://stackoverflow.com/questions/27803152/sicp-example-counting-change-cannot-understand
 - https://github.com/trptcolin/sicp-study/blob/master/scheme/week-2/exercise-1.14.ss
 - http://telegraphics.com.au/~toby/sicp/ex1-14.svg
|# 

#lang sicp

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else
     (+
      (cc amount (- kinds-of-coins 1))
      (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond
    ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)
    )
  )

(count-change 10)