#lang sicp

; ex-2.24: derive hieararchy for (list 1 (list 2 (list 3 4)))

(list 1 (list 2 (list 3 4)))
; (1 (2 (3 4)))

; node 1     -> 1
; node 2     -> (2 (3 4))
; node 2.1   -> 2
; node 2.2   -> (3 4)
; node 2.2.1 -> 3
; node 2.2.2 -> 4
