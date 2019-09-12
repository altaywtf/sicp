#lang racket

; explicit dispatch
; + good for the mvp
; - messy structure, unscalable

; data-directed
; + well structured, tracable, open to introduce new types/operations
; - verbose

; message passing
; + dynamic, powerful
; - not as tracable as data-directed

; new types      -> message passing
; new operations -> data directed
