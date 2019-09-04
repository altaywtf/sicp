#lang racket
(require "./ch-2.3.4.rkt")

; ex-2.70 -> encode / decode symbols of alphabet.
; - generate tree, encode message.
; - how many bits are required for the encoding?
; - what is the smallest number of bits needed to encode this message if we used a fixed-length code for the alphabet

(define alphabet
  '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define lyrics
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))


; huhuhuhuhfman-tree
(define huffman-tree (generate-huffman-tree alphabet))


; # of bits
(length (encode lyrics huffman-tree))
