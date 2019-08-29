#lang sicp

; ex-2.66: lookup for sets as binary trees

(define (lookup given-key set-of-records)
  (if
   (null? set-of-records) false
   (let ((record (entry set-of-records)))
     (cond
       ((equal? given-key record) record)
       ((< given-key record) (lookup given-key (left-branch set-of-records)))
       ((> given-key record) (lookup given-key (right-branch set-of-records)))))))
