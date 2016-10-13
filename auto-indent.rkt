#lang racket

(provide auto-indent)

(define (add-indentation space-num str)
  (if (equal? space-num 0)
      str
      (string-append (make-string space-num #\space) str)))

(define (is-member? el lst) ; return #t if el is in lst, else #f
  (cond ((equal? lst '()) #f)
        ((equal? el (car lst)) #t)
        (#t (is-member? el (cdr lst)))))

; split the string by newlines, indent each line by ind-level spaces.
; ind-level increments if it encounters "{" and decrements if it encounters "}"
(define (auto-indent spinal-str) ; spinal-str is string
  (define ind-level 0)
  (define (func str)
    (if (is-member? "}" (string-split str))
        (set! ind-level (- ind-level 2))
        ind-level)
    (define return (add-indentation ind-level str))
    (if (is-member? "{" (string-split str)) 
        (set! ind-level (+ ind-level 2))
        ind-level)
    (string-append return "\n"))
  (apply string-append (map func (regexp-split "\n" spinal-str))))

; test
;(display (auto-indent "abc {\n cdb\n bdc {\n bcd\n }\n}\n"))