#lang racket

; code generator generator
(provide template% generate-compile)

; template superclass
(define template% (class object%
                    (init open
                          inside
                          close
                          type)
                    
                    ; fields
                    (define open-content open)
                    (define inside-content inside)
                    (define close-content close)
                    (define type-field type)
                    
                    (super-new)
                    
                    ; accessors
                    (define/public (get-open-content)
                      open-content)
                    (define/public (get-inside-content)
                      inside-content)
                    (define/public (get-close-content)
                      close-content)
                    (define/public (type?)
                      type-field)))


; generate function which will compile given template into string
(define (generate-compile opening insiding closing) ; args are functions which return strings
  (lambda (template)
    (string-append (opening (send template get-open-content))
                   (insiding (send template get-inside-content))
                   (closing (send template get-close-content)))))
            