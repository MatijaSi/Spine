#lang racket

(require "generic.rkt")

; general compile
(define (cond-compile template)
  (let ((template-type (send template type?)))
    (cond ((equal? 'wire           template-type) (wire-compile           template))
          ((equal? 'spinalbox      template-type) (spinalbox-compile      template))
          ((equal? 'comment        template-type) (comment-compile        template))
          ((equal? 'bundle         template-type) (bundle-compile         template))
          ((equal? 'component      template-type) (component-compile      template))
          ((equal? 'object         template-type) (object-compile         template))
          ((equal? 'scala-function template-type) (scala-function-compile template)))))

; spinalbox (gets pasted literary)

(define spinalbox-compile (generate-compile (lambda (str) (quote ""))
                                            (lambda (str) (string-append str "\n"))
                                            (lambda (str) (quote ""))))

(define (spinalbox-generate spinalbox)
  (new template% (open "") (inside spinalbox) (close "") (type 'spinalbox)))

; comment

(define comment-compile (generate-compile (lambda (str) (quote "/* "))
                                          (lambda (str) (string-append str " "))
                                          (lambda (str) (quote "*/\n"))))

(define (comment-generate content)
  (new template% (open "") (inside content) (close "") (type 'comment)))
                                            

; wire

(define wire-compile (generate-compile (lambda (str) (string-append "val " str " = "))
                                       (lambda (str) (string-append str " "))
                                       (lambda (str) (string-append str "\n"))))

(define (wire-generate name direction data-type)
  (new template% (open name) (inside direction) (close data-type) (type 'wire)))

; bundle

(define bundle-compile (generate-compile (lambda (str) (string-append "val " str " = new Bundle {\n"))
                                         (lambda (lst) (apply string-append (map cond-compile lst)))
                                         (lambda (str) (quote "}\n"))))
                                           
(define (bundle-generate name lst)
  (new template% (open name) (inside lst) (close "") (type 'bundle)))

; component

(define component-compile (generate-compile (lambda (str) (string-append "class " str " extends Component {\n"))
                                            (lambda (lst) (apply string-append (map cond-compile lst)))
                                            (lambda (str) (quote "}\n"))))

(define (component-generate name lst)
  (new template% (open name) (inside lst) (close "") (type 'component)))

; object

(define object-compile (generate-compile (lambda (str) (string-append "object " str " {\n"))
                                         (lambda (lst) (apply string-append (map cond-compile lst)))
                                         (lambda (str) (quote "}\n"))))

(define (object-generate name lst)
  (new template% (open name) (inside lst) (close "") (type 'object)))

; scala-function

(define scala-function-compile (generate-compile (lambda (d-pair) (string-append "def " (car d-pair) "(" (cadr d-pair) ") {\n"))
                                                 (lambda (lst) (apply string-append (map cond-compile lst)))
                                                 (lambda (str) (quote "}\n"))))

(define (scala-function-generate name args body)
  (new template% (open (list name args)) (inside body) (close "") (type 'scala-function)))
                                                 

; test

(define a (wire-generate "a" "in" "Bool"))
(define b (wire-generate "b" "in" "Bool"))
(define c (wire-generate "c" "out" "Bool"))
(define io (bundle-generate "io" (list a c b)))
(define logic (spinalbox-generate "io.c := io.a & io.b"))
(define import (spinalbox-generate "import spinal.core._"))
(define component (component-generate "AND_GATE" (list io logic)))
(define main-body (spinalbox-generate "SpinalVhdl(new AND_GATE)"))
(define main (scala-function-generate "main" "args: Array[String]" (list main-body)))
(define object (object-generate "AND_GATE" (list main)))

(display (apply string-append (map cond-compile (list import component object))))