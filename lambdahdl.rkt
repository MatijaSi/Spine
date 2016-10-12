#lang racket

(require "spinalhdl.rkt")

; higher level, builds upon wrappers

(define (hw:wires names dir type)
  (map (lambda (name) (wire-generate name dir type)) names))

(define (hw:component name wires logic) ; generate component with name and wires in io bundle. Logic is string.
  (let ((bundle   (bundle-generate "io" wires))
        (logicbox (spinalbox-generate logic)))
    (component-generate name (list bundle logicbox))))

(define (:= wire expr) ; out is "val wire := expr"
  (let ((right-part expr))
    (if (list? expr)
        (set! expr (hw:logic expr))
        #t)
    (string-append "val " wire " := " expr "\n")))

(define (hw:logic three-lst) ; convert (+ "a" "b") to "a + b"
  (string-append (cadr three-lst) " " (symbol->string (car three-lst)) " " (caddr three-lst)))

(define (hw:init-object component) ; generate init object from component
  (let ((name (send component get-open-content)))
    (object-generate name
                     (list (scala-function-generate "main"
                                                    "args: Array[String]"
                                                    (list (spinalbox-generate (string-append "SpinalVhdl(new "
                                                                                             name
                                                                                             ")"))))))))

(define (hw:2in-1out-bool-component name logic)
  (hw:component name (cons (wire-generate "c" "out" "Bool") (hw:wires '("a" "b") "in" "Bool")) logic))

(define (display&cond-compile hw)
  (display (cond-compile hw)))

(define (multi-display&cond-compile lst)
  (map (lambda (hw) (begin
                      (display&cond-compile hw)
                      (newline)
                      "ok"))
       lst))
        
; test

(define and-gate (hw:2in-1out-bool-component "AND_GATE"
                                             (:= "io.c" '(& "io.a" "io.b"))))

(multi-display&cond-compile (list and-gate (hw:init-object and-gate)))