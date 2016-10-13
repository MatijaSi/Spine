#lang racket

(require "spinalhdl.rkt")
(require "auto-indent.rkt")

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

(define (display&indent-compile hw)
  (display (indent-compile hw)))

(define (multi-display&indent-compile lst)
  (map (lambda (hw) (begin
                      (display&indent-compile hw)
                      (newline)
                      "ok"))
       lst))

(define (indent-compile hw)
  (auto-indent (cond-compile hw)))

; test

(define and-gate (hw:2in-1out-bool-component "AND_GATE"
                                             (:= "io.c" '(& "io.a" "io.b"))))

(multi-display&indent-compile (list and-gate (hw:init-object and-gate)))