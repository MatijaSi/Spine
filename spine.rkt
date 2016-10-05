#lang racket

; SPINE - a simple SpinalHDL generator

(struct source-file (comment content-list)      #:mutable)
(struct component   (name comment content-list) #:mutable)
(struct bundle      (name comment content-list) #:mutable)
(struct wire        (name comment dir type)     #:mutable)
(struct spinalbox   (comment content)           #:mutable)

(define (commentify comment) ; appends /* ... */ to string
  (if (equal? comment "")
      comment
      (string-append "/* " comment " */\n")))

(define (compile-source source)
  (cond ((spinalbox?   source) (compile-spinalbox   source))
        ((wire?        source) (compile-wire        source))
        ((bundle?      source) (compile-bundle      source))
        ((component?   source) (compile-component   source))
        ((source-file? source) (compile-source-file source))
        (#t source)))

(define (compile-spinalbox source) ; turns spinalbox struct into string
  (string-append (commentify (spinalbox-comment source))
                 (apply string-append
                        (map (lambda (str) (string-append str "\n"))
                             (spinalbox-content source)))))

(define (compile-wire source) ; turns wire struct into string
  (string-append (commentify (wire-comment source))
                 "val " (wire-name source)
                 " = "
                 (wire-dir source) " " (wire-type source)
                 "\n"))

(define (compile-bundle source)
  (string-append (commentify (bundle-comment source))
                 "val " (bundle-name source)
                 " = new Bundle {\n"
                 (apply string-append
                        (map compile-source (bundle-content-list source)))
                 "}\n"))

(define (compile-component source)
  (string-append (commentify (component-comment source))
                 "class " (component-name source) " extends Component {\n"
                 (apply string-append
                        (map compile-source (component-content-list source)))
                 "}\n"))

(define (compile-source-file source)
  (string-append "import spinal.core._\n\n"
                 (commentify (source-file-comment source))
                 "\n"
                 (apply string-append
                        (map compile-source (source-file-content-list source)))))

; test

(define io-bundle (bundle "io" "bundle for io" (list (wire "a" "input 1" "in"  "Bool")
                                                     (wire "b" "input 2" "in"  "Bool")
                                                     (wire "c" "output"  "out" "Bool"))))

(define and_gate (spinalbox "combinatorial logic" '("io.c := io.a & io.b")))

(define ag-component (component "AND_GATE" "simple_example" (list io-bundle and_gate)))

(define sourcefile (source-file "Test..." (list ag-component)))

(display (compile-source sourcefile))