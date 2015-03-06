#lang info

(define collection "lipics")

(define deps '("base" "scribble-lib" "at-exp-lib"))
(define build-deps '("racket-doc" "scribble-doc"))

(define scribblings '(("lipics.scrbl")))

(define pkg-desc "Port of the lipics style to Scribble")
(define pkg-authors '(asumu stamourv))
