#lang racket/base

(require (except-in scribble/base
                    author)
         (except-in scribble/core
                    paragraph)
         scribble/private/defaults scribble/decode
         setup/collects
         (rename-in scribble/doclang
                    [#%module-begin -#%module-begin])
         (for-syntax racket/base))

(provide (all-from-out scribble/base)
         (except-out (all-from-out scribble/doclang)
                     -#%module-begin)
         (rename-out [--#%module-begin #%module-begin])
         author
         affil
         abstract
         paragraph paragraph*)

;; header mostly taken from the lipics sample article
(define (post-process doc)
  (add-defaults doc
                ;; FIXME: allow configuration
                (string->bytes/utf-8 #<<FORMAT
\documentclass[letterpaper,UKenglish]{lipics}
\usepackage{microtype}
\bibliographystyle{plain}
FORMAT
)
                (collection-file-path "style.tex" "lipics")
                (list (collection-file-path "lipics.cls"
                                            "lipics"))
                #f))

(define-syntax (--#%module-begin stx)
  (syntax-case stx ()
    [(_ ?e ...)
     (quasisyntax/loc stx
       (-#%module-begin doc post-process () ?e ...))]))

;; Reader configuration for #lang
(module reader scribble/base/reader
  lipics
  #:wrapper1 (lambda (t) (t)))


;; TODO abstract between the two
(define (author #:affil-no [affil-no "1"] . name)
  (make-paragraph
   (make-style 'pretitle '())
   (make-multiarg-element (make-style "lipicsauthor" '())
                          (list (decode-content (list affil-no))
                                (decode-content name)))))
(define (affil #:affil-no [affil-no "1"] . name)
  (make-paragraph
   (make-style 'pretitle '())
   (make-multiarg-element (make-style "lipicsaffil" '())
                          (list (decode-content (list affil-no))
                                (decode-content name)))))


;; command wrappers
;; taken from classicthesis-scribble
(define-syntax-rule (define-wrappers (name style) ...)
  (begin
    (define (name . str)
      (make-element (make-style style '()) (decode-content str)))
    ...
    (provide name ...)))
(define-syntax-rule (define-pre-title-wrappers (name style) ...)
  (begin
    (define (name . str)
      (make-paragraph
       (make-style 'pretitle '())
       (make-multiarg-element
        (make-style style '())
        (decode-content str))))
    ...
    (provide name ...)))
(define-syntax-rule (define-includer name style)
  (begin
    (define-syntax (name stx)
      (syntax-case stx ()
        [(_ module)
         (let ()
           (define name* (gensym 'name))
           #'(begin
               (require (rename-in module [doc name*]))
               (make-nested-flow (make-style style '(command))
                                 (part-blocks name*))))]))
    (provide name)))

(define-wrappers
  [abstract               "lipicsabstract"]
  [paragraph              "paragraph"]
  [paragraph*             "paragraph*"]
  [copyright              "Copyright"]
  [subject-classification "subjclass"]
  [keywords               "keywords"])

(define-pre-title-wrappers
  [author-running "authorrunning"]
  [title-running  "titlerunning"])

(define-includer include-abstract "lipicsabstract")

;; TODO
;; - doi (and editor macros in general)
;; - test figures
;; - test bibliographies
;;   - ugh. will probably need to override Autobibentry and whatever else to just call their stuff
;;   - but how can we do the linking...
;;   - heck, maybe just do our own munging based on autobib's data structures...
;;     - yeah, probably easiest. I guess the `key` field of an `auto-bib` struct is the label...
;;     - then generate \bibitem s. like in their example
;; - test footnotes, including on the title
;; - acks
