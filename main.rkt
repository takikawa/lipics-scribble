#lang racket/base

(require "download.rkt"
         (except-in scribble/base
                    author)
         (except-in scribble/core
                    paragraph)
         (except-in racket/class
                    abstract)
         scribble/private/defaults scribble/decode
         scribble/html-properties
         scribble/latex-properties
         setup/main-collects
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
         paragraph paragraph*
         volume-info
         lipics-style)

;; header mostly taken from the lipics sample article
(define (post-process doc)
  (add-defaults doc
                ;; FIXME: allow configuration
                (string->bytes/utf-8 #<<FORMAT
%% Scribble needs these options, so provide before lipics
\PassOptionsToPackage{usenames,dvipsnames}{color}
\documentclass[letterpaper,UKenglish]{lipics}
\usepackage{microtype}
\bibliographystyle{plain}

%% from http://tex.stackexchange.com/a/39418
\makeatletter
\newcommand{\dontusepackage}[2][]{%
  \@namedef{ver@#2.sty}{9999/12/31}%
  \@namedef{opt@#2.sty}{#1}}
\makeatother

%% Disable Scribble's default usepackage statements
\dontusepackage{ccaption}

%% Save array package's internals to restore later
\makeatletter
  \let\@tmpmkpream\@mkpream
  \let\@tmpaddamp\@addamp
\makeatother

%% Save the value of \rmdefault to restore
\let\tmprmdefault\rmdefault
FORMAT
)
                (collection-file-path "style.tex" "lipics")
                (list lipics-class-path
                      lipics-cc-path
                      lipics-logo-path)
                #f))

(define-syntax (--#%module-begin stx)
  (syntax-case stx ()
    [(_ ?e ...)
     (quasisyntax/loc stx
       (-#%module-begin doc post-process ()
                        ;; Download necessary style files, if needed
                        (download-lipics-files)
                        ?e ...))]))

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

(define (volume-info editors n-editors event volume issue starting-page-no)
  (make-multiarg-element (make-style "volumeinfo" '())
                         (map decode-content
                              (map list
                                   (list editors
                                         n-editors
                                         event
                                         volume
                                         issue
                                         starting-page-no)))))


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
  [keywords               "keywords"]
  [doi                    "DOI"]
  [series-logo            "serieslogo"]
  [event-short-name       "EventShortName"]
  [acknowledgments        "acknowledgments"])

(define-pre-title-wrappers
  [author-running "authorrunning"]
  [title-running  "titlerunning"])

(define-includer include-abstract "lipicsabstract")

;; Bibliography setup
(define autobib-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list
     (make-css-addition (abs "autobib.css"))
     (make-tex-addition (abs "autobib.tex")))))

(define bib-single-style (make-style "AutoBibliography" autobib-style-extras))
(define bibentry-style (make-style "Autobibentry" autobib-style-extras))
(define colbibnumber-style (make-style "Autocolbibnumber" autobib-style-extras))
(define colbibentry-style (make-style "Autocolbibentry" autobib-style-extras))

(define lipics-style
  (new
   (class object%
     (define/public (bibliography-table-style) bib-single-style)
     (define/public (entry-style) colbibentry-style)
     (define/public (disambiguate-date?) #f)
     (define/public (collapse-for-date?) #f)
     (define/public (get-cite-open) "[")
     (define/public (get-cite-close) "]")
     (define/public (get-group-sep) ", ")
     (define/public (get-item-sep) ", ")
     (define/public (render-citation date-cite i)
       (make-element
        (make-style "Thyperref" (list (command-extras (list (make-label i)))))
        (list (number->string i))))
     (define/public (render-author+dates author dates) dates)
     (define (make-label i)
       (string-append "autobiblab:" (number->string i)))
     (define/public (bibliography-line i e)
       ;; Output a single column table using \bibitem to match the
       ;; LIPIcs format.
       (list (make-nested-flow
              (make-style "bibitem" (list 'multicommand))
              ;; the string here doesn't matter because we let
              ;; autobib do cross-referencing, but it should be freshly
              ;; generated to avoid warnings
              (list (make-paragraph plain (list (symbol->string (gensym)))) e))))
     (super-new))))

;; TODO
;; - figure out listings
;; - make this work without sed
