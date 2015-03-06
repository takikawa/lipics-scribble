#lang scribble/manual

@(require (for-label lipics
                     scribble/decode))

@title{LIPIcs support for Scribble}

@(defmodule lipics #:lang)

The @racketmodname[lipics] language provides support for the
@(hyperlink "http://www.dagstuhl.de/publikationen/lipics/" "LIPIcs")
paper format.

The language provides all of the bindings of @racketmodname[scribble/base]
in addition to additional procedures for supporting LIPIcs-specific
typesetting.

If using the @racketmodname[scriblib/autobib] library, ensure that the
@racket[define-cite] form is used with the @racket[lipics-style] setting
for @racket[#:style].

Also see the
@(hyperlink "https://github.com/takikawa/lipics-scribble/blob/master/example.scrbl"
            "example document")
to get started.

@defproc[(author [#:affil-no affil-no pre-content?]
                 [name pre-content?] ...)
         paragraph?]{
  Registers an author name for the paper. Each use of @racket[author] should
  correspond to a single author on the paper.

  The @racket[affil-no] argument typesets as a superscript indicating the
  institution (typeset using @racket[affil]) the author is affiliated with.
}

@defproc[(affil [#:affil-no affil-no pre-content?]
                [name pre-content?] ...)
         paragraph?]{
  Typesets the name, location, and contact information for an institution.
}

@defproc[(abstract [pre-content pre-content?] ...) element?]{
  Typesets an abstract.
}

@defproc[(paragraph [pre-content pre-content?] ...) element?]{
  Typesets a paragraph.
}

@defproc[(paragraph* [pre-content pre-content?] ...) element?]{
  Typesets a paragraph.
}

@defproc[(subject-classification [pre-content pre-content?] ...)
         element?]{
  Typesets the ACM subject classification entry.
}

@defproc[(volume-info [editors pre-content?]
                      [n-editors pre-content?]
                      [event pre-content?]
                      [volume pre-content?]
                      [issue pre-content?]
                      [starting-page-no pre-content?])
         multiarg-element?]{
  Typesets the volume information.
}

@defproc[(copyright [pre-content pre-content?] ...)
         element?]{
  Typesets the copyright information.
}

@defproc[(keywords [pre-content pre-content?] ...)
         element?]{
  Typesets the keywords and phrases for the paper.
}

@defproc[(doi [pre-content pre-content?] ...)
         element?]{
  Typesets the DOI.
}

@defproc[(event-short-name [pre-content pre-content?] ...)
         element?]{
  Typesets the short name displayed in the corner of the page.
}

@defproc[(acknowledgments [pre-content pre-content?] ...)
         element?]{
  Typesets an acknowledgments section.
}

@defproc[(author-running [pre-content pre-content?] ...)
         paragraph?]{
  Typesets the author list shown at the top of pages.
}

@defproc[(title-running [pre-content pre-content?] ...)
         paragraph?]{
  Typesets the title shown at the top of pages.
}

@defthing[lipics-style any/c]{
  A value used to customize bibliography generation for the paper.
  Provide this as an argument for the @racket[define-cite] form using
  the @racket[#:style] keyword.
}
