#lang lipics

@title{Test}
@author[#:affil-no "1"]{Bob Arctor}
@author[#:affil-no "2"]{Bob Arctor, Jr.}
@affil[#:affil-no "1"]{ @; the blank lines are significant. yes, that's ugly
Some University

Some place, Some country

@tt|{bob@some.edu}|}

@affil[#:affil-no "1"]{
Other University

Other place, Some country

@tt|{bob@other.edu}|}

@; @abstract{
@; This is not a real abstract. It just plays one on TV.
@; }
@include-abstract{example-abstract.scrbl}

@section{Introduction}

Lorem ipsum.
