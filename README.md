# lipics-scribble

[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://pkg-build.racket-lang.org/doc/lipics/index.html)

This package implements a Racket language for writing papers using the
[LIPIcs](http://www.dagstuhl.de/publikationen/lipics/) paper format.

Installation
------------

You can install the package from the [package catalog](http://pkgs.racket-lang.org)
with the following command:

`raco pkg install lipics`

Usage
-----

Use `#lang lipics` as your language. The language will behave like the
`scribble/base` language with additional bindings for LIPIcs specific
typesetting.

See [`example.scrbl`](example.scrbl) for an example document in this style.

When the document is built, the style files for the LIPIcs format will
be downloaded from the website and untarred into your Racket add-on folder
(i.e., wherever `(find-system-path 'addon-dir)` points to).

---

Copyright (c) 2015 Asumu Takikawa, Vincent St-Amour

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with this program. If not, see http://www.gnu.org/licenses.
