#lang racket/base

;; Provides helpers for downloading lipics style files

(require file/md5
         file/untgz
         net/url
         racket/file
         racket/port)

(provide lipics-class-path
         lipics-cc-path
         lipics-logo-path
         download-lipics-files)

(define lipics-url  "http://drops.dagstuhl.de/styles/lipics/lipics-authors.tgz")
(define lipics-hash #"7338672a9c77897adc85df782a1d3bab")

(define lipics-base-path
  (build-path (find-system-path 'addon-dir) "lipics-style-files"))
(define lipics-class-path (build-path lipics-base-path "lipics.cls"))
(define lipics-cc-path (build-path lipics-base-path "cc-by.pdf"))
(define lipics-logo-path (build-path lipics-base-path "lipics-logo-bw.pdf"))

;; Download lipics class file to the add-on directory
(define (download-lipics-files)
  (unless (directory-exists? lipics-base-path)
    (define tmp (make-temporary-file))
    (displayln (format "Downloading class file via ~a" lipics-url))
    (define out (open-output-file tmp #:exists 'truncate))
    (call/input-url (string->url lipics-url)
                    get-pure-port
                    (λ (in) (copy-port in out)))
    (close-output-port out)
    (define hash
      (with-input-from-file tmp
        (λ () (md5 (current-input-port) #t))))
    (unless (equal? hash lipics-hash)
      (raise-arguments-error 'lipics
                             "Invalid MD5 hash for lipics tarball"
                             "expected" lipics-hash
                             "given" hash))
    ;; Don't make the directory until we have a valid download
    (make-directory lipics-base-path)
    (untgz tmp
           #:dest lipics-base-path
           #:strip-count 1
           #:filter (λ (path . _)
                      (regexp-match? #rx"lipics.cls$|cc-by.pdf$|logo-bw.pdf$"
                                     path)))))
