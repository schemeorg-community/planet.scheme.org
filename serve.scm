#!/usr/bin/env gosh
;; Simple file server for previewing Planet Scheme output.

(use makiki)
(use file.util)

(define (main args)
  (let ((port (if (> (length args) 1)
                  (string->number (cadr args))
                  8080))
        (docroot (if (> (length args) 2)
                     (caddr args)
                     "output")))
    (format #t "Serving ~a on http://localhost:~a/~%" docroot port)
    (define-http-handler #/.*/ (file-handler :root docroot))
    (start-http-server :port port)))
