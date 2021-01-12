(import (scheme base) (scheme char) (scheme file) (scheme write))
(import (srfi 1) (srfi 115))
(cond-expand
  (gauche (import (rfc http) (rfc sha) (rfc uri))
          (import (only (gauche base)
                        make
                        make-keyword
                        port->string
                        sys-basename))
          (import (only (file util)
                        path-extension))))

(define (disp . xs) (for-each display xs) (newline))

(define (edisp . xs)
  (parameterize ((current-output-port (current-error-port)))
    (apply disp xs)))

(define (map/odd f xs)
  (let loop ((acc '()) (xs xs) (odd? #f))
    (if (null? xs) (reverse acc)
        (loop (cons (f (car xs) odd?) acc) (cdr xs) (not odd?)))))

(define (sha1 in)
  (let ((digest (make <sha1>)))
    (let loop ()
      (let ((bytes (read-bytevector 4096 in)))
        (if (eof-object? bytes)
            (digest-hexify (digest-final! digest))
            (begin (digest-update! digest bytes)
                   (loop)))))))

(define url-regexp
  '(: "http" (? "s") "://"
      (+ (- graphic "\""))
      "." (or "jpeg" "jpg" "png" "svg")))

(define (download-http-uri-as-bytevector uri)
  (call-with-port
   (open-output-bytevector)
   (lambda (out)
     (http-get (uri-ref uri 'host+port)
               (uri-ref uri 'path+query)
               (make-keyword 'secure)  (string-ci=? "https"
                                                    (uri-ref uri 'scheme))
               (make-keyword 'sink)    out
               (make-keyword 'flusher) (lambda _ #t))
     (get-output-bytevector out))))

(define (download uri)
  (edisp "Downloading " uri)
  (guard (_ (else ""))
    (let* ((bytes (download-http-uri-as-bytevector uri))
           (sha (call-with-port (open-input-bytevector bytes) sha1))
           (orig-ext (path-extension (sys-basename (uri-ref uri 'path))))
           (ext (string-downcase
                 (if (string=? orig-ext "jpg") "jpeg" orig-ext)))
           (sha-ext (string-append "images/cache/" sha "." ext)))
      (edisp "Saving into " sha-ext)
      (call-with-port (open-binary-output-file sha-ext)
                      (lambda (out) (write-bytevector bytes out)))
      sha-ext)))

(let ((html-string (port->string (current-input-port))))
  (write-string
   (fold-right string-append ""
               (map/odd (lambda (s odd?) (if odd? (download s) s))
                        (regexp-partition url-regexp html-string)))))
