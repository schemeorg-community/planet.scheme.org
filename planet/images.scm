(import (scheme base) (scheme char) (scheme file)
        (scheme read) (scheme write))
(import (srfi 1) (srfi 115) (srfi 193))
(cond-expand
  (gauche (import (rfc http) (rfc sha) (rfc uri))
          (import (only (gauche base)
                        make
                        make-keyword
                        port->string
                        pprint
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

(define uri-regexp
  '(: "http" (? "s") "://"
      (+ (- graphic "\""))
      "." (or "jpeg" "jpg" "png" "svg")))

(define (download-http-uri-as-bytevector uri)
  (call-with-port
   (open-output-bytevector)
   (lambda (out)
     (parameterize ((http-user-agent "https://planet.scheme.org/ images"))
       (http-get (uri-ref uri 'host+port)
                 (uri-ref uri 'path+query)
                 (make-keyword 'secure)  (string-ci=? "https"
                                                      (uri-ref uri 'scheme))
                 (make-keyword 'sink)    out
                 (make-keyword 'flusher) (lambda _ #t)))
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

(define cache-file (string-append (script-directory) "images-cache.scm"))

(define (load-cache)
  (if (file-exists? cache-file) (with-input-from-file cache-file read)
      '()))

(define (save-cache cache)
  (with-output-to-file cache-file (lambda () (pprint cache))))

(let ((cache (load-cache))
      (html-string (port->string (current-input-port))))
  (write-string
   (fold-right
    string-append ""
    (map/odd (lambda (s odd?)
               (if odd?
                   (let* ((uri s)
                          (pair (assoc uri cache))
                          (file (if pair (cdr pair) (download uri))))
                     (unless (or pair (string=? "" file))
                       (set! cache (cons (cons uri file) cache)))
                     file)
                   s))
             (regexp-partition uri-regexp html-string))))
  (save-cache cache))
