#!/usr/bin/env gosh
;; Planet Scheme — feed aggregator in Gauche Scheme

(use file.util)
(use gauche.parameter)
(use gauche.sequence)
(use rfc.http)
(use rfc.sha)
(use rfc.uri)
(use srfi.13)
(use srfi.19)
(use srfi.115)
(use sxml.ssax)
(use sxml.sxpath)
(use sxml.tools)
(use text.html-lite)
(use util.match)

(define (config-ref config key . default)
  (let ((pair (assq key (cdr config))))
    (cond (pair (cadr pair))
          ((pair? default) (car default))
          (else (error "Missing config key" key)))))

(define (config-feeds config)
  (let ((feeds-section (assq 'feeds (cdr config))))
    (if feeds-section
        (filter (lambda (x) (and (pair? x) (eq? (car x) 'feed)))
                (cdr feeds-section))
        '())))

(define (entry-ref entry key)
  (let ((pair (assq key entry)))
    (and pair (cdr pair))))

(define (feed-ref feed key . default)
  (let ((pair (assq key (cdr feed))))
    (cond (pair (cadr pair))
          ((pair? default) (car default))
          (else #false))))

;; Try multiple date formats common in RSS/Atom feeds.
(define date-formats
  '(;; RFC 822 / RFC 2822 variants
    "~a, ~d ~b ~Y ~H:~M:~S ~z"
    "~a, ~d ~b ~Y ~H:~M:~S GMT"
    "~d ~b ~Y ~H:~M:~S ~z"
    "~d ~b ~Y ~H:~M:~S GMT"
    ;; ISO 8601 / RFC 3339
    "~Y-~m-~dT~H:~M:~S~z"
    "~Y-~m-~dT~H:~M:~SZ"
    "~Y-~m-~d~H:~M:~S~z"
    "~Y-~m-~d ~H:~M:~S"
    "~Y-~m-~d"))

(define (parse-date string)
  (and (string? string)
       (let ((s (string-trim-both string)))
         (let loop ((formats date-formats))
           (and (not (null? formats))
		(guard (_ (else (loop (cdr formats))))
                  (string->date s (car formats))))))))

;; Fallback: extract just a date from an ISO-ish string for sorting.
(define (parse-date/loose string)
  (or (parse-date string)
      (and (string? string)
           (>= (string-length string) 10)
           (guard (_ (else #false))
             (string->date (substring string 0 10) "~Y-~m-~d")))))

(define (date->rfc822 date)
  (if date
      (date->string date "~a, ~d ~b ~Y ~H:~M:~S ~z")
      ""))

(define (date->iso8601 date)
  (if date
      (let ((s (date->string date "~Y-~m-~dT~H:~M:~S~z")))
        ;; SRFI 19 ~z produces e.g. "-0700", but RFC 3339 requires "-07:00".
        (regexp-replace #/([+-]\d{2})(\d{2})$/ s "\\1:\\2"))
      ""))

;; Convert strftime-style format (%A, %B, %d, etc.) to SRFI 19 (~A, ~B, ~d).
;; %-d (no padding) is handled by post-processing.
(define (strftime->srfi19 format)
  (regexp-replace-all #/%-?([A-Za-z%])/
		      format
                      (lambda (m)
                        (let ((c (rxmatch-substring m 1)))
                          (if (string=? c "%")
			      "~"
                              (string-append "~" c))))))

(define (date->display date format)
  (if date
      ;; SRFI 19 doesn't support %-d (no-pad), so we strip leading zeros
      ;; from the day field after formatting.
      (let ((has-nopad-day? (string-contains format "%-d"))
            (result (date->string date (strftime->srfi19 format))))
        (if has-nopad-day?
            (regexp-replace-all #/([ ,])0(\d)/ result
                                (lambda (m)
                                  (string-append (rxmatch-substring m 1)
                                                 (rxmatch-substring m 2))))
            result))
      ""))

(define (html-escape string)
  (if (string? string)
      (html-escape-string string)
      ""))

(define (fetch-feed-xml url)
  (guard (e (else
             (format (current-error-port) "Error fetching ~a: ~a~%" url e)
             #false))
    (let-values (((scheme user host port path query fragment)
                  (uri-parse url)))
      (let ((secure (and scheme (string-ci=? scheme "https")))
            (host+port (if port (format "~a:~a" host port) host))
            (path+query (if query (format "~a?~a" path query) path)))
        (let-values (((status headers body)
                      (parameterize ((http-user-agent "Planet Scheme/1.0 (https://planet.scheme.org/)"))
                        (http-get host+port path+query :secure secure))))
          (if (string-prefix? "2" status)
              body
              (begin
                (format (current-error-port)
                        "HTTP ~a fetching ~a~%" status url)
                #false)))))))

(define (safe-parse-xml string)
  (guard (e (else
             (format (current-error-port) "XML parse error: ~a~%" e)
             #false))
    (call-with-input-string string
      (lambda (port)
        (ssax:xml->sxml port '((atom . "http://www.w3.org/2005/Atom")
                               (content . "http://purl.org/rss/1.0/modules/content/")
                               (dc . "http://purl.org/dc/elements/1.1/")
                               (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                               (rss1 . "http://purl.org/rss/1.0/")
                               (xhtml . "http://www.w3.org/1999/xhtml")))))))

(define (sxml-text node)
  (define (collect node)
    (cond ((string? node) (list node))
          ((pair? node)
           (append-map collect
                       (if (and (pair? node) (symbol? (car node)))
                           (cdr node) node)))
          (else '())))
  (cond ((not node) #false)
        ((string? node) node)
        ((pair? node)
         (let ((texts (collect node)))
           (and (not (null? texts))
		(string-concatenate texts))))
        (else #false)))

;; Get the first match using an SXPath-like manual accessor.
(define (sxml-find sxml . path)
  (let loop ((node sxml) (path path))
    (if (null? path)
        node
        (let ((tag (car path)))
          (let find ((children (if (pair? node) (cdr node) '())))
            (cond ((null? children) #false)
                  ((and (pair? (car children))
                        (eq? (caar children) tag))
                   (loop (car children) (cdr path)))
                  (else (find (cdr children)))))))))

;; Get all matching children.
(define (sxml-find-all sxml tag)
  (if (not (pair? sxml))
      '()
      (filter (lambda (child) (and (pair? child) (eq? (car child) tag)))
              (cdr sxml))))

;; Get text content of a nested element.
(define (sxml-text-at sxml . path)
  (let ((node (apply sxml-find sxml path)))
    (and node (sxml-text node))))

;; Get attribute value.

;; Strip namespace prefix from SXML tag names for HTML output.
(define (strip-ns-prefix symbol)
  (let* ((s (symbol->string symbol))
         (i (string-index s #\:)))
    (if i
	(substring s (+ i 1) (string-length s))
	s)))

;; Serialize SXML content back to HTML string (for entry content).
(define (sxml->string node escape-text tag-name)
  (cond ((not node) "")
        ((string? node) (escape-text node))
        ((pair? node)
         (if (and (symbol? (car node)) (not (eq? (car node) '@)))
             (let* ((tag (tag-name (car node)))
                    (rest (cdr node))
                    (attributes (if (and (pair? rest) (pair? (car rest))
					 (eq? (caar rest) '@))
				    (cdar rest) '()))
                    (children (if (and (pair? rest) (pair? (car rest))
                                       (eq? (caar rest) '@))
                                  (cdr rest) rest))
                    (attr-str (string-concatenate
                               (map (lambda (a)
                                      (format " ~a=\"~a\""
                                              (car a) (html-escape (cadr a))))
                                    attributes)))
                    (recurse (lambda (n)
                               (sxml->string n escape-text tag-name))))
               (if (null? children)
                   (format "<~a~a/>" tag attr-str)
                   (format "<~a~a>~a</~a>"
                           tag attr-str
                           (string-concatenate (map recurse children))
                           tag)))
             (let ((recurse (lambda (n)
                              (sxml->string n escape-text tag-name))))
               (string-concatenate (map recurse node)))))
        (else (escape-text (x->string node)))))

(define (sxml->html-string node)
  (sxml->string node identity strip-ns-prefix))

(define (sxml->xml-string node)
  (sxml->string node html-escape symbol->string))

;; Make an entry record.
(define (make-entry title link content author date-str channel-name channel-link
                    feed-url)
  (let ((parsed-date (parse-date/loose date-str)))
    `((title . ,title)
      (link . ,link)
      (content . ,content)
      (author . ,author)
      (date-string . ,date-str)
      (date . ,parsed-date)
      (channel-name . ,channel-name)
      (channel-link . ,channel-link)
      (feed-url . ,feed-url))))

(define (parse-atom sxml channel-name feed-url)
  (let* ((feed (sxml-find sxml 'atom:feed))
         (channel-link
	  (or (let ((link-node
                     (let find ((children (if feed (cdr feed) '())))
                       (cond ((null? children) #false)
                             ((and (pair? (car children))
                                   (eq? (caar children) 'atom:link)
                                   (let ((rel (sxml:attr (car children) 'rel)))
                                     (or (not rel)
                                         (string=? rel "alternate")))
                                   (let ((type (sxml:attr (car children)
							  'type)))
                                     (or (not type)
                                         (string-contains type "html"))))
                              (car children))
                             (else (find (cdr children)))))))
                (and link-node (sxml:attr link-node 'href)))
              ""))
         (entries (if feed (sxml-find-all feed 'atom:entry) '())))
    (filter-map
     (lambda (entry)
       (let ((title (sxml-text-at entry 'atom:title))
             (link (let ((link-node
                          (let find ((children (cdr entry)))
                            (cond ((null? children) #false)
                                  ((and (pair? (car children))
                                        (eq? (caar children) 'atom:link)
                                        (let ((rel (sxml:attr (car children)
							      'rel)))
                                          (or (not rel)
                                              (string=? rel "alternate"))))
                                   (car children))
                                  (else (find (cdr children)))))))
                     (and link-node (sxml:attr link-node 'href))))
             (content-node (or (sxml-find entry 'atom:content)
                               (sxml-find entry 'atom:summary)))
             (author (sxml-text-at entry 'atom:author 'atom:name))
             (date (or (sxml-text-at entry 'atom:updated)
                       (sxml-text-at entry 'atom:published))))
         (let ((content (if content-node
                            (let ((type (sxml:attr content-node 'type)))
                              (if (and type (string-contains type "html"))
                                  (string-concatenate
                                   (map sxml->html-string
                                        (filter (lambda (x)
                                                  (not (and (pair? x)
							    (eq? (car x) '@))))
                                                (cdr content-node))))
                                  (html-escape (or (sxml-text content-node)
						   ""))))
                            "")))
           (make-entry (or title "")
		       (or link "")
		       content
		       (or author "")
                       (or date "")
		       channel-name
		       channel-link
		       feed-url))))
     entries)))

(define (parse-rss2 sxml channel-name feed-url)
  (let* ((channel (sxml-find sxml 'rss 'channel))
         (channel-link (or (sxml-text-at channel 'link) ""))
         (items (if channel (sxml-find-all channel 'item) '())))
    (filter-map
     (lambda (item)
       (let ((title (sxml-text-at item 'title))
             (link (sxml-text-at item 'link))
             (content (or (sxml-text-at item 'content:encoded)
                          (sxml-text-at item 'description)))
             (author (or (sxml-text-at item 'dc:creator)
                         (sxml-text-at item 'author)))
             (date (or (sxml-text-at item 'pubDate)
                       (sxml-text-at item 'dc:date))))
         (make-entry (or title "")
		     (or link "")
		     (or content "")
                     (or author "")
		     (or date "")
                     channel-name
		     channel-link
		     feed-url)))
     items)))

(define (parse-rss1 sxml channel-name feed-url)
  (let* ((channel (sxml-find sxml 'rdf:RDF 'rss1:channel))
         (channel-link (or (sxml-text-at channel 'rss1:link) ""))
         (rdf (sxml-find sxml 'rdf:RDF))
         (items (if rdf (sxml-find-all rdf 'rss1:item) '())))
    (filter-map
     (lambda (item)
       (let ((title (sxml-text-at item 'rss1:title))
             (link (sxml-text-at item 'rss1:link))
             (content (or (sxml-text-at item 'content:encoded)
                          (sxml-text-at item 'rss1:description)))
             (author (sxml-text-at item 'dc:creator))
             (date (sxml-text-at item 'dc:date)))
         (make-entry (or title "")
		     (or link "")
		     (or content "")
                     (or author "")
		     (or date "")
                     channel-name
		     channel-link
		     feed-url)))
     items)))

(define (parse-feed sxml channel-name feed-url)
  (cond ((sxml-find sxml 'atom:feed)
         (parse-atom sxml channel-name feed-url))
        ((sxml-find sxml 'rss)
         (parse-rss2 sxml channel-name feed-url))
        ((sxml-find sxml 'rdf:RDF)
         (parse-rss1 sxml channel-name feed-url))
        (else
         (format (current-error-port)
                 "Unknown feed format for ~a~%" feed-url)
         '())))

(define image-uri-regexp
  '(: "http" (? "s") "://"
      (+ (- graphic "\""))
      "." (or "jpeg" "jpg" "png" "svg")))

(define (sha1-bytes bv)
  (let ((digest (make <sha1>)))
    (digest-update! digest bv)
    (digest-hexify (digest-final! digest))))

(define (download-image uri output-dir)
  (format (current-error-port) "Downloading image ~a~%" uri)
  (guard (_ (else ""))
    (let-values (((scheme user host port path query fragment)
                  (uri-parse uri)))
      (let* ((secure (and scheme (string-ci=? scheme "https")))
             (host+port (if port (format "~a:~a" host port) host))
             (path+query (if query (format "~a?~a" path query) path)))
        (call-with-port
         (open-output-bytevector)
         (lambda (out)
           (parameterize ((http-user-agent "https://planet.scheme.org/ images"))
             (http-get host+port path+query
                       :secure secure :sink out :flusher (lambda _ #true)))
           (let* ((bytes (get-output-bytevector out))
                  (sha (sha1-bytes bytes))
                  (original-extension (path-extension (sys-basename path)))
                  (extension (string-downcase
                              (if (string=? original-extension "jpg")
				  "jpeg"
				  original-extension)))
                  (relpath (format "images/cache/~a.~a" sha extension))
                  (fullpath (build-path output-dir relpath)))
             (make-directory* (sys-dirname fullpath))
             (call-with-port (open-binary-output-file fullpath)
                             (lambda (out) (write-bytevector bytes out)))
             relpath)))))))

(define (cache-images-in-html html-string image-cache output-dir)
  (let loop ((parts (regexp-partition image-uri-regexp html-string))
             (odd? #false)
             (cache image-cache)
             (accumulator '()))
    (if (null? parts)
        (values (string-concatenate (reverse accumulator)) cache)
        (let ((s (car parts)))
          (if odd?
              (let* ((pair (assoc s cache))
                     (file (if pair
			       (cdr pair)
                               (download-image s output-dir)))
                     (new-cache (if (or pair (string=? "" file))
                                    cache
                                    (cons (cons s file) cache))))
                (loop (cdr parts) #false new-cache (cons file accumulator)))
              (loop (cdr parts) #true cache (cons s accumulator)))))))

(define (cache-all-images entries image-cache output-dir)
  (let loop ((entries entries) (cache image-cache) (accumulator '()))
    (if (null? entries)
        (values (reverse accumulator) cache)
        (let-values (((cached-content new-cache)
                      (cache-images-in-html
                       (or (entry-ref (car entries) 'content) "")
                       cache output-dir)))
          (loop (cdr entries) new-cache
                (cons (cons cached-content (car entries)) accumulator))))))

(define (entry-sxml pair date-format)
  (let* ((cached-content (car pair))
         (entry (cdr pair))
         (title (entry-ref entry 'title))
         (link (html-escape (or (entry-ref entry 'link) "")))
         (author (entry-ref entry 'author))
         (entry-date (date->display (entry-ref entry 'date) date-format)))
    `(div (@ (class "entrygroup"))
          ,@(if (and title (not (string=? title "")))
                `((h4 (a (@ (href ,link)) ,title)))
                '())
          (div (@ (class "entry"))
               (div ,cached-content)
               (p (@ (class "date"))
                  (a (@ (href ,link))
                     ,@(if (and author (not (string=? author "")))
                           `(,(string-append
                               "by " (html-escape author) " at "))
                           '())
                     ,(html-escape entry-date)))))))

(define (channel-group-sxml channel-entries date-format)
  (let* ((first-entry (cdr (car channel-entries)))
         (channel-name (entry-ref first-entry 'channel-name))
         (channel-link (or (entry-ref first-entry 'channel-link) "")))
    `(div (@ (class "channelgroup"))
          (h3 (a (@ (href ,(html-escape channel-link))
                    (title ,(html-escape channel-name)))
                 ,(html-escape channel-name)))
          ,@(map (lambda (pair) (entry-sxml pair date-format))
                 channel-entries))))

(define (day-group-sxml day-entries date-format)
  (let ((date-string (date->display (entry-ref (cdr (car day-entries)) 'date)
                                    date-format))
        (channel-groups (group-sequence day-entries
                                       :key (lambda (pair)
                                              (entry-ref (cdr pair)
                                                         'channel-name))
                                       :test equal?)))
    `(div (@ (class "daygroup"))
          (h2 ,(html-escape date-string))
          ,@(map (lambda (group) (channel-group-sxml group date-format))
                 channel-groups))))

(define (sidebar-sxml feeds channel-links date-format now)
  `(div (@ (class "sidebar"))
        (h2 "About")
        (p (b "Planet Scheme")
           " collects blog posts from individuals"
           " and projects around the Scheme community.")
        (p (img (@ (src "images/feed-icon-10x10.png") (alt "")))
           " " (a (@ (href "/atom.xml")) "Feed"))
        (h2 "Maintenance")
        (p "Planet Scheme is brought to you by the "
           (a (@ (href "https://www.scheme.org/")) "Scheme.org")
           " community. It was previously curated by"
           ,(string-append " Jens Axel S\u00f8gaard."))
        (p "To send feedback or to have your blog featured,"
           " please write the "
           (a (@ (href "https://srfi.schemers.org/srfi-list-subscribe.html#schemeorg"))
              (code "schemeorg") " mailing list")
           ".")
        (p (a (@ (href "https://github.com/schemeorg/planet.scheme.org"))
              "Source code"))
        (h2 "Blogs")
        (ul ,@(map (lambda (feed)
                     (let* ((name (feed-ref feed 'name))
                            (url (feed-ref feed 'url))
                            (blog-link (assoc url channel-links))
                            (blog-url (if (and blog-link
                                               (not (string=? (cdr blog-link) "")))
                                          (cdr blog-link) url)))
                       `(li (a (@ (href ,(html-escape blog-url))
                                  (title ,(html-escape name)))
                               ,(html-escape name))
                            " "
                            (a (@ (href ,(html-escape url))) "(feed)"))))
                   feeds))
        (h2 "Orrery")
        (ul (li (a (@ (href "http://planet.clojure.in/")) "Planet Clojure"))
            (li (a (@ (href "https://planet.lisp.org/")) "Planet Lisp"))
            (li (a (@ (href "https://racket-stories.com/")) "Racket Stories")))
        (h2 "Source code")
        (p (a (@ (href "https://github.com/schemeorg-community/planet.scheme.org")) "Github"))
        (h2 "Last updated")
        (p ,(html-escape (date->display now date-format)))))

(define (page-sxml planet-name day-groups sidebar)
  `(html
    (head
     (title ,(html-escape planet-name))
     (meta (@ (charset "utf-8")))
     (meta (@ (name "generator")
              (content "Planet Scheme (Gauche)")))
     (link (@ (href "planet.css") (rel "stylesheet") (type "text/css")))
     (link (@ (href "/atom.xml")
	      (rel "alternate")
	      (type "application/atom+xml")))
     (link (@ (href "/rss20.xml")
	      (rel "alternate")
	      (type "application/rss+xml")))
     (link (@ (href "images/favicon/favicon.svg")
	      (rel "icon")
	      (sizes "any")
	      (type "image/svg+xml"))))
    (body
     (h1 ,(html-escape planet-name))
     ,@day-groups
     ,sidebar)))

(define (write-html-file path page)
  (call-with-output-file path
    (lambda (out)
      (display "<!DOCTYPE html>\n" out)
      (display (sxml->html-string page) out)
      (newline out))))

(define (generate-html config entries output-dir channel-links)
  (let* ((planet-name (config-ref config 'name))
         (date-format (config-ref config 'date-format "%A, %B %-d, %Y"))
         (feeds (config-feeds config))
         (now (current-date)))
    (let-values (((processed-entries final-cache)
                  (cache-all-images entries (load-image-cache output-dir) output-dir)))
      (let* ((day-groups
              (group-sequence processed-entries
                              :key (lambda (pair)
                                     (date->display
                                      (entry-ref (cdr pair) 'date)
                                      date-format))
                              :test equal?))
             (page
              (page-sxml
               planet-name
               (map (lambda (group) (day-group-sxml group date-format))
                    day-groups)
               (sidebar-sxml feeds channel-links date-format now))))
        (write-html-file (build-path output-dir "index.html") page)
        (save-image-cache final-cache output-dir)))))

(define (write-xml-file path doc)
  (call-with-output-file path
    (lambda (out)
      (display "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" out)
      (display (sxml->xml-string doc) out)
      (newline out))))

(define (entry-title-string entry)
  (let ((title (or (entry-ref entry 'title) ""))
        (channel (or (entry-ref entry 'channel-name) "")))
    (if (string=? title "")
        channel
        (string-append channel ": " title))))

(define (generate-atom config entries output-dir)
  (let ((planet-name (config-ref config 'name))
        (planet-link (config-ref config 'link))
        (now (current-date)))
    (write-xml-file
     (build-path output-dir "atom.xml")
     `(feed (@ (xmlns "http://www.w3.org/2005/Atom"))
            (title ,planet-name)
            (link (@ (href ,planet-link) (rel "alternate")))
            (link (@ (href ,(string-append planet-link "atom.xml"))
                     (rel "self")
                     (type "application/atom+xml")))
            (id ,planet-link)
            (updated ,(date->iso8601 now))
            ,@(map
               (lambda (entry)
                 (let ((link (or (entry-ref entry 'link) ""))
                       (content (or (entry-ref entry 'content) ""))
                       (author (or (entry-ref entry 'author) "")))
                   `(entry
                     (title ,(entry-title-string entry))
                     (link (@ (href ,link) (rel "alternate")))
                     (id ,link)
                     (updated ,(date->iso8601 (entry-ref entry 'date)))
                     ,@(if (string=? author "")
                           '()
                           `((author (name ,author))))
                     (content (@ (type "html")) ,content)
                     (source (title ,(or (entry-ref entry 'channel-name) ""))))))
               entries)))))

(define (generate-rss config entries output-dir)
  (let ((planet-name (config-ref config 'name))
        (planet-link (config-ref config 'link)))
    (write-xml-file
     (build-path output-dir "rss20.xml")
     `(rss (@ (version "2.0")
              (|xmlns:atom| "http://www.w3.org/2005/Atom"))
           (channel
            (title ,planet-name)
            (link ,planet-link)
            (language "en")
            (description ,(string-append planet-name " - " planet-link))
            (|atom:link| (@ (rel "self")
                            (href ,(string-append planet-link "rss20.xml"))
                            (type "application/rss+xml")))
            ,@(map
               (lambda (entry)
                 (let ((link (or (entry-ref entry 'link) ""))
                       (content (or (entry-ref entry 'content) "")))
                   `(item
                     (title ,(entry-title-string entry))
                     (guid (@ (isPermaLink "true")) ,link)
                     (link ,link)
                     ,@(if (string=? content "")
                           '()
                           `((description ,content)))
                     (pubDate ,(date->rfc822 (entry-ref entry 'date))))))
               entries))))))

(define (image-cache-file output-dir)
  (build-path (sys-dirname output-dir) "images-cache.scm"))

(define (load-image-cache output-dir)
  (let ((f (image-cache-file output-dir)))
    (guard (_ (else '()))
      (let ((data (with-input-from-file f read)))
        (if (list? data) data '())))))

(define (save-image-cache cache output-dir)
  (with-output-to-file (image-cache-file output-dir)
    (lambda () (write cache) (newline))))

(define (entry-date-sortable entry)
  (let ((d (entry-ref entry 'date)))
    (if d (date->modified-julian-day d) 0)))

(define (sort-entries entries)
  (sort entries > entry-date-sortable))

(define (filter-entries entries items-per-page days-per-page)
  (let* ((now (current-date))
         (cutoff-jd (- (date->modified-julian-day now) days-per-page))
         (by-date (filter (lambda (e) (>= (entry-date-sortable e) cutoff-jd))
                          entries)))
    (if (> (length by-date) items-per-page)
        (take by-date items-per-page)
        by-date)))

(define (main args)
  (let* ((config-file (if (> (length args) 1) (cadr args) "config.scm"))
         (config (with-input-from-file config-file read))
         (output-dir (or (and (> (length args) 2) (caddr args)) "output"))
         (items-per-page (config-ref config 'items-per-page 15))
         (days-per-page (config-ref config 'days-per-page 100))
         (feeds (config-feeds config)))
    (make-directory* output-dir)
    (make-directory* (build-path output-dir "images" "cache"))
    (format (current-error-port) "Fetching ~a feeds.~%" (length feeds))
    (let ((all-entries
           (append-map
            (lambda (feed)
              (let ((url (feed-ref feed 'url))
                    (name (feed-ref feed 'name)))
                (format (current-error-port) "  ~a (~a)~%" name url)
                (let ((xml (fetch-feed-xml url)))
                  (if xml
                      (let ((sxml (safe-parse-xml xml)))
                        (if sxml
                            (parse-feed sxml name url)
                            '()))
                      '()))))
            feeds)))
      (format (current-error-port)
	      "Got ~a entries total.~%"
	      (length all-entries))
      (let* ((channel-links
              (let loop ((entries all-entries) (accumulator '()))
                (if (null? entries)
		    accumulator
                    (let* ((e (car entries))
                           (feed-url (entry-ref e 'feed-url))
                           (ch-link (entry-ref e 'channel-link)))
                      (loop (cdr entries)
                            (if (or (not ch-link) (string=? ch-link "")
                                    (assoc feed-url accumulator))
                                accumulator
                                (cons (cons feed-url ch-link) accumulator)))))))
             (entries (filter-entries (sort-entries all-entries)
                                     items-per-page days-per-page)))
        (format (current-error-port) "Showing ~a entries.~%" (length entries))
        (generate-html config entries output-dir channel-links)
        (generate-atom config entries output-dir)
        (generate-rss config entries output-dir)
        (format (current-error-port) "Done.  Output in \"~a/\".~%" output-dir)
        0))))