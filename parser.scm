(module (readit parser)
  (make-meta meta-state meta-key meta-title
   parse-readit readit-ref? readit-set?)
  (import scheme (chicken base) comparse srfi-1 srfi-14)

  (define-record-type metadata
    (make-meta state key title)
    metadata?
    (state meta-state)
    (key meta-key)
    (title meta-title))

  (define-record-printer (metadata input port)
    (define (join . strings)
      (fold (lambda (str output)
              (string-append
                (if (> (string-length output) 0)
                  (string-append output " ")
                  output) str)) "" strings))

    (display (join
               (string (meta-state input))
               (string-append "[" (symbol->string (meta-key input)) "]:")
               (meta-title input))))

  (define symbol-charset
    (char-set-union
      char-set:letter
      char-set:digit
      (->char-set "!$%&*+-./:<=>?@^_~")))

  (define symbol-charset-start
    (char-set-difference symbol-charset char-set:digit))

  ;;;;
  ;; Utility functions
  ;;;;

  (define parse-symbol
    (bind (as-string (all-of
                       (in symbol-charset-start)
                       (zero-or-more (in symbol-charset))))
          (lambda (str) (result (string->symbol str)))))

  (define (parse-any-except char . chars)
    (as-string (one-or-more
      (in (char-set-complement (list->char-set
                                  (cons char chars)))))))

  (define parse-spaces
    (zero-or-more (in char-set:blank)))

  (define parse-text
    (parse-any-except #\newline))

  (define-syntax spaces-sequence*
    (syntax-rules ()
      ((_ () body ...)
       (begin body ...))
      ((_ ((binding parser) more-bindings ...) body ...)
       (bind (preceded-by parse-spaces parser)
             (lambda (binding)
                (spaces-sequence* (more-bindings ...) body ...))))))

  ;;;;
  ;; Parser for literals
  ;;;;

  (define (parse-escaped ctrl-chars)
    (define parse-char
      (any-of
        (sequence* ((_ (is #\\))
                    (i item))
          (result i))
        (in (char-set-complement (list->char-set ctrl-chars)))))

    (as-string (one-or-more parse-char)))

  (define (parse-vector parser)
    (define (parse-vector*)
      (one-or-more
        (sequence* ((elem parser)
                    (_    (maybe (is #\,)))
                    (_    parse-spaces))
          (result elem))))

    (bind (parse-vector*)
          (lambda (lst)
            (result (list->vector lst)))))

  (define parse-set
    (enclosed-by (is #\{)
                 (parse-vector (parse-escaped '(#\, #\})))
                 (is #\})))

  (define parse-ref
    (enclosed-by (is #\[)
                 (parse-vector parse-symbol)
                 (is #\])))

  ;;;;
  ;; Parsers for entry parts
  ;;;;

  (define parse-state
    (in (string->char-set "-x")))

  (define parse-key
    (enclosed-by (is #\[) parse-symbol (is #\])))

  (define parse-title
    parse-text)

  ;;;;
  ;; Parsers for optional field values
  ;;;;

  (define parse-field-name
    (parse-any-except #\:))

  (define parse-field-value
    (any-of
      parse-set
      parse-ref
      parse-text))

  (define parse-field
    (spaces-sequence* ((_     (is #\*))
                       (name  parse-field-name)
                       (_     (is #\:))
                       (value parse-field-value)
                       (_     (is #\newline)))
      (result (cons name value))))

  (define parse-fields
    (zero-or-more parse-field))

  ;;;;
  ;; Parser for optional notes
  ;;;;

  (define parse-note
    (spaces-sequence* ((_    (is #\*))
                       (text parse-text)
                       (_    (is #\newline)))
      (result text)))

  (define parse-notes
    (zero-or-more parse-note))

  ;;;;
  ;; Combine utility parsers
  ;;;;

  (define parse-info
    (sequence* ((fields (maybe parse-fields '()))
                (notes  (maybe (preceded-by
                                 (is #\newline)
                                 parse-notes) '())))
      (result (list fields notes))))

  (define parse-entry
    (spaces-sequence* ((state  parse-state)
                       (key    parse-key)
                       (_      (is #\:))
                       (title  parse-title)
                       (_      (is #\newline))
                       (info   (maybe parse-info (list '() '()))))
      (result (cons (make-meta state key title) info))))

  (define parse-entries
    (one-or-more (preceded-by
                   (zero-or-more (in char-set:whitespace))
                   parse-entry)))

  ;;;;
  ;; Interface functions
  ;;;;

  (define (readit-ref? obj)
    (and (vector? obj)
         (>= (vector-length obj) 1)
         (every symbol? (vector->list obj))))

  (define (readit-set? obj)
    (and (vector? obj)
         (>= (vector-length obj) 1)
         (every string? (vector->list obj))))

  (define (parse-readit input)
    (parse parse-entries input)))
