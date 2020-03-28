(module (readit parser)
  (make-meta meta-state meta-key meta-title parse-fields
   parse-readit readit-ref? readit-set? parse-indent parse-note parse-notes)
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
                (if (zero? (string-length output))
                  output
                  (string-append output " ")) str)) "" strings))

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

  (define parse-indent
    (any-of
      (is #\tab)
      (repeated (is #\space) min: 4 max: 4)))

  (define parse-blanks
    (zero-or-more (in char-set:blank)))

  (define parse-text
    (parse-any-except #\newline))

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
                    (_    parse-blanks))
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
    (sequence* ((_     parse-indent)
                (_     (is #\*))
                (_     parse-blanks)
                (name  parse-field-name)
                (_     (is #\:))
                (_     parse-blanks)
                (value parse-field-value)
                (_     parse-blanks)
                (_     (is #\newline)))
      (result (cons name value))))

  (define parse-fields
    (zero-or-more parse-field))

  ;;;;
  ;; Parser for optional notes
  ;;;;

  (define parse-note
    (sequence* ((_    parse-indent)
                (text parse-text)
                (_    (one-or-more (is #\newline))))
      (result (string-append text "\n"))))

  (define parse-notes
    (as-string (zero-or-more parse-note)))

  ;;;;
  ;; Combine utility parsers
  ;;;;

  (define parse-info
    (sequence* ((fields (maybe parse-fields '()))
                (notes  (maybe (preceded-by
                                 (is #\newline)
                                 parse-notes) "")))
      (result (list fields notes))))

  (define parse-entry
    (sequence* ((state  parse-state)
                (_      parse-blanks)
                (key    parse-key)
                (_      (is #\:))
                (_      parse-blanks)
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
    (parse (sequence* ((r parse-entries)
                       (_ end-of-input))
           (result r)) input)))
