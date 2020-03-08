(import comparse srfi-14)

(module (readit parser)
  (parse-entry make-meta meta-state
   meta-key meta-authors meta-title)
  (import scheme (chicken base) comparse srfi-14)

  (define-record-type metadata
      (make-meta state key authors title)
      metadata?
      (state meta-state)
      (key meta-key)
      (authors meta-authors)
      (title meta-title))

  ;;;;
  ;; Utility functions
  ;;;;

  (define (parse-any-except char)
    (as-string (one-or-more
      (in (char-set-complement! (char-set char))))))

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
  ;; Parsers for entry parts
  ;;;;

  (define parse-state
    (in (string->char-set "-x")))

  (define parse-key
    (as-string (enclosed-by (is #\<)
                            (one-or-more (in (char-set-union!
                                               char-set:letter
                                               char-set:digit
                                               char-set:punctuation)))
                            (is #\>))))

  (define parse-author
    (parse-any-except #\:))

  (define parse-title
    parse-text)

  ;;;;
  ;; Parsers for optional field values
  ;;;;

  (define parse-field-name
    (parse-any-except #\:))

  ;; TODO: Add more elaborate field value types
  (define parse-field-value
    parse-text)

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
                       (author parse-author)
                       (_      (is #\:))
                       (title  parse-title)
                       (_      (is #\newline))
                       (info   (maybe parse-info (list '() '()))))
      (result (cons (make-meta state key author title) info)))))
