(import comparse srfi-14)

(module (readit parser)
  (parse-entry make-meta meta-state
   meta-key meta-title parse-ref-literal)
  (import scheme (chicken base) comparse srfi-14)

  (define-record-type metadata
    (make-meta state key title)
    metadata?
    (state meta-state)
    (key meta-key)
    (title meta-title))

  (define symbol-charset
    (char-set-union
      char-set:letter
      char-set:digit
      (->char-set "!$%&*+-./:<=>?@^_~")))

  ;;;;
  ;; Utility functions
  ;;;;

  (define (parse-any-except char . chars)
    (as-string (one-or-more
      (in (char-set-complement! (list->char-set
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

  ;; TODO: Support escaping for data transparency
  (define parse-set-literal
    (enclosed-by (is #\{)
                 (zero-or-more
                   (sequence* ((elem (parse-any-except #\, #\}))
                               (_    (maybe (is #\,))))
                              (result elem)))
                 (is #\})))

  (define parse-sym-literal
    (bind (as-string
            (all-of
              (in (char-set-difference symbol-charset char-set:digit))
              (zero-or-more (in symbol-charset))))
          (lambda (str) (result (string->symbol str)))))

  (define parse-ref-literal
    (enclosed-by (is #\[) parse-sym-literal (is #\])))

  ;;;;
  ;; Parsers for entry parts
  ;;;;

  (define parse-state
    (in (string->char-set "-x")))

  ;; TODO: Support escaping for data transparency
  (define parse-set-literal
    (enclosed-by (is #\{)
                 (zero-or-more
                   (sequence* ((elem (parse-any-except #\, #\}))
                               (_    (maybe (is #\,))))
                              (result elem)))
                 (is #\})))

  (define parse-title
    parse-text)

  ;;;;
  ;; Parsers for optional field values
  ;;;;

  (define parse-field-name
    (parse-any-except #\:))

  ;; TODO: Add more elaborate field value types
  (define parse-field-value
    (any-of
      (bind parse-set-literal
            (lambda (lst) (result (list->vector lst))))
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
                       (key    parse-ref-literal)
                       (title  parse-title)
                       (_      (is #\newline))
                       (info   (maybe parse-info (list '() '()))))
      (result (cons (make-meta state key title) info)))))
