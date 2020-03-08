(load "parser.scm")
(import comparse test)

(define (parse-file path)
  (call-with-input-file (string-append "testdata" "/" path)
    (lambda (port) (parse parse-entry port))))

(test-group "parser"
  (test "parse entry without fields and notes"
    (list (make-meta #\-
               "thompson1984trust"
               "Ken Thompson"
               "Reflections on Trusting Trust") '() '())
    (parse-file "thompson1984trust.txt"))

  (test "parse entry with fields and without notes"
    (list (make-meta #\x
                     "chomsky1956hierarchy"
                     "Noam Chomsky"
                     "Three models for the description of language")
          '(
            ("DOI" . "10.1109/TIT.1956.1056813")
            ("Importance" . "High")
           )
          '())
    (parse-file "chomsky1956hierarchy.txt")))
