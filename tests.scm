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
    (parse-file "thompson1984trust.txt")))
