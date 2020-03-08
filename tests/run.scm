(import (readit parser) comparse test)

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
    (parse-file "chomsky1956hierarchy.txt"))

  (test "parse entry without fields and with a single note"
    (list (make-meta #\-
                     "mccarthy1960lisp"
                     "John McCarthy"
                     "Recursive functions of symbolic expressions and their computation by machine, part I")
          '()
          '("Introduces a programming system called LISP"))
    (parse-file "mccarthy1960lisp.txt"))

  (test "parse entry with fields and notes"
    (list (make-meta #\-
                     "landin1966languages"
                     "Peter Landin"
                     "The next 700 programming languages")
          '(("DOI" . "10.1145/365230.365257"))
          '(
            "Describes a family of unimplemented languages"
            "Focuses on expression-based languages"
           ))
    (parse-file "landin1966languages.txt")))

(test-exit)
