(import (readit parser) test)

(define (parse-file path)
  (call-with-input-file (string-append "testdata" "/" path)
    (lambda (port) (parse-readit port))))

(define (parse-entries path)
  (let ((r (parse-file path)))
    (if r r (error "syntax error"))))

(define (parse-entry path)
  (car (parse-entries path)))

(test-group "parser"
  (test "parse entry without fields and notes"
    (list (make-meta #\-
               'thompson1984trust
               "Reflections on Trusting Trust") '() '())
    (parse-entry "thompson1984trust.txt"))

  (test "parse entry with fields and without notes"
    (list (make-meta #\x
                     'chomsky1956hierarchy
                     "Three models for the description of language")
          '(
            ("Importance" . "High")
            ("DOI" . "10.1109/TIT.1956.1056813")
           )
          '())
    (parse-entry "chomsky1956hierarchy.txt"))

  (test "parse entry with set fields"
    (list (make-meta #\x
                     'ritchie1974unix
                     "The UNIX Time-Sharing System")
          '(
            ("Authors" . #("Dennis M. Ritchie" "Ken Thompson"))
            ("Topics" . #("UNIX"))
           )
          '())
    (parse-entry "ritchie1974unix.txt"))

  (test "parse entry with escaped set fields"
    (list (make-meta #\-
                     'bratus2015bugs
                     "The Bugs We Have to Kill")
          '(
            ("Escaped Comma" . #("foo,bar"))
            ("Escaped Bracket" . #("foo{bar}"))
            ("Escaped Backslash" . #("foo\\bar"))
            ("Escaped All" . #("foobar"))
            ("Escaped Multiple" . #("foo," "bar"))
           )
          '())
    (parse-entry "bratus2015bugs.txt"))

  (test "parse entry with ref fields"
    (list (make-meta #\x
                     'bach1986unix
                     "The Design of the UNIX Operating System")
          '(
            ("Related" . #(ritchie1974unix))
            ("References" . #(dijkstra68sequential pike84blit))
           )
          '())
    (parse-entry "bach1986unix.txt"))

  (test "parse entry without fields and with a single note"
    (list (make-meta #\-
                     'mccarthy1960lisp
                     "Recursive functions of symbolic expressions and their computation by machine, part I")
          '()
          '("Introduces a programming system called LISP"))
    (parse-entry "mccarthy1960lisp.txt"))

  (test "parse entry with fields and notes"
    (list (make-meta #\-
                     'landin1966languages
                     "The next 700 programming languages")
          '(("DOI" . "10.1145/365230.365257"))
          '(
            "Describes a family of unimplemented languages"
            "Focuses on expression-based languages"
           ))
    (parse-entry "landin1966languages.txt")))

(test-exit)
