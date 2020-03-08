(load "parser.scm")
(import comparse test matchable srfi-1)

(test-group "parser"
  (test "parse entry without fields and notes"
    (list (make-meta #\-
               "thompson1984trust"
               "Ken Thompson"
               "Reflections on Trusting Trust") '() '())
    (parse parse-entry "- <thompson1984trust> Ken Thompson: Reflections on Trusting Trust\n")))
