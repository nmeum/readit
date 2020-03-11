(import (chicken process-context) srfi-37)

(define names '())

(define help
  (option
    '(#\h "help") #f #f
    (lambda _
      (usage))))

(define name
  (option
    '(#\n "name") #t #t
    (lambda (o n x vals)
      (set! names (cons x names))
      vals)))

(define (usage)
  (print "Usage: readit [-n NAME] FILE...")
  (exit))

(define (parse-args)
  (args-fold
    (command-line-arguments)
    (list help name)
    (lambda (o n x vals)
      (error "unrecognized option" n))
    cons
    '()))

(define (main)
  (let ((files (parse-args)))
    (if (null? files)
      (usage)
      (begin
        (print names)))))

(cond-expand
  ((or chicken-script compiling) (main))
  (else #t))
