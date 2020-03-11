(import (chicken process-context) (chicken file)
        (readit parser) srfi-1 srfi-37)

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

(define (parse-file path)
  (call-with-input-file path
    (lambda (port)
      (let ((r (parse-readit port)))
        (if r r (error "syntax error in" fp))))))

(define (parse-files fps)
  (fold (lambda (fp entries)
          (append (parse-file fp) entries))
        '() fps))

(define (parse-args)
  (args-fold
    (command-line-arguments)
    (list help name)
    (lambda (o n x vals)
      (error "unrecognized option" n))
    cons
    '()))

(define (main)
  (let ((fps (parse-args)))
    (when (null? fps)
      (usage))

    (for-each (lambda (fp)
                (unless (file-exists? fp)
                  (error "file does not exist" file))) fps)

    (print (parse-files fps))))

(cond-expand
  ((or chicken-script compiling) (main))
  (else #t))
