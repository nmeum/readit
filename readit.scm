(import (chicken process-context) (chicken file) (chicken format)
        matchable (readit parser) srfi-1 srfi-37)

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

(define (field-filter names)
  (lambda (field)
    (match-let (((fkey . _) field))
      (any (lambda (name)
             (equal? name fkey)) names))))

(define (filter-entries entries names)
  (fold (lambda (entry fields)
          (match-let (((_ f _) entry))
            (append (filter (field-filter names) f) fields)))
        '() entries))

(define (main)
  (let ((fps (parse-args)))
    (when (null? fps)
      (usage))

    (for-each (lambda (fp)
                (unless (file-exists? fp)
                  (error "file does not exist" file))) fps)

    (let* ((entries (parse-files fps))
           (fields  (filter-entries entries names)))
      (for-each (lambda (field)
        (printf "~A:~A~%" (car field) (cdr field))) fields))))

(cond-expand
  ((or chicken-script compiling) (main))
  (else #t))
