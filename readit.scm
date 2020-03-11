(import (chicken process-context) (chicken file) (chicken format)
        matchable (readit parser) srfi-1 srfi-37)

(define names '())  ;; or
(define fvals '()) ;; and

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

(define value
  (option
    '(#\v "value") #t #t
    (lambda (o n x vals)
      (set! fvals (cons x fvals))
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
    (list help name value)
    (lambda (o n x vals)
      (error "unrecognized option" n))
    cons
    '()))

;; TODO: match regex
;; TODO: optionally ignore case during matches
(define (field-matches? fval str)
  (cond ((readit-ref? fval) #f)
        ((readit-set? fval)
         (any (lambda (e) (equal? e str)) (vector->list fval)))
        (else (equal? fval str))))

(define (filter-fields fields names vals)
  (filter (lambda (field)
            (match-let (((key . val) field))
              (and
                (any (lambda (n) (equal? key n)) names)
                (every (lambda (v) (field-matches? val v)) vals))))
          fields))

(define (fold-entries entries names vals)
  (fold (lambda (entry mod-entries)
    (match-let (((meta fields notes) entry))
      (append (list
                (list meta
                      (filter-fields fields names vals)
                      notes))
              mod-entries))) '() entries))

(define (main)
  (let ((fps (parse-args)))
    (when (null? fps)
      (usage))

    (for-each (lambda (fp)
                (unless (file-exists? fp)
                  (error "file does not exist" fp))) fps)

    (let* ((entries  (parse-files fps))
           (filtered (fold-entries entries names fvals)))
      (for-each (lambda (entry)
                  (match-let (((meta fields _) entry))
                             (if (null? fvals)
                               (for-each (lambda (f)
                                           (print (car f) ":" (cdr f)))
                                         fields)
                               (unless (null? fields)
                                 (display meta)
                                 (newline)))))
                filtered))))

(cond-expand
  ((or chicken-script compiling) (main))
  (else #t))
