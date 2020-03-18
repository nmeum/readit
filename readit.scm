(import (chicken process-context) (chicken file) (chicken format)
        matchable (readit parser) srfi-1 srfi-37)
(include-relative "util.scm")

(define files '())
(define fvals '())
(define state '())

(define help
  (option
    '(#\h "help") #f #f
    (lambda _
      (usage))))

(define done
  (option
    '(#\x "exclude-done") #f #f
    (lambda (o n x vals)
      (set! state #\-)
      vals)))

(define file
  (option
    '(#\f "file") #t #t
    (lambda (o n x vals)
      (set! files (cons x files))
      vals)))

(define value
  (option
    '(#\v "value") #t #t
    (lambda (o n x vals)
      (set! fvals (cons x fvals))
      vals)))

(define (usage)
  (print "Usage: readit [-x] [-f FILE] [-v VALUE] NAME")
  (exit))

;; TODO: match regex
;; TODO: optionally ignore case during matches
(define (field-matches? fval str)
  (cond ((readit-ref? fval)
         (any (lambda (e) (equal? e (string->symbol str)))
              (vector->list fval)))
        ((readit-set? fval)
         (any (lambda (e) (equal? e str)) (vector->list fval)))
        (else (equal? fval str))))

(define (filter-fields fields name vals)
  (filter (lambda (field)
            (match-let (((key . val) field))
              (and
                (equal? key name)
                (every (lambda (v) (field-matches? val v)) vals))))
          fields))

(define (filter-entries entries state name vals)
  (filter (lambda (entry)
            (match-let (((meta fields _) entry))
              (and
                (or (null? state) (eqv? (meta-state meta) state))
                (not (null? (filter-fields fields name vals))))))
          entries))

(define (main)
  (let* ((args (parse-args (list help done file value))))
    (when (not (equal? (length args) 1))
      (usage))

    (let* ((entries
             (if (null? files)
               (parse-input (current-input-port))
               (parse-files files)))
           (filtered (filter-entries entries state (car args) fvals)))
      (for-each (lambda (entry) (print (car entry))) filtered))))

(cond-expand
  ((or chicken-script compiling) (main))
  (else #t))
