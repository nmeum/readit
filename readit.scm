(import (chicken process-context) (chicken file) (chicken format)
        matchable (readit parser) srfi-1 srfi-37)

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
    (list help done file value)
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
  (let* ((args (parse-args)))
    (when (not (equal? (length args) 1))
      (usage))

    (for-each (lambda (fp)
                (unless (file-exists? fp)
                  (error "file does not exist" fp))) files)

    (let* ((entries  (parse-files files))
           (filtered (filter-entries entries state (car args) fvals)))
      (for-each (lambda (entry)
                  (match-let (((meta fields _) entry))
                    (unless (null? fields)
                      (print meta))))
                filtered))))

(cond-expand
  ((or chicken-script compiling) (main))
  (else #t))
