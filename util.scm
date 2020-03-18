(define (parse-input port)
  (let ((r (parse-readit port)))
    (if r r (error "syntax error" port))))

(define (parse-files fps)
  (fold (lambda (fp entries)
          (append (call-with-input-file
                    fp parse-input) entries))
        '() fps))

(define (parse-args flags)
  (args-fold
    (command-line-arguments)
    flags
    (lambda (o n x vals)
      (error "unrecognized option" n))
    cons
    '()))
