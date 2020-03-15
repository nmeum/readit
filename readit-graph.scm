(import (chicken process-context) (chicken format)
        matchable (readit parser) srfi-1)

(define (parse-input port)
  (let ((r (parse-readit port)))
    (if r r (error "syntax error" port))))

(define (parse-files fps)
  (fold (lambda (fp entries)
          (append (call-with-input-file
                    fp parse-input) entries))
        '() fps))

(define (build-alist entries)
  (fold (lambda (entry alist)
          (let* ((meta (car entry))
                 (key  (meta-key meta))
                 (pair (cons key entry)))
            (if (assoc key alist)
              (error "duplicate key" key)
              (cons pair alist))))
        '() entries))

(define (filter-refs entry)
  (match-let (((_ fields _) entry))
    (filter (lambda (field)
              (match-let (((key . val) field))
                (readit-ref? val))) fields)))

(define (print-ref entry field alist)
  (match-let (((key . val) field))
    (for-each (lambda (ref)
      (printf "\"~A\" -> \"~A\" [label=\"~A\"];~%"
            (meta-title (car entry))
            (let* ((p (assoc ref alist))
                   (e (if (not p)
                        (error "undefined reference" ref)
                        (cdr p))))
              (meta-title (car e)))
            key)) (vector->list val))))

(define (print-graph entries)
  (let ((alist (build-alist entries)))
    (printf "digraph G {~%rankdir = \"LR\";~%~%")
    (for-each (lambda (entry)
                (let ((refs (filter-refs entry)))
                  (for-each (lambda (ref)
                              (print-ref entry ref alist)) refs)))
              entries)
    (printf "}~%")))

(define (main)
  (let* ((files (command-line-arguments))
         (entries
           (if (null? files)
             (parse-input (current-input-port))
             (parse-files files))))
    (print-graph entries)))

(cond-expand
  ((or chicken-script compiling) (main))
  (else #t))
