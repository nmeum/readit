(import (chicken process-context) (chicken format)
        matchable (readit parser) srfi-1 srfi-37)
(include-relative "util.scm")

(define kflag #f)

(define (usage)
  (print "Usage: readit [-k] FILE...")
  (exit))

(define help
  (option
    '(#\h "help") #f #f
    (lambda _
      (usage))))

(define use-keys
  (option
    '(#\k "keys") #f #f
    (lambda (o n x vals)
      (set! kflag #t)
      vals)))

(define (dot-escape str)
  (define (needs-esc c)
    (or (eqv? c #\") (eqv? c #\\)))

  (define (dot-escape* cur end)
    (if (> cur end)
      ""
      (let ((c (string-ref str cur)))
        (string-append
          (if (needs-esc c)
            (list->string (list #\\ c))
            (string c))
          (dot-escape* (+ cur 1) end)))))

  (dot-escape* 0 (- (string-length str) 1)))

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
  (define (node-desc meta)
    (if kflag
      (meta-key meta)
      (dot-escape (meta-title meta))))

  (match-let (((key . val) field))
    (for-each (lambda (ref)
      (printf "\t\"~A\" -> \"~A\" [label=\"~A\"];~%"
            (node-desc (car entry))
            (let* ((p (assoc ref alist))
                   (e (if (not p)
                        (error "undefined reference" ref)
                        (cdr p))))
              (node-desc (car e)))
            (dot-escape key))) (vector->list val))))

(define (print-graph entries)
  (let ((alist (build-alist entries)))
    (printf "digraph G {~%")
    (for-each (lambda (entry)
                (let ((refs (filter-refs entry)))
                  (for-each (lambda (ref)
                              (print-ref entry ref alist)) refs)))
              entries)
    (printf "}~%")))

(define (main)
  ;; TODO: Add -e flag (do not label edges)
  (let* ((files (parse-args (list help use-keys)))
         (entries
           (if (null? files)
             (parse-input (current-input-port))
             (parse-files files))))
    (print-graph entries)))

(cond-expand
  ((or chicken-script compiling) (main))
  (else #t))
