(define-module process.helper
  (export normalize-redirect split-redirects split-pf call-with-port)
  (use gauche.collection)
  (use util.match))

(select-module process.helper)

(define (input-redirect? x)
  (memq x '(< << <<< <&)))

(define (output-redirect? x)
  (memq x '(> >> >&)))

(define (normalize-redirect r)
  (match r
    (((? input-redirect? sym) src)
     `(,sym 0 ,src))
    (((? output-redirect? sym) sink)
     `(,sym 1 ,sink))
    (_ r)))

(define (split-redirects rs)
  (fold2 (lambda (r ins outs)
           (match r
             (((? input-redirect?) _fd _src)
              (values (cons r ins) outs))
             (((? output-redirect?) _fd _sink)
              (values ins (cons r outs)))
             (_
              (error "invalid redirection: " r))))
         '() '()
         rs))

(define (split-pf xs)
  (fold3 (lambda (x cmd&args keys redirs)
           (match x
             (((? keyword?) arg)
              (values cmd&args (append! x keys) redirs))
             (((? symbol?) . rest)
              (values cmd&args keys (cons (normalize-redirect x) redirs)))
             (_
              (values (cons x cmd&args) keys redirs))))
         '() '() '()
         (reverse xs)))
