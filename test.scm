(use gauche.test)
(use srfi-1)
(use gauche.process)

(test-start "process.notation")

(use process.notation)
(test-module 'process.notation)

;; ----------------------------------------------------------------------
(test-section "&, %")

(test* "& returns <process>" #t (process? (& (true))))
(test* "% returns integer" #t (integer? (% (true))))

(define-syntax test-invalid-keyword-argument
  (syntax-rules ()
    ((_ key arg)
     (test* (format "invalid keyword to %: ~S" key)
            (test-error)
            (% ((true)
                key arg))))))

(test-invalid-keyword-argument :input "file")
(test-invalid-keyword-argument :output "file")
(test-invalid-keyword-argument :fork #t)
(test-invalid-keyword-argument :wait #t)

(test* "run/port returns port"
       #t
       (port? (run/port (echo foo))))

(test* "run/port: port contents"
       "foo\n"
       (port->string (run/port (echo foo))))

(test* "run-file"
       "foo\n"
       (let ((fn (run/file (echo foo))))
         (and (string? fn)
              (file-exists? fn)
              (call-with-input-file fn port->string))))

(test* "run/string"
       "1 2 3\n"
       (run/string (echo 1 2 3)))

(test* "run/port->list: port contents"
       '(1 2 3)
       (run/port->list read (echo 1 2 3)))

(let ((x 1))
  (test* "unquote argument"
         (format "~A ~A~%" x x)
         (run/string (echo ,x ,x))))

(let ((xs (iota 5)))
  (test* "unquote-splicing argument"
         xs
         (run/port->list read
                         (echo ,@xs))))

(let ((x 1)
      (cmd 'echo))
  (test* "unquote command"
         (format "~A ~A~%" x x)
         (run/string (,cmd ,x ,x))))

(let ((x 1)
      (cmd '(echo)))
  (test* "unquote-splicing command"
         (format "~A ~A~%" x x)
         (run/string (,@cmd ,x ,x))))

(let ((cmd 'echo))
  (test* "unquote whole command"
         "\n"
         (run/string (,cmd))))

(let* ((xs (iota 5))
       (cmd `(echo ,@xs)))
  (test* "unquote-splicing whole command"
         xs
         (run/port->list read
                         (,@cmd))))

(let ((cmds '(echo 1 2 3)))
  (test* "run/string: equivalence"
         (port->string (run/port (,@cmds)))
         (run/string (,@cmds))))

(let ((cmds '(echo 1 2 3)))
  (test* "run/port->list: equivalence"
         (port->list read
                     (run/port (,@cmds)))
         (run/port->list read (,@cmds))))

(test* "run/strings"
       (run/port->list read-line (^ (yes) (head -n 5)))
       (run/strings (^ (yes) (head -n 5))))

(test* "run/sexp"
       (read (run/port (^ (yes) (head -n 5))))
       (run/sexp (^ (yes) (head -n 5))))

(test* "run/sexps"
       (run/port->list read (^ (yes) (head -n 5)))
       (run/sexps (^ (yes) (head -n 5))))

(let ((n 5))
  (test* "pipeline"
         n
         (length (run/port->list read
                                 (^ (yes y) (head -n ,n))))))

(let ((file "test/output"))
  (test* "pipeline with redirects"
         (call-with-input-file "test/words" port->string)
         (begin
           (% (^ (cat -n) (cut -f 2))
              (< "test/words")
              (> ,file))
           (call-with-input-file file port->string))))

(test* "run/port+proc"
       "foo\n"
       (receive (in proc) (run/port+proc (echo foo))
         (and (process? proc)
              (input-port? in)
              (port->string in))))

(test* "run/collecting (1)"
       '((foo) (bar) (baz) (quux))
       (receive (_ p1 p2 p3 p4)
           (run/collecting (1 2 3 4) (test/echo4))
         (list (port->sexp-list p1)
               (port->sexp-list p2)
               (port->sexp-list p3)
               (port->sexp-list p4))))

(test* "run/collecting (2)"
       '((foo) (baz) (bar) (quux))
       (receive (_ p1 p2 p3 p4)
           (run/collecting (1 3 2 4) (test/echo4))
         (list (port->sexp-list p1)
               (port->sexp-list p2)
               (port->sexp-list p3)
               (port->sexp-list p4))))

(test* "run/collecting (3)"
       '((quux) (foo) (baz))
       (receive (_ p1 p2 p3)
           (run/collecting (4 1 3) (test/echo4))
         (list (port->sexp-list p1)
               (port->sexp-list p2)
               (port->sexp-list p3))))

(test* ":directory"
       (call-with-input-file "test/words" port->string)
       (run/string ((cat words)
                    :directory "test")))

(test* "port/string discards fd/2"
       "foo\n"
       (run/string (test/echo2)))

(test* ">&"
       "foo\n\nbar\n"
       (run/string (test/echo2)
                   (>& 2 1)))

(test* ":error, >&"
       (run/string (echo foo))
       (let ((name 'stderr))
         (port->string
          (process-output
           (& ((echo foo)
               :error name)
              (>& 1 2))
           name))))

(let ((file "test/words"))
  (test* "<&"
         (call-with-input-file file port->string)
         (call-with-input-file file
           (lambda (p)
             (run/string (cat)
                         (< 4 ,p)
                         (<& 0 4))))))

(let ((file "test/output"))
  (test* ">"
         "ab\n"
         (begin
          (% (echo ab)
             (> ,file))
          (call-with-input-file file port->string)))
  (test* ">>"
         "ab\ncd\n"
         (begin
           (% (echo cd)
              (>> ,file))
           (call-with-input-file file port->string))))

(let ((str "abc"))
  (test* "<<"
         str
         (run/string (cat)
                     (<< ,str))))

(let ((obj '("a" #\b c #x0d)))
  (test* "<< (error)"
         (test-error)
         (run/string (cat)
                     (<< ,obj)))
  (test* "<<<"
         (write-to-string obj)
         (run/string (cat)
                     (<<< ,obj))))

;; ----------------------------------------------------------------------
(test-section "&&, ||")

(define-syntax test-cond
  (syntax-rules ()
    ((_ exp expr)
     (test* (format "~S" 'expr) exp expr))))

(test-cond #t (&&))
(test-cond #t (&& (% (true))))
(test-cond #f (&& (% (false))))
(test-cond #f (&& (% (false)) (% (false))))
(test-cond #f (&& (% (false)) (% (true))))
(test-cond #f (&& (% (true)) (% (false))))
(test-cond #t (&& (% (true)) (% (true))))

(test* "&& (shorthand)"
       (&& (% (true)) (% (false)))
       (&& (true) (false)))

(let ((file "test/output"))
  (test* "&& (short circuit)"
         "ab\n"
         (begin
           (&& (% (test/echofail ab)
                  (> ,file))
               (% (echo cd)
                  (>> ,file)))
           (call-with-input-file file port->string))))

(test-cond #f (||))
(test-cond #t (|| (% (true))))
(test-cond #f (|| (% (false))))
(test-cond #f (|| (% (false)) (% (false))))
(test-cond #t (|| (% (false)) (% (true))))
(test-cond #t (|| (% (true)) (% (false))))
(test-cond #t (|| (% (true)) (% (true))))

(test* "|| (shorthand)"
       (|| (% (false)) (% (true)))
       (|| (false) (true)))

(let ((file "test/output"))
  (test* "|| (short circuit)"
         "ab\n"
         (begin
           (|| (% (echo ab)
                  (> ,file))
               (% (echo cd)
                  (>> ,file)))
           (call-with-input-file file port->string))))

(test-end)
