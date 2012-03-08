(define-module process.notation.syntactical
  (export exec run& run && ||
          run/port run/port->list run/file
          run/string run/strings run/sexp run/sexps
          run/port+proc run/collecting)
  (use srfi-11)
  (use gauche.process)
  (use file.util)
  (use process.helper))

(select-module process.notation.syntactical)

(define-syntax %run
  (syntax-rules ()
    ((_ fork? pf (redirects ...))
     (receive (ins outs) (split-redirects
                          (map normalize-redirect `(redirects ...)))
       (%%run fork? pf ins outs)))))

(define-syntax %%run
  (syntax-rules (^ unquote unquote-splicing)
    ((_ fork? (^ pf) ins outs)
     (%%run fork? pf ins outs))
    ((_ fork? (^ pf1 pfs ...) ins outs)
     (let ((p (%%run #t pf1 ins '((> 1 stdout)))))
       (%%run fork? (^ pfs ...) `((< 0 ,(process-output p))) outs)))
    ((_ fork? (cmd args ...) ins outs)
     (receive (cmd&args keys redirects) (split-pf `(cmd args ...))
       (let-keywords keys ((error #f)
                           (directory #f)
                           (sigmask #f)
                           (detached #f)
                           (host #f))
         (run-process cmd&args
                      :error error
                      :directory directory
                      :sigmask sigmask
                      :detached detached
                      :host host
                      :redirects (append ins outs redirects)
                      :fork fork?
                      :wait #f
                      ))))
    ((_ fork? (cmd args ...) ins outs)
     (%%run fork? ((cmd args ...)) ins outs))))

(define-syntax exec
  (syntax-rules ()
    ((_ pf redirects ...)
     (%run #f pf (redirects ...)))))

(define-syntax run&
  (syntax-rules ()
    ((_ pf redirects ...)
     (%run #t pf (redirects ...)))))

(define-syntax run
  (syntax-rules ()
    ((_ pf redirects ...)
     (let ((p (run& pf redirects ...)))
       (and (process-wait p)
            (process-exit-status p))))))

(define-syntax run/port
  (syntax-rules ()
    ((_ pf redirects ...)
     (process-output (run& pf (> stdout) redirects ...)))))

(define-syntax run/port->list
  (syntax-rules ()
    ((_ reader pf redirects ...)
     (call-with-port (run/port pf redirects ...) (cut port->list reader <>)))))

(define-syntax run/file
  (syntax-rules ()
    ((_ pf redirects ...)
     (receive (out name) (sys-mkstemp
                          (build-path (temporary-directory)
                                      "gauche.process.out."))
       (let ((p (run& pf (> ,out) redirects ...)))
         (process-wait p)
         (close-output-port out)
         name)))))

(define-syntax run/string
  (syntax-rules ()
    ((_ pf redirects ...)
     (call-with-port (run/port pf redirects ...) port->string))))

(define-syntax run/strings
  (syntax-rules ()
    ((_ pf redirects ...)
     (run/port->list read-line pf redirects ...))))

(define-syntax run/sexp
  (syntax-rules ()
    ((_ pf redirects ...)
     (call-with-port (run/port pf redirects ...) read))))

(define-syntax run/sexps
  (syntax-rules ()
    ((_ pf redirects ...)
     (run/port->list read pf redirects ...))))

(define-syntax run/port+proc
  (syntax-rules ()
    ((_ pf redirects ...)
     (let ((p (run& pf (> stdout) redirects ...)))
       (values (process-output p)
               p)))))

(define-syntax run/collecting
  (syntax-rules ()
    ((_ (fds ...) pf redirects ...)
     (%run/collecting (fds ...) () () () pf redirects ...))))

(define-syntax %run/collecting
  (syntax-rules ()
    ((_ () (fds ...) (ports ...) (names ...) pf redirects ...)
     (let-values (((ports names) (sys-mkstemp
                                  (build-path
                                   (temporary-directory)
                                   (format "gauche.process.out.fd.~A." fds))))
                  ...)
       (let ((p (run& pf (> fds ,ports) ... redirects ...)))
         (process-wait p)
         (close-output-port ports)
         ...
         (values (process-exit-status p)
                 (open-input-file names)
                 ...))))
    ((_ (fd . rest) (fds ...) (ports ...) (names ...) pf redirects ...)
     (%run/collecting rest
                      (fds ... fd)
                      (p ports ...)
                      (x names ...)
                      pf redirects ...))))

(define (exit-success? st)
  (zero? (sys-wait-exit-status st)))

(define-syntax ?
  (syntax-rules (run)
    ((_ (run pf redirects ...))
     (exit-success? (run pf redirects ...)))
    ((_ pf)
     (? (run pf)))))

(define-syntax &&
  (syntax-rules ()
    ((_ pf  ...)
     (and (? pf) ...))))

(define-syntax ||
  (syntax-rules ()
    ((_ pf  ...)
     (or (? pf) ...))))

