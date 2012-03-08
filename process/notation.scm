(define-module process.notation
  (export exec run& run && ||
          run/port run/port->list run/file
          run/string run/strings run/sexp run/sexps
          run/port+proc run/collecting)
  (use srfi-11)
  (use gauche.process)
  (use file.util)
  (use util.match)
  (use process.helper))

(select-module process.notation)

(define (%run fork pf redirects)
  (receive (ins outs) (split-redirects (map normalize-redirect redirects))
    (%%run fork pf ins outs)))

(define (%%run fork? pf ins outs)
  (match pf
    (('^)
     (error "empty pipeline"))
    (('^ pf1)
     (%%run fork? pf1 ins outs))
    (('^ pf1 . rest)
     (let ((p (%%run #t pf1 ins '((> 1 stdout)))))
       (%%run fork? `(^ ,@rest) `((< 0 ,(process-output p))) outs)))
    (_
     (receive (cmd&args keys redirects) (split-pf pf)
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
                      :wait #f))))))

(define (exec pf . redirects)
  (%run #f pf redirects))

(define (run& pf . redirects)
  (%run #t pf redirects))

(define (run pf . redirects)
  (let ((p (apply run& pf redirects)))
    (process-wait p)
    (process-exit-status p)))

(define (run/port pf . redirects)
  (let ((p (apply run& pf '(> 1 stdout) redirects)))
    (process-output p)))

(define (run/port->list reader pf . redirects)
  (call-with-port (apply run/port pf redirects) (cut port->list reader <>)))

(define (run/file pf . redirects)
  (receive (out name) (sys-mkstemp
                       (build-path (temporary-directory)
                                   "gauche.process.out."))
    (let ((p (apply run& pf `(> ,out) redirects)))
      (process-wait p)
      (close-output-port out)
      name)))

(define (run/string pf . redirects)
  (call-with-port (apply run/port pf redirects) port->string))

(define (run/strings pf . redirects)
  (apply run/port->list read-line pf redirects))

(define (run/sexp pf . redirects)
  (call-with-port (apply run/port pf redirects) read))

(define (run/sexps pf . redirects)
  (apply run/port->list read pf redirects))

(define (run/port+proc pf . redirects)
  (let ((p (apply run& pf '(> stdout) redirects)))
    (values (process-output p)
            p)))

(define (run/collecting fds pf . redirects)
  (let* ((ports&names (map (lambda (fd)
                             (receive port&name
                                 (sys-mkstemp
                                  (build-path
                                   (temporary-directory)
                                   (format "gauche.process.out.fd.~A" fd)))
                               port&name))
                           fds))
         (ports (map car ports&names))
         (redirs (map (lambda (fd port)
                        `(> ,fd ,port))
                      fds ports))
         (names (map cadr ports&names))
         (p (apply run& pf (append redirs redirects))))
    (process-wait p)
    (for-each close-output-port ports)
    (apply values
           (process-exit-status p)
           (map open-input-file names))))

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
