# What's this?
Gauche-process-notation is a process notation library a la
[Scsh](http://www.scsh.net/).

This library is a thin wrapper to `run-process` procedure in
`gauche.process` module.

## Examples

    (use process.notation)

    ;;; download files
    (use srfi-1)

    (% (wget ,@(map (pa$ format "http://www.example.com/img/~D.jpg")
                    (iota 10 1))))

    ;;; classical word frequency analysis
    (define freqs
      (map (lambda (s)
             (call-with-input-string s (cut port->list read <>)))
           (run/strings (^ ((wget -O - "http://example.org/licenses/gpl.txt")
                            :error :null)
                          (tr -c "A-Za-z" "\n")
                          (tr "A-Z" "a-z")
                          (grep -v "^$")
                          (sort)
                          (uniq -c)
                          (sort -rn)))))

## API
### Syntax: ! pf redirects ...
### Syntax: & pf redirects ...
### Syntax: % pf redirects ...
### Auxiliary syntax: ^

Runs a command specified with the process form `pf`.

`&` forks a subprocess, and returns a `<process>` object.

`!` forks a subprocess, calls `sys-exec`, and never returns.

`%` forks a subprocess, waits until the subprocess terminates,
and returns `process-exit-status` of the subprocess.

`pf`'s syntax is:

    pf ::= (cmd args ...)
         | ((cmd args ...) :key error directory sigmask detached host)
         | (^ pf0 pf1 ...)

The first form specifies a command and its arguments in the same format
with `cmd/args` argument to `run-process`.

The next form can pass keyword arguments to `run-process`.

In these forms, `(cmd args ...)` is implicitly `quasiquote`d.
So you should `unquote` or `unquote-splicing` to evaluate the subforms.

The last is a pipeline connects each `pf`s with pipes.
`^` is one of the pipeline notations of
[Thompson shell](http://en.wikipedia.org/wiki/Thompson_shell).
`^` is bound in `gauche` module as an alias to `lambda`.

`redirects` specify how to redirect child process's IOs in the same format
as the `:redirects` arguments to `run-process` procedure.
As an extension of this library, `fd` part of the IO redirection are optional.
`fd`s for input redirections, `<`, `<<`, `<<<` and, `<&`, are defaults to `0`,
`fd`s for output redirections, `>`, `>>`, and `>&`, are defaults to `1`.
For example, `(< "file")` equals to `(< 0 "file")`, and
`(> :null)` equals to `(> 1 :null)`.

`redirects` are implicitly `quasiquote`d.

If `pf` is a pipeline, the input redirections in `redirects` are
for the left most subprocess in the pipeline,
the output redirections are for the right most subprocess.

### Syntax: run pf redirects ...
An alias to `%`.

### Syntax: run/port pf redirects ...
Runs a subprocess, and returns an input port connected to
the subprocess's stdout.
Returns immediately after forking subprocess.

### Syntax: run/port->list reader pf redirects ...
Equivalent to `(port->list reader (run/port pf redirects ...))`.
Returns when the port reaches at eof.

### Syntax: run/file pf redirects ...
Runs a subprocess, redirects its stdout to temporary file,
and returns the name of the temporary file.
Returns when the subprocess exits.

### Syntax: run/string pf redirects ...
Equivalent to `(port->string (run/port pf redirects ...))`.
Returns when the port reaches at eof.

### Syntax: run/strings pf redirects ...
Equivalent to `(run/port->list read-line pf redirects ...)`.
Returns when the port reaches at eof.

### Syntax: run/sexp pf redirects ...
Equivalent to `(read (run/port pf redirects ...))`.
Returns when `read` completes.

### Syntax: run/sexps pf redirects ...
Equivalent to `(run/port->list read pf redirects ...)`.
Returns when the port reaches at eof.

### Syntax: run/port+proc pf redirects ...
Runs a subprocess, and returns two values: an input port connected to
the subprocess's stdout, and its `<process>` object.
Returns immediately after forking subprocess.

### Syntax: run/ proc pf redirects ...
Generalized variant of `run/`*s.
Forks a subprocess with `(& pf redirects ...)`,
and applies `proc` to resulting `<process>` object.

For example, `run/port` is equivalent to
`(run/ process-output pf (> stdout) redirects ...)`.

### Syntax: run/collecting (fds ...) pf redirects ...
Runs a subprocess, redirects its output file descriptor `fds` ... to
temporary files, and returns multiple-values:
the exit status of the subprocess and input ports on the temporary files.
Returns when the subprocess exits.

### Syntax: && (% pf redirects ...) ...
### Syntax: || (% pf redirects ...) ...
### Syntax: && pf ...
### Syntax: || pf ...
### Auxiliary syntax: %
Analogous to `&&` and `||` in the shell.

`&&` runs each `pf` from left to right and immediately returns `#f`
when one `pf` abnormally exit,
i.e. `(sys-wait-exit-status (% pf redirects ...)) != 0`.
Any remaining `pf`s are not invoked.
If all the `pf`s successfully exit, returns `#t`.
If there are no `pf`s, then `#t` is returned.

`||` runs each `pf` from left to right and immediately returns `#t`
when one `pf` successfully exit.
Any remaining `pf`s are not invoked.
If all the `pf`s abnormally exit, returns `#f`.
If there are no `pf`s, then `#f` is returned.

In the case there are no need to specify `redirects`,
you may omit surrounding `(% ...)`.
