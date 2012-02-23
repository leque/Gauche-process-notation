# What's this?
Gauche-process-notation is a process notation library inspired by
[Scsh](http://www.scsh.net/).

This library is a thin wrapper to `run-process` procedure in
`gauche.process` module.

## Examples

    (use process.notation)

    ;;; download files
    (use srfi-1)

    (run `(wget ,@(map (pa$ format "http://www.example.com/img/~D.jpg")
                       (iota 10 1))))

    ;;; classical word frequency analysis
    (define freqs
      (map (lambda (s)
             (call-with-input-string s (cut port->list read <>)))
           (run/strings '(^ (wget -O - "http://example.org/licenses/gpl.txt"
                                  (:error :null))
                           (tr -c "A-Za-z" "\n")
                           (tr "A-Z" "a-z")
                           (grep -v "^$")
                           (sort)
                           (uniq -c)
                           (sort -rn)))))

## API
### Module: process.notation
#### Procedure: exec pf redirects ...
#### Procedure: run& pf redirects ...
#### Procedure: run pf redirects ...

Runs a command specified with the process form `pf`.

`run&` forks a subprocess, and returns a `<process>` object.

`exec` forks a subprocess, calls `sys-exec`, and never returns.

`run` forks a subprocess, waits until the subprocess terminates,
and returns `process-exit-status` of the subprocess.

`pf`'s syntax is:

    pf ::= (cmd-elems ...)
         | (^ pf0 pf1 ...)

    cmd-elems ::= iospec
                | keyword-arg
                | obj

    iospec ::= (symbol . rest)

    keyword-arg ::= (key . rest)

    key ::= :error | :directory | :host | :sigmask | :detached

The first form specifies a command, its arguments, redirects, and
keywords arguments to `run-process`. `iospec`s are
passed to `run-process`'s `:redirects` argument.
`keyword-arg`s are keyword arguments to `run-process`.
The rest elements are passed to `run-process` as `cmd&args` argument.

The next is a pipeline connects each `pf`s with pipes.
`^` is one of the pipeline notations of
[Thompson shell](http://en.wikipedia.org/wiki/Thompson_shell).

`redirects` specify how to redirect child process's IOs in the same format
as the `:redirects` arguments to `run-process` procedure.
As an extension of this library, `fd` part of the IO redirection are optional.
`fd`s for input redirections, `<`, `<<`, `<<<` and, `<&`, are defaults to `0`,
`fd`s for output redirections, `>`, `>>`, and `>&`, are defaults to `1`.
For example, `(< "file")` equals to `(< 0 "file")`, and
`(> :null)` equals to `(> 1 :null)`.

If `pf` is a pipeline, the input redirections in `redirects` are
for the left most subprocess in the pipeline,
the output redirections are for the right most subprocess.

#### Procedure: run/port pf redirects ...
Runs a subprocess, and returns an input port connected to
the subprocess's stdout.
Returns immediately after forking subprocess.

#### Procedure: run/port->list reader pf redirects ...
Equivalent to `(port->list reader (run/port pf redirects ...))`.
Returns when the port reaches at eof.

#### Procedure: run/file pf redirects ...
Runs a subprocess, redirects its stdout to temporary file,
and returns the name of the temporary file.
Returns when the subprocess exits.

#### Procedure: run/string pf redirects ...
Equivalent to `(port->string (run/port pf redirects ...))`.
Returns when the port reaches at eof.

#### Procedure: run/strings pf redirects ...
Equivalent to `(run/port->list read-line pf redirects ...)`.
Returns when the port reaches at eof.

#### Procedure: run/sexp pf redirects ...
Equivalent to `(read (run/port pf redirects ...))`.
Returns when `read` completes.

#### Procedure: run/sexps pf redirects ...
Equivalent to `(run/port->list read pf redirects ...)`.
Returns when the port reaches at eof.

#### Procedure: run/port+proc pf redirects ...
Runs a subprocess, and returns two values: an input port connected to
the subprocess's stdout, and its `<process>` object.
Returns immediately after forking subprocess.

#### Procedure: run/collecting fds pf redirects ...
Runs a subprocess, redirects its output file descriptors in the list `fds` to
temporary files, and returns multiple-values:
the exit status of the subprocess and input ports on the temporary files.
Returns when the subprocess exits.

#### Syntax: && (run pf redirects ...) ...
#### Syntax: || (run pf redirects ...) ...
#### Syntax: && pf ...
#### Syntax: || pf ...
#### Auxiliary syntax: run
Analogous to `&&` and `||` in the shell.

`&&` runs each `pf` from left to right and immediately returns `#f`
when one `pf` abnormally exit,
i.e. `(sys-wait-exit-status (run pf redirects ...)) != 0`.
Any remaining `pf`s are not invoked.
If all the `pf`s successfully exit, returns `#t`.
If there are no `pf`s, then `#t` is returned.

`||` runs each `pf` from left to right and immediately returns `#t`
when one `pf` successfully exit.
Any remaining `pf`s are not invoked.
If all the `pf`s abnormally exit, returns `#f`.
If there are no `pf`s, then `#f` is returned.

In the case there are no need to specify `redirects`,
you may omit surrounding `(run ...)`.

### Module: process.notation.syntactical
This module provides syntactical vaiants of procedures in `process.notation`.

These syntaxes implicitly `quasiquote` `pf` and `redirects`.
So you should `unquote` or `unquote-splicing` subforms to evaluate it.

The pipeline symbol `^` is matched hygienically.
`^` is the same identifier as one bound in `gauche` module.

#### Procedure: exec pf redirects ...
#### Procedure: run& pf redirects ...
#### Procedure: run pf redirects ...
#### Auxiliary syntax: ^
#### Procedure: run pf redirects ...
#### Procedure: run/port pf redirects ...
#### Procedure: run/port->list reader pf redirects ...
#### Procedure: run/file pf redirects ...
#### Procedure: run/string pf redirects ...
#### Procedure: run/strings pf redirects ...
#### Procedure: run/sexp pf redirects ...
#### Procedure: run/sexps pf redirects ...
#### Procedure: run/port+proc pf redirects ...
#### Procedure: run/collecting (fds ...) pf redirects ...
#### Syntax: && (run pf redirects ...) ...
#### Syntax: || (run pf redirects ...) ...
#### Syntax: && pf ...
#### Syntax: || pf ...
#### Auxiliary syntax: run
