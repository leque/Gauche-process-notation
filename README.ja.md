# これは何
[Scsh](http://www.scsh.net/) の Process Notation 風のプロセス起動構文
[Gauche](http://practical-scheme.net/gauche/) 用。
ただし Scsh との互換性はない。

## API
### Syntax: ! pf redirects ...
### Syntax: & pf redirects ...
### Syntax: % pf redirects ...
### Auxiliary syntax: ^
process form `pf` で指定された Unix プロセスを起動する。
& はサブプロセスを fork し、 <process> オブジェクトを返す。
! は exec して返らない。 % は起動したプロセスを
process-wait し、 process-exit-status した結果を返す。

pf の構文は以下の通り。

    pf ::= (cmd args ...)
         | ((cmd args ...) :key error directory sigmask detached host)
         | (^ pf0 pf1 ...)

最初の形式は run-process の第一引き数と同じ形式で起動するコマンドとその引き数を
指定する。

次の形式では、それに加えて run-process のキーワード引き数をいくつか指定できる。
:input, :output はパイプライン、もしくは !, &, % 等の redirects 引き数で
指定されるため、キーワード引き数として与えることはできないことに注意

以上のふたつの形式で、 (cmd args ...) の部分は暗黙に quasiquote される。
値を評価したい場合には unquote や unquote-splicing を使う。

最後の形式はパイプラインで、各 pf をパイプでつなぐ。 ^ という記号は
[Thompson shell](http://en.wikipedia.org/wiki/Thompson_shell) から取った。
^ は gauche モジュールで lambda の別名として束縛されているものを参照している。

redirects には gauche.process の run-process 手続きの :redirects と
同じ形式で入出力リダイレクトを指定できる。また、この構文の拡張として入出力リダイレクトの
最初の fd を省略することもできる。省略した場合には、入力リダイレクト
<、 <<、 <<<、 <& では 0 を指定したものとして扱い、出力リダイレクト
>、 >>、 >& では 1 を指定したものとして扱う。例えば、 (< "file") は
(< 0 "file") と同じであり、 (> :null) は (> 1 :null) と同じである。

redirects は暗黙に quasiquote される。

pf がパイプラインを表す場合には、 redirects の入力指定はパイプの左端の
プロセスへの入力の指定になり、出力指定は右端のプロセスへの出力指定になる。

### Syntax: run pf redirects ...
% の別名

### Syntax: run/port pf redirects ...
子プロセスを起動し、その子プロセスの標準出力に接続した入力ポートを返す。
子プロセスを起動したら即座に返る。

### Syntax: run/port->list reader pf redirects ...
(port->list reader (run/port pf redirects ...)) に同じ。
ポートが eof に到達した時点で返る。

### Syntax: run/file pf redirects ...
pf の標準出力を一時ファイルにリダイレクトし、 その一時ファイルの名前を返す。
子プロセスが終了した時点で返る。

### Syntax: run/string pf redirects ...
(port->string (run/port pf redirects ...)) に同じ。
ポートが eof に到達した時点で返る。

### Syntax: run/strings pf redirects ...
(run/port->list read-line pf redirects ...) に同じ。
ポートが eof に到達した時点で返る。

### Syntax: run/sexp pf redirects ...
(read (run/port pf redirects ...)) に同じ。
オブジェクトをひとつ読み込んだ時点で返る。

### Syntax: run/sexps pf redirects ...
(run/port->list read pf redirects ...) に同じ。
ポートが eof に到達した時点で返る。

### Syntax: run/port+proc pf redirects ...
子プロセスを起動し、その子プロセスの標準出力に接続した入力ポートと、
<process> オブジェクトとの二値を返す。

### Syntax: run/ proc pf redirects ...
run/* 構文の汎化版。 (& pf redirects ...) の返すプロセスオブジェクトに
proc を適用し、その戻り値を返す。

例えば、 run/port は (run/ process-output pf (> stdout) redirects ...)
と同じである。

### Syntax: run/collecting (fds ...) pf redirects ...
子プロセスを起動してその出力ファイルディスクリプタ fds ... を
一時ファイルにリダイレクトし、 <process> オブジェクトと、その一時ファイルを
読み込むための入力ポートを多値として返す。
子プロセスが終了した時点で返る。

### Syntax: && (% pf redirects ...) ...
### Syntax: || (% pf redirects ...) ...
### Syntax: && pf ...
### Syntax: || pf ...
### Auxiliary syntax: %
シェルの && と || に対応する。

&& は各 pf を左から順に実行し、いずれかの pf が異常終了した時点
（(sys-wait-exit-status (% pf redirects ...)) != 0 の時点）で #f を返す。
このとき、残りの式は評価されない。すべての pf が正常終了した場合には #t を返す。
pf ... が空の場合には #t を返す。

|| は各 pf を左から順に実行し、いずれかの pf が正常終了した時点で #t を返す。
このとき、残りの式は評価されない。すべての pf が異常終了した場合には #f を返す。
pf ... が空の場合には #f を返す。

redirects ... を指定しない場合には、簡略構文として (% _ redirecsts ...) を
省略できる。
