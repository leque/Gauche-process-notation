# これは何
[Scsh](http://www.scsh.net/) の Process Notation 風のプロセス起動構文
[Gauche](http://practical-scheme.net/gauche/) 用。
ただし Scsh との互換性はない。

## 使用例

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
process form `pf` で指定された Unix プロセスを起動する。
`run&` はサブプロセスを `fork` し、 `<process>` オブジェクトを返す。
`exec` は `sys-exec` して返らない。 `run` は起動したプロセスを
`process-wait` し、 `process-exit-status` した結果を返す。

`pf` の構文は以下の通り。

    pf ::= (cmd-elems ...)
         | (^ pf0 pf1 ...)

    cmd-elems ::= iospec
                | keyword-arg
                | obj

    iospec ::= (symbol . rest)

    keyword-arg ::= (key . rest)

    key ::= :error | :directory | :host | :sigmask | :detached

最初の形式ではコマンドとその引き数、リダイレクト、 `run-process` 手続きへの
キーワード引き数を指定する。 `iospec` は `run-process` 手続きの
`:redirects` 引き数になり、 `keyword-arg` はキーワード引き数になる。
それらをリストから取り除いたものが `run-process` の `cmd&args` 引き数になる。

二番目の形式はパイプラインで、各 `pf` をパイプでつなぐ。 `^` という記号は
[Thompson shell](http://en.wikipedia.org/wiki/Thompson_shell) から取った。

`redirects` には `gauche.process` の `run-process` 手続きの `:redirects` と
同じ形式で入出力リダイレクトを指定できる。また、この構文の拡張として入出力リダイレクトの
最初の `fd` を省略することもできる。省略した場合には、入力リダイレクト
`<`、 `<<`、 `<<<`、 `<&` では `0` を指定したものとして扱い、出力リダイレクト
`>`、 `>>`、 `>&` では `1` を指定したものとして扱う。例えば、 `(< "file")` は
`(< 0 "file")` と同じであり、 `(> :null)` は `(> 1 :null)` と同じである。

`pf` がパイプラインを表す場合には、 `redirects` の入力指定はパイプの左端の
プロセスへの入力の指定になり、出力指定は右端のプロセスへの出力指定になる。

#### Procedure: run/port pf redirects ...
子プロセスを起動し、その子プロセスの標準出力に接続した入力ポートを返す。
子プロセスを起動したら即座に返る。

#### Procedure: run/port->list reader pf redirects ...
`(port->list reader (run/port pf redirects ...))` に同じ。
ポートが eof に到達した時点で返る。

#### Procedure: run/file pf redirects ...
`pf` の標準出力を一時ファイルにリダイレクトし、 その一時ファイルの名前を返す。
子プロセスが終了した時点で返る。

#### Procedure: run/string pf redirects ...
`(port->string (run/port pf redirects ...))` に同じ。
ポートが eof に到達した時点で返る。

#### Procedure: run/strings pf redirects ...
`(run/port->list read-line pf redirects ...)` に同じ。
ポートが eof に到達した時点で返る。

#### Procedure: run/sexp pf redirects ...
`(read (run/port pf redirects ...))` に同じ。
オブジェクトをひとつ読み込んだ時点で返る。

#### Procedure: run/sexps pf redirects ...
`(run/port->list read pf redirects ...)` に同じ。
ポートが eof に到達した時点で返る。

#### Procedure: run/port+proc pf redirects ...
子プロセスを起動し、その子プロセスの標準出力に接続した入力ポートと、
`<process>` オブジェクトとの二値を返す。

#### Procedure: run/collecting fds pf redirects ...
子プロセスを起動してその出力ファイルディスクリプタのうちリスト `fds` で指定されたものを
一時ファイルにリダイレクトし、 `<process>` オブジェクトと、その一時ファイルを
読み込むための入力ポートを多値として返す。
子プロセスが終了した時点で返る。

#### Syntax: && (run pf redirects ...) ...
#### Syntax: || (run pf redirects ...) ...
#### Syntax: && pf ...
#### Syntax: || pf ...
#### Auxiliary syntax: run
シェルの `&&` と `||` に対応する。

`&&` は各 `pf` を左から順に実行し、いずれかの `pf` が異常終了した時点
（`(sys-wait-exit-status (run pf redirects ...))` ≠ `0` の時点）で `#f` を返す。
このとき、残りの式は評価されない。すべての `pf` が正常終了した場合には `#t` を返す。
`pf` ... が空の場合には `#t` を返す。

`||` は各 `pf` を左から順に実行し、いずれかの `pf` が正常終了した時点で `#t` を返す。
このとき、残りの式は評価されない。すべての `pf` が異常終了した場合には `#f` を返す。
`pf` ... が空の場合には `#f` を返す。

`redirects` ... を指定しない場合には、簡略構文として外側の
`(run ...)` を省略できる。

### Module: process.notation.syntactical
このモジュールでは `process.notation` モジュールの手続きの構文版を提供する。

これらの構文では `pf` や `redirecs` は暗黙に `quasiquote` される。
サブフォームを評価したい場合には `unquote` や `unquote-splicing` を使う。

また、これらの構文は衛生的なマクロとして定義されている。
パイプラインを表す識別子 `^` は `gauche` モジュールで束縛されているものと同じである。

#### Syntax: exec pf redirects ...
#### Syntax: run& pf redirects ...
#### Syntax: run pf redirects ...
#### Auxiliary syntax: ^
#### Syntax: run/port pf redirects ...
#### Syntax: run/port->list reader pf redirects ...
#### Syntax: run/file pf redirects ...
#### Syntax: run/string pf redirects ...
#### Syntax: run/strings pf redirects ...
#### Syntax: run/sexp pf redirects ...
#### Syntax: run/sexps pf redirects ...
#### Syntax: run/port+proc pf redirects ...
#### Syntax: run/collecting (fds ...) pf redirects ...
#### Syntax: && (run pf redirects ...) ...
#### Syntax: || (run pf redirects ...) ...
#### Syntax: && pf ...
#### Syntax: || pf ...
#### Auxiliary syntax: run
