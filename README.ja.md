# Gauche-process-notation [![Build Status](https://travis-ci.org/leque/Gauche-process-notation.svg?branch=master)](https://travis-ci.org/leque/Gauche-process-notation)

[Scsh](http://www.scsh.net/) の Process Notation 風のプロセス起動構文を
[Gauche](http://practical-scheme.net/gauche/) で実装したものです。
Scsh との互換性はありません。

## Requirement

* [Gauche](http://practical-scheme.net/gauche/) 0.9.4 or later

## 使用例

```scheme
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
```

## API
### Module: process.notation
#### Procedure: exec pf redirects ...
#### Procedure: run& pf redirects ...
#### Procedure: run pf redirects ...
process form `pf` で指定された Unix プロセスを起動します。
`run&` はサブプロセスを `fork` し、 `<process>` オブジェクトを返します。
`exec` は `sys-exec` して返りません。 `run` は起動したプロセスを
`process-wait` し、 `process-exit-status` した結果を返します。

`pf` の構文は以下の通りです。

```
pf ::= (cmd-elems ...)
     | (^ pf0 pf1 ...)

cmd-elems ::= iospec
            | keyword-arg
            | obj

iospec ::= (symbol . rest)

keyword-arg ::= (key . rest)

key ::= :error | :directory | :host | :sigmask | :detached
```

最初の形式ではコマンドとその引数、リダイレクト、 `run-process` 手続きへの
キーワード引数を指定します。 `iospec` は `run-process` 手続きの
`:redirects` 引数になり、 `keyword-arg` はキーワード引数になります。
それらをリストから取り除いたものが `run-process` の `cmd&args` 引数に
なります。

二番目の形式はパイプラインで、各 `pf` のプロセスをパイプでつなぎます。
`^` という記号は
[Thompson shell](http://en.wikipedia.org/wiki/Thompson_shell) から取られました。

`redirects` には `gauche.process` の `run-process` 手続きの `:redirects` と
同じ形式で入出力リダイレクトを指定します。また、この構文の拡張として
入出力リダイレクトの最初の `fd` を省略することができます。
省略した場合には、入力リダイレクト `<`、 `<<`、 `<<<`、 `<&` では
`0` （標準入力）を指定したものとして扱い、出力リダイレクト
`>`、 `>>`、 `>&` では `1` （標準出力）を指定したものとして扱います。
例えば、 `(< "file")` は `(< 0 "file")` と同じで意味になり、
`(> :null)` は `(> 1 :null)` と同じ意味になります。

`pf` がパイプラインを表す場合には、 `redirects` の入力指定はパイプの左端の
プロセスへの入力を指定し、出力指定は右端のプロセスへの出力指定になります。

#### Procedure: run/port pf redirects ...
子プロセスを起動し、その子プロセスの標準出力に接続した入力ポートを返します。
子プロセスを起動したら即座に返ります。

#### Procedure: run/port->list reader pf redirects ...
`(port->list reader (run/port pf redirects ...))` に同じです。
ポートが eof に到達した時点で返ります。

#### Procedure: run/file pf redirects ...
`pf` の標準出力を一時ファイルにリダイレクトし、
その一時ファイルの名前を返します。
子プロセスが終了した時点で返ります。

#### Procedure: run/string pf redirects ...
`(port->string (run/port pf redirects ...))` に同じです。
ポートが eof に到達した時点で返ります。

#### Procedure: run/strings pf redirects ...
`(run/port->list read-line pf redirects ...)` に同じです。
ポートが eof に到達した時点で返ります。

#### Procedure: run/sexp pf redirects ...
`(read (run/port pf redirects ...))` に同じです。
オブジェクトをひとつ読み込んだ時点で返ります。

#### Procedure: run/sexps pf redirects ...
`(run/port->list read pf redirects ...)` に同じです。
ポートが eof に到達した時点で返ります。

#### Procedure: run/port+proc pf redirects ...
子プロセスを起動し、その子プロセスの標準出力に接続した入力ポートと、
`<process>` オブジェクトとの二値を返す。
子プロセスを起動したら即座に返ります。

#### Procedure: run/collecting fds pf redirects ...
子プロセスを起動して、その出力ファイルディスクリプタのうちリスト
`fds` で指定されたものを一時ファイルにリダイレクトし、
子プロセス終了ステータスと、一時ファイルを読み込むための入力ポートを
多値として返します。返り値の個数は `(+ 1 (length fds))` 個です。
子プロセスが終了した時点で返ります。

#### Syntax: && (run pf redirects ...) ...
#### Syntax: || (run pf redirects ...) ...
#### Syntax: && pf ...
#### Syntax: || pf ...
#### Auxiliary syntax: run
シェルの `&&` と `||` に対応します。

`&&` は各 `pf` を左から順に実行し、いずれかの `pf` が異常終了した時点
（`(sys-wait-exit-status (run pf redirects ...))` ≠ `0` の時点）で
`#f` を返します。このとき、残りの式は評価されません。
すべての `pf` が正常終了した場合には `#t` を返します。
`pf` ... が空の場合には `#t` を返します。

`||` は各 `pf` を左から順に実行し、いずれかの `pf` が正常終了した時点で
`#t` を返します。このとき、残りの式は評価されません。
すべての `pf` が異常終了した場合には `#f` を返します。
`pf` ... が空の場合には `#f` を返します。

`redirects` ... を指定しない場合には、簡略構文として外側の
`(run ...)` を省略できます。
