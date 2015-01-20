# Fly-2.0
Fly 2.0は限定継続をベースにしたスクリプト言語です．

## コンパイル・実行
Fly 2.0 をコンパイル・実行するには，ソースコードを clone してきて以下のコマンドを実行します．
```
$ make
$ ./fly2 test.fly
hello, world.
```
## チュートリアル

### hello, world.

以下は画面に `hello, world.` と表示して改行する Fly2 プログラムです．

```
(* hello.fly *)
writeln "hello, world.";
```

このプログラムを実行するには，上のプログラムを `hello.fly` という名前で保存し，
シェルを立ち上げて，
```
$ fly2 hello.fly
```
と入力します．

実行結果:
```
hello, world.
```
プログラムの説明をします．
 - １行目はコメントです．OCamlスタイルのコメントが利用可能で，コメントの入れ子も可能です．
 - 文字列を表示するには `write` 関数か `writeln` 関数を使います．
`write` は文字列を単に表示し，`writeln` は文字列を表示した後で改行します．
 - 文字列は内部では数値のリストとして表現されています．
 - 文の最後にはセミコロンをつけます．

### 変数と計算
変数に値を代入するには，
```
! 変数名 = 式;
```
という文を記述します．

以下は，底辺がa高さがbの三角形の面積を求めるプログラムです．
```
!a = 5;
!b = 3;
!s = a * b / 2;
println s;
```

実行結果:
```
7.5
```
- `println` 関数は任意の値を表示して改行する関数です．改行しない `print` 関数もあります．
- `write` と `print` の違いは，`write` が文字列専用，`print` は任意の値です．
- 変数名はアルファベットの小文字から始める必要があります．

### print と write

文字列を `print` 関数で表示すると，数値のリストが表示されます．
逆に，リストを `write` 関数に渡すと文字列が表示されます．

```
println "abc";
writeln [97, 98, 99];
```
実行結果:
```
[97, 98, 99]
abc
```
### シフトとブロック (1)

シフト式 (`$ 変数 -> 式`) は，ブロック (`{ 式 }`) で囲まれた範囲の継続を取り出して，
変数`k`に束縛します． 

```
!x = 1 + { 10 + $k -> k (k 100) };
println x;
```

実行結果:
```
121
```
### シフトとブロック (2)

継続を取り出して，単に捨てるだけの場合は，以下のように変数を省略できます．

```
!x = 1 + { 10 + $ -> 100 };
println x;
```

実行結果:
```
101
```
### タグ付きのシフトとブロック

シフトとブロックにはそれぞれタグ (`P`, `Q` など大文字で始まる文字列) をつけることができます．

```
!x = 1 + {P: 10 + {Q: 100 + $P: k -> k (k 1000)} };
println x;

!y = 1 + {P: 10 + {Q: 100 + $Q: k -> k (k 1000)} };
println x;
```

実行結果:
```
1221
1211
```
- ブロックのタグを省略した場合は，無名のタグが指定されます．
- シフトのタグを省略した場合は，最も内側に挿入されているタグが指定されます．

### 関数を定義する (1)
関数を定義するには，シフトとブロックを使って以下のように書きます．
```
!double = { ($ k -> k) * 2 };
println (double (double 3));
```

実行結果:
```
12
```

### 関数を定義する (2)
`($ k -> k)` は頻出のパターンなので，代わりに `$$` と書くことができます．
```
!mult = { $$ * 2 };
println (double (double 3));
```

実行結果:
```
12
```

### 関数を定義する (3)
複数の引数をとる関数は，複数 `$$` と書くことで定義できます．
```
!mult = { $$ * $$ };
println (mult 10 20);
```

実行結果:
```
200
```

### 関数を定義する (4)
引数を複数使う場合は，以下のように書きます．

```
!pow2 = { !x = $$; x * x };
println (pow2 12);
```

実行結果:
```
144
```
### 条件分岐
条件分岐をするには，
```
? 式;
```
という文を使います．`?`文は，式が真の場合は式の値を返し，偽の場合は，`($ -> v)` (v は式の評価結果)を実行します ．

```
!x = 10;
println { ? x > 0; 123 };
println { ? x > 10; 456 };
```
実行結果:
```
123
0
```
条件が偽の場合のタグを指定するには，
```
?P: 式;
```
のように記述します．

### パターンマッチ
パターンマッチをするには，
```
! パターン = 式;
```
という文を使います．`!`文は式を評価してパターンとマッチさせます．パターンマッチに失敗した場合は，`($ -> v)` (v は式の評価結果)を実行します．

```
!x = [1, 2, 3];
println { ! [] = x; 123 };
println { ! x :: xs = x; xs };
```
実行結果:
```
[1, 2, 3]
[2, 3]
```
パターンマッチに失敗した場合のタグを指定するには，
```
!P: パターン = 式;
```
のように記述します．

### 再帰
これまで出てきた構文を使うことで，不動点演算子を以下のように定義できます．
```
!fix = { !f = $$; { !x = $$; f { !y = $$; (x x) y } }
                  { !x = $$; f { !y = $$; (x x) y } } };
```

この不動点演算子を使うことで，階乗を計算する関数は以下のように記述できます．

```
!fact = fix { !f = $$; !n = $$;
  {R: {!0 = n; $R: -> 1} * f (n - 1)}
};

println (fact 5);
```

不動点演算子は`@`演算子として標準ライブラリで定義されているので，毎回自分で定義する必要はありません．

```
!fact = @ { !f = $$; !n = $$;
  {R: {!0 = n; $R: -> 1} * f (n - 1)}
};

println (fact 5);
```

実行結果:
```
120
```

### フィボナッチ数
以下は`n`番目のフィボナッチ数を求める関数の定義例です．
```
!fib = @ { !fib = $$; !n = $$;
  {IF:
    {? n = 0; $IF: -> 1};
    {? n = 1; $IF: -> 1};
    fib (n - 1) + fib (n - 2)
  }
};

println (fib 10);
```

実行結果:
```
89
```
### リストのマップ
リストのマップ関数の定義例です．
```
!map = @ { !map = $$; !f = $$; !lis = $$;
  {CASE:
    { ! [] = lis; $CASE: -> []};
    { ! hd :: tl = lis; f hd :: map f tl }
  }
};

println (map {$$*2} [1,2,3]);
```

実行結果:
```
[2, 4, 6]
```
### リストのプレフィックス
[浅井先生と亀山先生の論文](http://logic.cs.tsukuba.ac.jp/~kam/papers/aplas07.pdf)に載っている例．

```
!prefix = { !lst = $$;
  !visit = @ { !visit = $$; !lst = $$;
    {CASE:
      {! [] = lst; $P: -> []};
      {! hd :: tl = lst;
        hd :: ($P: k -> k [] :: {P: k (visit tl)})}
    }
  };
  {P: visit lst};
};

println (prefix [1,2,3,4,5]);
```

実行結果:
```
[[1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]
```

## リファレンスマニュアル
### プログラムの実行
```
$ fly2 src1.fly src2.fly ...
```
ソースファイル `src1.fly`, `src2.fly`, .. を順番に実行していきます．

### プログラムの構造
fly2 のプログラムはセミコロンで区切った文の列で表します．
```
文1;
文2;
...
文n;
```
最後の文`n`のセミコロンは省略できます．文には以下の3種類があります．
- 式文
- `!`文 (パターンマッチ文)
- `?`文 (条件判定文)

### コメント
```
(* コメント *)
```
`(*` と `*)` で囲まれた範囲はコメントとして無視されます．コメントは入れ子にできます．

### 式文
```
式;
```
式を実行して結果を現在の値に設定します．

### !文 (パターンマッチ文)
```
! パターン = 式;
! タグ : パターン = 式;
```
式を実行してパターンとマッチさせます．
マッチした変数は以降の文で使うことができます．

パターンマッチに成功した場合，式の値(`v`)を現在の値に設定します．

パターンマッチに失敗した場合，`($ -> v)` もしくは `($ タグ : -> v)` を実行します．

### パターン
パターンには以下のものがあります．
- ワイルドカードパターン(`_`)
- 変数パターン
- 数値リテラル
- orパターン (`p1 | p2`)
- asパターン (`p & x`)
- リストパターン (`[p1, p2, ...]`)
- コンスパターン (`p1 :: p2`)

### ?文 (条件判定文)
```
? 式;
? タグ : 式;
```
式を実行して値を`v`とすると，`v`が`0`または`v`が`[]`の場合，`($ -> v)` もしくは `($ タグ : -> v)` を実行します．

`v`が`0`以外かつ`v`が`[]`以外の場合，`v`を現在の値に設定します．

### 数値
数値は内部では倍精度の浮動小数点で表現されています．
数値リテラルとして，以下のような表記が可能です．
```
0
12
3.14
5e-3
```
### リスト
リストは値の列です．
リストリテラルは
```
[]
[1, 2, 3]
```
のように表記します．

### 文字列
文字列は内部では数値のリストです．
文字列リテラルは，
```
"abc"
```
のように表記します．上のリストは，
```
[97, 98, 99]
```
と同じです．

エスケープシーケンスとして，
|文字|意味|
|---|---|
|`\n`|改行|
|`\t`|タブ|
|`\r`|復帰|
|`\\`|`\`|
|`\"`|`"`|
が利用できます．

### 演算子
演算子には以下のものがあります．
|演算子|機能|
|---|---|
|`-e`|単項マイナス|
|`@e`|`e`の不動点|
|`e + e`|加算|
|`e - e`|減算|
|`e * e`|乗算|
|`e / e`|除算|
|`e < e`|より小さい|
|`e > e`|より大きい|
|`e <= e`|以下|
|`e >= e`|以上|
|`e = e`|等しい|
|`e <> e`|等しくない|
|`e :: e`|リストのコンス|

### シフト
シフト式には以下の形式があります．
```
$ -> 式
$ タグ: -> 式
$ 変数 -> 式
$ タグ: 変数 -> 式
```
シフトはブロックで囲まれた間の継続を取り出します．
タグを省略すると，現在の位置からもっとも内側のリセットが指定されたことになります．

### ブロック
ブロック文には以下の形式があります．
```
{ 文1; 文2; ... }
{タグ: 文1; 文2; .. }
```
ブロックはタグを文脈に挿入して，文を順番に実行していきます．
ブロック自体は最後の文の値を返します．
タグを省略すると，無名のタグが指定されたことになります．

### ライブラリ関数
|関数名|機能|
|---|---|
|`print e`|任意の値を表示する|
|`println e`|任意の値を表示して改行する|
|`write e`|文字列を表示する|
|`writeln e`|文字列を表示して改行する|
|`put e`|文字を1文字表示する|
|`get e`|文字を1文字取得する|
|`true`|値`1`|
|`false`|値`0`|
|`not e`|`e`が真(`0`でないまたは`[]`でない)の場合`1`を返し，それ以外の場合`0`を返す|
|`is_list e`|引数がリストの場合`1`それ以外の場合`0`|
|`is_num e`|引数が数値の場合`1`それ以外の場合`0`|
|`to_str e`|値を文字列に変換|
|`floor e`|数値を整数に切り捨て|
|`flush e`|出力をフラッシュ|
