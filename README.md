# purescript_book

[実例によるpurescript][pure]の各章の実装です。すでに[githubリポジトリ][pure_git]に公式のものがありますが演習の答えが(多分)なかったので追加してあります。教材の関係でpurescript 0.11でやってますが一通り終わったら0.12でもやりたいって思ってます。

# Installation

- Node.js v10.15.0
- npm v6.7.0
- pulp 12.3.1
- purescript 0.11.7
- psc-package 0.5.1

はすでにあるものと思っています。

```
$ git clone git@github.com:tabata0330/purescript_book.git
```

クローンして

```
$ cd purescript_book
$ psc-package updates
$ pulp --psc-package run
```

で動くと思いますが、書き換えたりしたら

```
$ pulp test
```

で動作確認したり

```
$ pulp repl
> import Data.AddressBook
> ...
```

で挙動確認するのが手っ取り早いと思います。

[pure]:http://aratama.github.io/purescript/
[pure_git]:https://github.com/paf31/purescript-book