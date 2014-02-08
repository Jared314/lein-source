# lein-source

Edit Clojure forms, not Clojure files.

<img align="center" src="/diagram.png?raw=true" alt="Dataflow Diagram" />

## Install

User-level:

Put `[lein-source "0.3.0"]` into the `:plugins` vector of your
`:user` profile.

Project-level:

Put `[lein-source "0.3.0"]` into the `:plugins` vector of your project.clj.

## Source Task

The source task will pull the project configuration from a locations other than your local project.clj file.

    $ lein source --file project2.txt, repl

    $ lein source --string "(defproject test1 \"0.1.0\" :description \"Test project\")", repl

    $ lein source --url "https://raw.github.com/technomancy/leiningen/master/project.clj", repl

    $ lein source --git ., repl

    $ lein source --git . 36c5da189d50, repl

    $ lein source --git ~/Desktop/project1 HEAD test.txt, repl

    $ cat test.txt | lein source --stdin, run

Note: Mixing `--stdin` and `repl` is not supported.


Chained tasks are allowed using the same syntax as `lein do`.

    $ lein source --file project2.txt, clean, test foo.test-core, jar

## Base Task

The base task will read/write clojure forms, from the input stream or nrepl, into the correct file and directory structure, based on the specified namespaces.

Piped Input

    $ echo "stuff.core" | lein base .

    $ echo "stuff.core/thing" | lein base .

    $ echo "(ns stuff.core) (defn thing [] true)" | lein base .

"Headless" nREPL Server Input

    $ lein base . --nrepl

    $ lein base . --nrepl :port 1234

Chained tasks are allowed using the same syntax as `lein do`.

    $ echo "(ns stuff.core) (defn thing [] true)" | lein base ., jar

## License

Copyright © 2013 Jared Lobberecht

Distributed under the Eclipse Public License version 1.0.
