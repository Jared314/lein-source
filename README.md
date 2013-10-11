# lein-source

A Leiningen v2.0 plugin to pull project configuration from different locations.

## Install

User-level:

Put `[lein-source "0.1.0-SNAPSHOT"]` into the `:plugins` vector of your
`:user` profile.

Project-level:

Put `[lein-source "0.1.0-SNAPSHOT"]` into the `:plugins` vector of your project.clj.

## Usage

    $ lein source --file project2.txt, repl

    $ lein source --string "(defproject test1 \"0.1.0\" :description \"Test project\")", repl

    $ lein source --url "https://raw.github.com/technomancy/leiningen/master/project.clj", repl

    $ cat test.txt | lein source --stdin, repl

    $ lein source --git ., repl

    $ lein source --git . 36c5da189d50, repl

    $ lein source --git ~/Desktop/project1 HEAD test.txt, repl

Chained tasks are allowed using the same syntax as `lein do`.

    $ lein source --file project2.txt, clean, test foo.test-core, jar

## License

Copyright © 2013 Jared Lobberecht

Distributed under the Eclipse Public License either version 1.0.
