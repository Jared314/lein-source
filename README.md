# lein-source

A Leiningen plugin to pull project configuration from different locations.

## Install

User-level:

Put `[lein-source "0.1.0-SNAPSHOT"]` into the `:plugins` vector of your
`:user` profile.

Project-level:

Put `[lein-source "0.1.0-SNAPSHOT"]` into the `:plugins` vector of your project.clj.

## Usage

    $ lein source --file project2.clj, run

    $ lein source --file test.txt, repl

## License

Copyright © 2013 Jared Lobberecht

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
