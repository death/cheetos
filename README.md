# cheetos

CHEETOS is a tool and a library that can record and compare results of
user-defined benchmarks at different stages in the process of
optimization.

The code is in a very early state, and exists as an accessory to help
in development of another project.

There is no documentation, and the interface may change at any time.
It may or may not work for you, but I am open to patches.

For now, to create the initial database, you can go to the "cheetos"
directory in your XDG data directory (see value of
`cheetos/persist::*cheetos-db-pathname*`) and type this in your shell:

```sh
$ sqlite3 cheetos.db < /path/to/cheetos/db/create.sql
```

To see how to define and run benchmarks, look at file `tests.lisp`.

To start the tool, load system `cheetos-ui` and call function
`cheetos/ui:cheetos`.

# license

MIT
