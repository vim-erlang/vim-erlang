# vim-erlang

This is a repository for the common documentation and tests of the vim-erlang
repositories.

## Tests

Use `make` to run the tests:

```
$ make test
git submodule update --init
Submodule 'vim-erlang-tags' (git@github.com:vim-erlang/vim-erlang-tags.git) registered for path 'vim-erlang-tags'
Cloning into '.../vim-erlang/vim-erlang-tags'...
Submodule path 'vim-erlang-tags': checked out 'd84dd4026b3bdba88d6ec35ad0ecfeca9c80afc7'
test/run-tests.sh
test/evaluate-test.sh
-> Tests passed.
```

The tested vim-erlang projects are downloaded as git submodules.

`test.sh` executes the tests and places their results in the `test/result`
directory. The expected results are checked in. If the content changes (i.e.,
the tests produce a result other than the expected result), then the test fails.

## FAQ

### Which repositories are maintained?

The following vim-erlang repositories are maintained:

- [vim-erlang-runtime](https://github.com/vim-erlang/vim-erlang-runtime)
- [vim-erlang-compiler](https://github.com/vim-erlang/vim-erlang-compiler)
- [vim-erlang-omnicomplete](https://github.com/vim-erlang/vim-erlang-omnicomplete)
- [vim-erlang-tags](https://github.com/vim-erlang/vim-erlang-tags)


### Why is no proper test framework used?

To keep it simple.

## Contributing

### Coding conventions

*   Follow Ericsson's [Programming Rules and
    Conventions](http://www.erlang.se/doc/programming_rules.shtml) where
    sensible/possible.

*   **The order of the functions should be top-down.**

    The "Utility functions" block constitute an exception to this rule, since
    they are mostly independent from each other, and it wouldn't be too helpful
    for their order to reflect the order in which they are used in the business
    logic. So they are ordered alphabetically.

    Example for top-down ordering:

    ```
    f() ->
        g(),
        h().

    g() ->
       gg().

    gg() ->
        ok.

    h() ->
        ok.
    ```

*   **Don't write lines longer than 80 characters.**

    Except if there is a very good reason (such as a long URL).

*   **Every Erlang function should have a docstring and a type spec.**

    The docstrings and type specs should follow the following format:

    ```
    %%------------------------------------------------------------------------------
    %% @doc Print stuff.
    %%
    %% Optional extra information about how the function works. You can use
    %% Edoc's syntax such as {@link other_function/0} (even though we don't
    %% generate HTML from this docstring).
    %% @end
    %%------------------------------------------------------------------------------
    -spec print(Param1, Param2) -> Result when
          Param1 :: [...],
          Param2 :: [...],
          Result :: [...].
    print(Param1, Param2) ->
    ```

    Note:

    -   One liner explanation in imperative mood.

    -   Each type parameter has a name.

*   **Every Erlang type spec should have a docstring.**

    The type spec's docstring should follow the following format:

    ```
    -type my_type() :: [...].
    %% One-liner description.
    %%
    %% Optional further information.
    ```
