# vim-erlang

This is a repository for the common documentation and tests of the vim-erlang
repositories.

## FAQ

### Which repositories are maintained?

The following vim-erlang repositories are maintained:

- [vim-erlang-runtime](https://github.com/vim-erlang/vim-erlang-runtime)
- [vim-erlang-compiler](https://github.com/vim-erlang/vim-erlang-compiler)
- [vim-erlang-omnicomplete](https://github.com/vim-erlang/vim-erlang-omnicomplete)
- [vim-erlang-tags](https://github.com/vim-erlang/vim-erlang-tags)

## Contributing

### Coding conventions

*   Follow Ericsson's [Programming Rules and
    Conventions](http://www.erlang.se/doc/programming_rules.shtml) where
    sensible/possible.

*   **The order of the functions should be top-down.**

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
