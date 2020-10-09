#!/bin/bash

# This script executes all vim-erlang tests.
#
# Usage:
#
#     test.sh

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -eu
set -o pipefail
IFS=$'\n\t'

test_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
main_dir="$( dirname "$test_dir" )"
fixture_dir="$test_dir/fixture"
result_dir="$test_dir/result"

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------

function vim-erlang-tags
{
    $main_dir/vim-erlang-tags/bin/vim_erlang_tags.erl "$@"
}

#-------------------------------------------------------------------------------
# Preparation
#-------------------------------------------------------------------------------

# Compile the project: both vim-erlang-compiler and vim-erlang-omnicomplete
# need at least one "rebar3 compile".
(cd "${fixture_dir}/rebar3_app/mylib"; rebar3 compile)
(cd "${fixture_dir}/rebar3_release/myapp"; rebar3 compile)

#-------------------------------------------------------------------------------
# vim-erlang-compiler
#-------------------------------------------------------------------------------

echo "Testing vim-erlang-compiler..."

# Compile every *.erl file.
#
# This includes the fixtures and the vim-erlang scripts.
 
for src_file in $(find -type f -name '*.erl'); do
    "$main_dir/vim-erlang-compiler/compiler/erlang_check.erl" "${src_file}"
done

#-------------------------------------------------------------------------------
# vim-erlang-tags
#-------------------------------------------------------------------------------

echo "Testing vim-erlang-tags..."

# Test a rebar application.

(cd "${fixture_dir}/rebar3_app/mylib"
 vim-erlang-tags
 mv tags "${result_dir}/rebar3_app%mylib%tags")

# Test that symbolic links are not followed without the "--follow" option.

(cd "${fixture_dir}/rebar3_release"
 vim-erlang-tags \
     --ignore "*/_build" \
     --output "${result_dir}/rebar3_release%tags-no-follow")

# Test that symbolic links are followed with the "--follow" option.

(cd "${fixture_dir}/rebar3_release"
 vim-erlang-tags \
     --ignore "*/_build" \
     --output "${result_dir}/rebar3_release%tags-follow" --follow)

# Test a rebar release.

(cd "${fixture_dir}/rebar3_release/myapp"
 vim-erlang-tags --ignore _build
 mv tags "${result_dir}/rebar3_release%myapp%tags")
