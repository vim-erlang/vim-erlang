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

# Run vim-erlang-omnicomplete on the given base directory ($1):
#
# 1.  Run "list-modules" and write the result into a
#     "<result-prefix>%list-modules" output file.
#
# 2.  Run "list-functions" on all modules found in the given module directory
#    ($2). Write the result into a "<result-prefix>%list-functions%<module>"
#    output file.
function test-vim-erlang-omnicomplete
{
    base_dir=$1
    module_dir=$2
    result_prefix=$3

    echo "  - Directory ${base_dir}"

    # Run "erlang_complete.erl list-modules".
    echo "    - list-modules"
    "$main_dir/vim-erlang-omnicomplete/autoload/erlang_complete.erl" \
        --basedir "${base_dir}" list-modules | \
        grep "^my" \
        > "${result_dir}/${result_prefix}%list-modules" || true

    # Run "erlang_complete.erl list-functions <module>".
    modules=$(find "${module_dir}" -type f -name '*.erl' |
              sed 's/^.*\/\([A-Za-z0-9_]*\)\.erl$/\1/g' |
              sort -u)
    for module in ${modules}; do
        echo "    - list-functions for module ${module}"
        "$main_dir/vim-erlang-omnicomplete/autoload/erlang_complete.erl" \
            --basedir "${base_dir}" list-functions "${module}" \
            > "${result_dir}/${result_prefix}%list-functions%${module}" || true
    done
}

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
 
for src_file in $(find . -type f -name '*.erl'); do
    "$main_dir/vim-erlang-compiler/compiler/erlang_check.erl" "${src_file}"
done

#-------------------------------------------------------------------------------
# vim-erlang-omnicomplete
#-------------------------------------------------------------------------------

echo "Testing vim-erlang-omnicomplete..."

# Test completion for every source directory. Every module is tried as a
# completion target.

test-vim-erlang-omnicomplete \
    "${fixture_dir}/rebar3_app/mylib/src" \
    "${fixture_dir}/rebar3_app" \
    "rebar3_app%mylib"

test-vim-erlang-omnicomplete \
    "${fixture_dir}/rebar3_release/myapp/apps/mylib/src" \
    "${fixture_dir}/rebar3_release/myapp" \
    "rebar3_release%mylib"

test-vim-erlang-omnicomplete \
    "${fixture_dir}/rebar3_release/myapp/apps/myapp/src" \
    "${fixture_dir}/rebar3_release/myapp" \
    "rebar3_release%myapp"

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
