#!/bin/bash

# This script executes all vim-erlang tests.
#
# Usage:
#
#     test.sh
#
# Debugging: add "set -x" to enable bash's verbose mode.

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
    grep_pattern=$4

    echo "  - Directory ${base_dir}"

    # Run "erlang_complete.erl list-modules".
    #
    # We filter for lines that start with "my" because we are not interested in
    # the modules provided by Erlang/OTP. The list of Erlang/OTP modules
    # depends on the Erlang/OTP installation, which would make the test
    # nondeterministic.
    echo "    - list-modules"
    "$main_dir/vim-erlang-omnicomplete/autoload/erlang_complete.erl" \
        --basedir "${base_dir}" list-modules | \
        grep "$grep_pattern" \
        > "${result_dir}/${result_prefix}%list-modules" || true

    # Run "erlang_complete.erl list-functions <module>".
    #
    # grep2: The lines that contain @spec deprecation warning are filtered out.
    # They are produced only when the Erlang/OTP version is at least 24. It is
    # possible that users use Erlang/OTP 24 and still use some old code, so
    # there would not be much point in complaining about @spec. Thus,
    # `erlang_complete.vim` ignores these warnings.
    modules=$(find "${module_dir}" -type f -name '*.erl' |
              sed 's/^.*\/\([A-Za-z0-9_]*\)\.erl$/\1/g' |
              sort -u)
    for module in ${modules}; do
        echo "    - list-functions for module ${module}"
        "$main_dir/vim-erlang-omnicomplete/autoload/erlang_complete.erl" \
            --basedir "${base_dir}" list-functions "${module}" \
            | grep -v 'warning: EDoc @spec tags are deprecated. Please use -spec attributes instead.' \
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
(cd "${fixture_dir}/errors/edoc_error"; rebar3 compile)

#-------------------------------------------------------------------------------
# vim-erlang-compiler
#-------------------------------------------------------------------------------

echo "Testing vim-erlang-compiler..."

erlang_check="$main_dir/vim-erlang-compiler/compiler/erlang_check.erl"

# Test successful compilations.
#
# Compile every *.erl file.
#
# This includes the fixtures and the vim-erlang scripts.

for src_file in $(find "${fixture_dir}/rebar3_app" \
                       "${fixture_dir}/rebar3_release" \
                       -type f -name '*.erl'); do
    "${erlang_check}" "${src_file}"
done

# Test unsuccessful compilations.
#
# 1.  sed: Remove the absolute path to the module.
#
# 2.  sed: Remove the column numbers because that is avaliable only from
#     Erlang/OTP 24.
"${erlang_check}" \
    "${fixture_dir}/errors/compilation_error/src/compilation_error.erl" \
    | sed 's/^.*\(compilation_error.erl:\)/\1/' \
    | sed 's/\(:[0-9][0-9]*:\)[0-9][0-9]*:/\1/' \
    > "${result_dir}/errors%compilation_error%compile%compilation_error" || true

"${erlang_check}" \
    "${fixture_dir}/errors/rebar_config_error/src/my_lib.erl" \
    > "${result_dir}/errors%rebar_config_error%compile%my_lib" 2>&1 || true

#-------------------------------------------------------------------------------
# vim-erlang-omnicomplete
#-------------------------------------------------------------------------------

echo "Testing vim-erlang-omnicomplete..."

# 1. Test completion for every source directory.
#
#    Every module is tried as a completion target.

test-vim-erlang-omnicomplete \
    "${fixture_dir}/rebar3_app/mylib/src" \
    "${fixture_dir}/rebar3_app" \
    "rebar3_app%mylib" \
    "^my"

test-vim-erlang-omnicomplete \
    "${fixture_dir}/rebar3_release/myapp/apps/mylib/src" \
    "${fixture_dir}/rebar3_release/myapp" \
    "rebar3_release%mylib" \
    "^my"

test-vim-erlang-omnicomplete \
    "${fixture_dir}/rebar3_release/myapp/apps/myapp/src" \
    "${fixture_dir}/rebar3_release/myapp" \
    "rebar3_release%myapp" \
    "^my"

# We redirect the standard error to /dev/null because 
# erlang_complete.erl would print "rebar.config consult failed" messages to the
# standard error.
test-vim-erlang-omnicomplete \
    "${fixture_dir}/errors/rebar_config_error/src" \
    "${fixture_dir}/errors/rebar_config_error" \
    "errors%rebar_config_error" \
    "" 2>/dev/null

test-vim-erlang-omnicomplete \
    "${fixture_dir}/errors/edoc_error/src" \
    "${fixture_dir}/errors/edoc_error/src" \
    "errors%edoc_error" \
    "^my"

module=no_such_module
"$main_dir/vim-erlang-omnicomplete/autoload/erlang_complete.erl" \
    --basedir "${fixture_dir}/rebar3_app/mylib/src" \
    list-functions "${module}" \
    > "${result_dir}/rebar3_app%mylib%list-functions%${module}" || true

# 2. Test that the comments in my_complete.erl are accurate.

expected_file=${fixture_dir}_expected
actual_file="${result_dir}/rebar3_app%mylib%list-functions%my_complete"

# Create ${expected_file}:
#
# - Go through "my_complete.erl"
# - Don't print anything unless asked ("-n")
# - Select the lines that start with "%     complete_"
# - Remove the "%     " part from the beginning
# - Print the rest to ${expected_file}
sed -n '/^%     complete_/{s/^%     //;p;}' \
    "${fixture_dir}/rebar3_app/mylib/src/my_complete.erl" \
    > "${expected_file}"

# Temporarily disable "pipefail" because diff will have a non-zero exit status.
set +o pipefail

# Write the diff between the excepted and actual completion list into a diff
# file.
#
# -   grep #1: The lines starting with a number are filtered out because they
#     print information about line numbers (which is only noise for us now).
#
# -   grep #2: The lines that contain @spec deprecation warning are filtered
#     out. They are produced only when the Erlang/OTP version is at least 24.
#     It is possible that users use Erlang/OTP 24 and still use some old code,
#     so there would not be much point in complaining about @spec. Thus,
#     `erlang_complete.vim` ignores these warnings.
diff "${expected_file}" "${actual_file}" \
    | grep -v '^[0-9]' \
    | grep -v 'warning: EDoc @spec tags are deprecated. Please use -spec attributes instead.' \
    > "${result_dir}/my_complete-omnicomplete.diff"

set -o pipefail

rm "${expected_file}"

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

# Test a rebar with the _build directory.

(cd "${fixture_dir}/rebar3_app/mylib"
 vim-erlang-tags --follow
 mv tags "${result_dir}/rebar3_app%mylib%tags-follow")

(cd "${fixture_dir}/rebar3_app"
 vim-erlang-tags --follow mylib
 mv tags "${result_dir}/rebar3_app%mylib%tags-follow-outside")
