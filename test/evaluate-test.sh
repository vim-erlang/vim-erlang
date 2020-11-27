#!/bin/bash

# Evaluate whether the previously executed test was successful.

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -eu
set -o pipefail
IFS=$'\n\t'
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

TEST_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
MAIN_DIR="$( dirname "$TEST_DIR" )"
cd "${MAIN_DIR}"

if [ "$(git status -s | grep " test/result/")" != "" ]; then
    echo
    echo "-> Test failed. The following file(s) changed:"
    echo
    git status -s | grep " test/result/"
    exit
else
    echo "-> Tests passed."
fi
