#! /usr/bin/env nix-shell
#! nix-shell ../default.nix --pure -i bash

set -euxo pipefail

case "$1" in
    "test")
        sbt ';+clean;test'
        ;;
    "coverage")
        sbt coverage test coverageReport
        bash <(curl -s https://codecov.io/bash)
        ;;
    "readme")
        sbt tut
        git diff --exit-code -- *.md
        ;;
    *)
        echo "no command specified!"
        exit 1
        ;;
esac
