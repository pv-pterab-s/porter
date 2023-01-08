#!/bin/bash
# -*- compile-command: "./harness-function.sh rotate"; -*-
set -e

if [ -z "$1" ]; then echo "usage: harness-function.sh <function name>"; fi
if [ ! -d "out/$1" ]; then git clone gh:pv-pterab-s/new-dev out/$1; fi

rm -f out/$1/lookup.hpp
ln -sf $(readlink -f out/af/src/backend/oneapi/kernel/$1.hpp) $(readlink -f out/$1/$1.hpp)

cat > out/af/.dir-locals.el <<EOF
((c++-mode . ((compile-command . "cd $(readlink -f out/$1) && rm -rf out && make && ./out/main"))))
EOF
