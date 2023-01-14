#!/bin/bash
# -*- compile-command: "./setup.sh"; -*-
# puts work environment at clean state. errs before erasing
set -e
source config.sh

function setup_af_branch {
    local BRANCH="$1"
    if [ -d out/$BRANCH-af ]; then echo "out/$BRANCH-af exists!"; exit 1; fi

    mkdir -p out
    git clone gh:pv-pterab-s/arrayfire out/$BRANCH-af
    (cd out/$BRANCH-af && git fetch --all && git checkout $BRANCH)
    PATCH_FILE="$(readlink -f patch-device_manager.diff)"
    (cd out/$BRANCH-af/src/backend/oneapi && patch device_manager.cpp < $PATCH_FILE)

    if [ ! -d out/$BRANCH-af/build ]; then
       (
           cd out/$BRANCH-af
           mkdir -p build
           cd build
           CXX=icpx CC=icx cmake .. -DCMAKE_CXX_FLAGS="-w" -DAF_BUILD_UNIFIED=OFF -DAF_BUILD_CPU=ON -DAF_BUILD_CUDA=OFF -DAF_BUILD_OPENCL=OFF -DAF_BUILD_ONEAPI=ON -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu/cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
           make -j22
       )
    fi
}

function setup_standalone {
    local FUNCTION_NAME="$1"
    if [ ! -d /tmp/af-standalones ]; then git clone gh:pv-pterab-s/af-standalones /tmp/af-standalones; fi
    (cd /tmp/af-standalones && git clean -dfx && git pull origin main) &> /dev/null

    rm -rf out/$FUNCTION_NAME
    (cd out && tar xf /tmp/af-standalones/batch-1/$FUNCTION_NAME.tar)
    rm -f out/$FUNCTION_NAME/$FUNCTION_NAME.hpp
    ln -sf $(readlink -f out/batch-1-af/src/backend/oneapi/kernel/$FUNCTION_NAME.hpp) $(readlink -f out/$FUNCTION_NAME/$FUNCTION_NAME.hpp)
    echo building $FUNCTION_NAME
    if ! (cd out/$FUNCTION_NAME && make) &> out/build-log-$FUNCTION_NAME.txt; then
        echo FAIL
        cat out/build-log-$FUNCTION_NAME.txt
        exit 1
    fi
}

function setup_env_for_function {
    local FUNCTION_NAME="$1"
    if [[ " $NEW_FUNCTIONS " =~ " $FUNCTION_NAME " ]]; then echo "function already ported!"; exit 1; fi
    if [ ! -d "out/$FUNCTION_NAME" ]; then git clone gh:pv-pterab-s/new-dev out/$FUNCTION_NAME; fi

    rm -f out/$FUNCTION_NAME/lookup.hpp
    ln -sf $(readlink -f out/batch-1-af/src/backend/oneapi/kernel)/$FUNCTION_NAME.hpp $(readlink -f out/$FUNCTION_NAME)/$FUNCTION_NAME.hpp

    cat > out/batch-1-af/.dir-locals.el <<EOF
((c++-mode . ((compile-command . "cd $(readlink -f out/$FUNCTION_NAME) && rm -rf out && make && ./out/main"))))
EOF
}



function main {
    # if given option, setup env for function
    if [ -n "$1" ]; then setup_env_for_function "$1"; exit; fi

    # both needed arrayfires
    setup_af_branch master
    setup_af_branch batch-1

    # all standalones (linked over)
    for I in $NEW_FUNCTIONS; do setup_standalone $I; done
}
main $*
