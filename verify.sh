#!/bin/bash
# -*- compile-command: "./verify.sh"; -*-
# this script starts work: clone af#batch-1. build. verify all new functions build
set -e

NEW_FUNCTIONS="select tile lookup meanshift gradient"

if [ ! -d out/af ]; then git clone gh:pv-pterab-s/arrayfire out/af; fi
if [ ! -d out/af/build ]; then
    cd $TARGET_DIR/af
    mkdir -p build
    cd build
    CXX=icpx CC=icx cmake .. -DCMAKE_CXX_FLAGS="-w" -DAF_BUILD_UNIFIED=OFF -DAF_BUILD_CPU=ON -DAF_BUILD_CUDA=OFF -DAF_BUILD_OPENCL=OFF -DAF_BUILD_ONEAPI=ON -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu/cmake
    make -j22
fi

if [ ! -d /tmp/af-standalones ]; then git clone gh:pv-pterab-s/af-standalones /tmp/af-standalones; fi
(cd /tmp/af-standalones && git clean -dfx && git pull origin main) &> /dev/null

for I in $NEW_FUNCTIONS; do
    [ -n "$I" ]
    rm -rf out/$I
    (cd out && tar xf /tmp/af-standalones/batch-1/$I.tar)
    rm -f out/$I/$I.hpp
    ln -sf $(readlink -f out/af/src/backend/oneapi/kernel/$I.hpp) $(readlink -f out/$I/$I.hpp)
    echo building $I
    if ! (cd out/$I && make) &> out/build-log-$I.txt; then
        echo FAIL
        cat out/build-log-$I.txt
        exit 1
    fi
done

echo "PASS: $NEW_FUNCTIONS"
