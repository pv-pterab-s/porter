#!/bin/bash
# -*- compile-command: "./run-tests.sh"; -*-
set -e

# for resident functions..

# run tests on master branch
if [ ! -d out/master-af ]; then git clone gh:pv-pterab-s/arrayfire out/master-af; fi
(cd out/master-af && git checkout master)
(
    cd out/master-af
    mkdir -p build
    cd build
    CXX=icpx CC=icx cmake .. -DCMAKE_CXX_FLAGS="-w" -DAF_BUILD_UNIFIED=OFF -DAF_BUILD_CPU=ON -DAF_BUILD_CUDA=OFF -DAF_BUILD_OPENCL=OFF -DAF_BUILD_ONEAPI=ON -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu/cmake
    make -j22
)

FUNCTIONS="select" # tile lookup meanshift gradient"
for I in $FUNCTIONS; do
    cd out/master-af/build
    ./test/test_select_on --gtest_list_tests
done

# run tests on batch-1 branch
