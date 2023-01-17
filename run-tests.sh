#!/bin/bash
# -*- compile-command: "./run-tests.sh"; -*-
set -e
source config.sh
cd out/batch-1-af/build

EXPECTED_SHA="50bb902c"
CURRENT_SHA="$(git rev-parse --short HEAD)"
if ! [ "$CURRENT_SHA" = "$EXPECTED_SHA" ]; then
    echo "out/batch-1-af is expected to be at sha $EXPECTED_SHA (but at $CURRENT_SHA)"
    exit 1
fi

echo "!!! select skipped"
echo "!!! tile"
./test/tile_oneapi --gtest_filter="Tile/0.*:-Tile/0.Tile111:Tile/0.Tile3D111:Tile/0.Tile2D111"
echo "!!! lookup"
./test/index_oneapi --gtest_filter="*lookup*:-lookup.largeDim:lookup.Issue2009"
echo "!!! meanshift"
./test/meanshift_oneapi --gtest_filter="-*GFOR*"
echo "!!! gradient"
./test/gradient_oneapi --gtest_filter="Grad/0.Grad0:Grad/0.Grad1:Grad/0.Grad2:Grad/2.Grad0:Grad/2.Grad1:Grad/2.Grad2:Grad.CPP"
echo "!!! rotate"
./test/rotate_oneapi --gtest_filter="Rotate/0.*"
echo "!!! resize"
./test/resize_oneapi --gtest_filter="Resize/0*:Resize/2*"
echo "!!! approx1"
./test/approx1_oneapi --gtest_filter="Approx1/0.*"
