#!/bin/bash
# -*- compile-command: "./run-tests.sh"; -*-
set -e
source config.sh
cd out/batch-1-af/build

EXPECTED_SHA="5dc2ae2c"
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
# echo "!!! meanshift"
# ./test/meanshift_oneapi --gtest_filter="-*GFOR*"
echo "!!! gradient"
./test/gradient_oneapi --gtest_filter="Grad/0.Grad0:Grad/0.Grad1:Grad/0.Grad2:Grad/2.Grad0:Grad/2.Grad1:Grad/2.Grad2:Grad.CPP"
echo "!!! rotate"
./test/rotate_oneapi --gtest_filter="Rotate/0.*"
echo "!!! resize"
./test/resize_oneapi --gtest_filter="Resize/0*:Resize/2*"
echo "!!! approx1"
./test/approx1_oneapi --gtest_filter="Approx1/0.*"
echo "!!! approx2"
./test/approx2_oneapi --gtest_filter="Approx2/0.*"
echo "!!! reorder"
./test/reorder_oneapi --gtest_filter="Reorder/0.Reorder012:Reorder/0.Reorder120:Reorder/0.Reorder201:Reorder/0.Reorder210:Reorder/0.Reorder0123:Reorder/0.Reorder1203:Reorder/0.Reorder1230:Reorder/0.Reorder1302:Reorder/0.Reorder1320:Reorder/0.Reorder2103:Reorder/0.Reorder2130:Reorder/0.Reorder2013:Reorder/0.Reorder2031:Reorder/0.Reorder2310:Reorder/0.Reorder2301:Reorder/0.Reorder3120:Reorder/0.Reorder3102:Reorder/0.Reorder3210:Reorder/0.Reorder3201:Reorder/0.Reorder3012:Reorder/0.Reorder3021"
echo "!!! transform"
./test/transform_oneapi --gtest_filter="Transform/0.*"
