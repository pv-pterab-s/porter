#!/bin/bash
# -*- compile-command: "./run-tests.sh"; -*-
set -e
source config.sh
cd out/batch-1-af/build

EXPECTED_SHA="ff37d59d"
if ! [ "$(git rev-parse --short HEAD)" = "$EXPECTED_SHA" ]; then
    echo "out/batch-1-af is expected to be at sha $EXPECTED_SHA"
    exit 1
fi

./test/tile_oneapi --gtest_filter="Tile/0.*:-Tile/0.Tile111:Tile/0.Tile3D111:Tile/0.Tile2D111"

for I in $NEW_FUNCTIONS; do
    if [ $I = lookup ]; then continue; fi  # missing tests?
    if [ $I = tile ]; then continue; fi  # already done
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!! $I"
    ./test/${I}_oneapi --gtest_filter="${I^}/0.*" || :
done
